{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

import Data.Text (Text, unpack, concat, pack, length)
import Prelude hiding (readFile, concat, length)
import Database.Persist.TH
import System.Environment (getArgs)
import Database.Persist
import Database.Persist.MySQL
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runNoLoggingT)
import XMLParser
import FlatFileParser
import Builder
import Model

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CM
import qualified Data.Text as T
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT)
import Text.Read (readMaybe)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] qq
mkBuilder qq

insertRecord (Left msg) = liftIO $ putStrLn $ "Error: " ++ show msg
insertRecord (Right record) = do
  key <- insertEntity record
  return ()

fieldValues :: PersistEntity a => [Text] -> [Text -> PersistValue] -> Either Text a
fieldValues fieldNames fieldFuncs = fromPersistValues fieldValues
    where
      fieldValues = zipWith id fieldFuncs fieldNames

getPersistValue :: PotentialType -> PersistValue
getPersistValue (MaybeInt str) = case tReadMaybe str :: Maybe Int of
                                   Just result -> toPersistValue result
                                   Nothing -> getPersistValue (MaybeDouble str)
getPersistValue (MaybeDouble str) = case tReadMaybe str :: Maybe Double of
                                      Just result -> toPersistValue result
                                      Nothing -> getPersistValue (MaybeString str)
getPersistValue (MaybeString "") = PersistNull
getPersistValue (MaybeString str) = toPersistValue str

getRecord :: (PersistEntity a, Buildable a) => [Text] -> Either Text a
getRecord fieldValues = build fieldValues

getRecords :: (MonadResource m) => ConduitM [[T.Text]] [Either Text Payment] m ()
getRecords = CM.map (\lines -> map getRecord lines)

-- consumeRecords :: (MonadResource m,
--                    MonadIO m,
--                    PersistEntity a
--                   ) => Consumer [Either Text a] m ()
consumeRecords = CM.mapM_ (\records -> do
                            runMigration migrateAll
                            -- let f = insertRecord
                            -- _
                            mapM_ insertRecord records
                          )

printRecord :: (PersistEntity a, Show a) => Either Text a -> IO ()
printRecord (Left msg) = putStrLn $ unpack msg
printRecord (Right record) = return () --print record

printLines :: (MonadResource m, MonadIO m) => Consumer [[T.Text]] m ()
printLines = CM.mapM_ (\lines -> mapM_ (liftIO . print) lines)

conn = defaultConnectInfo {connectUser = "josh", connectDatabase = "eIntern"}

main = do
  (path:args) <- getArgs
  runResourceT $ runNoLoggingT $ withMySQLConn conn $ runSqlConn $ CB.sourceFile path $$ getLines $= getRecords $= consumeRecords
