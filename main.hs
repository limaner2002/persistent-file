{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

import System.Environment
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.CSV.Conduit
import Data.Text (Text)
import Control.Monad.Trans.Resource (runResourceT)
import Builder
import Model

import qualified Data.Conduit.Combinators as CM
import Control.Monad.IO.Class (liftIO)
import Database.Persist.TH
import Database.Persist
import Database.Persist.MySQL
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

share [mkPersist sqlSettings, mkMigrate "migrateAll"] qq
mkBuilder qq

-- insertRecord :: (PersistEntity a) => Either Text a -> Int
insertRecord (Left msg) = do
  (liftIO . putStrLn . show) msg
insertRecord (Right record) = do
  key <- insertEntity record
  return ()

createRecords :: Monad m => Conduit (Row Text) m (Row (Either Text Mortgage))
createRecords = CM.map (\row -> return $ build row)

--insertRecords :: (MonadResource m, Monad m) => Conduit (Row (Either Text Payment)) m (Row Int)
insertRecords = CM.mapM_ (\record ->
                             mapM_ insertRecord record
                            -- errors <- mapM insertRecord records
                            -- liftIO $ putStrLn $ "Inserted with " ++ show (sum errors) ++ " errors."
                         )

reportErrors :: (MonadIO m, Monad m) => Consumer (Row Int) m ()
reportErrors = accumulate 0
    where
      accumulate nError = do
          n <- await
          case n of
            Nothing -> liftIO $ putStrLn $ "Inserted with " ++ show nError ++ " errors."
            Just err -> accumulate (nError + (sum err))

doIt path = do
  runMigration migrateAll
  CB.sourceFile path $= intoCSV (defCSVSettings {csvSep=','}) $= createRecords $$ insertRecords -- $$ reportErrors

main = do
  (path:args) <- getArgs
  let conn = defaultConnectInfo {connectUser = "josh", connectDatabase = "eIntern"}
  let f = (runResourceT . runNoLoggingT . (withMySQLConn conn) . runSqlConn)
  f $ doIt path
