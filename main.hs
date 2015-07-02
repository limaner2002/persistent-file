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

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CM
import qualified Data.Text as T
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT)
import Text.Read (readMaybe)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Dept
  deptno Int
  dname String
  loc String
  Primary deptno
  deriving Show
Emp
  empno Int
  ename String
  job String
  mgr Int Maybe
  hireDate String
  sal Double
  comm Double Maybe
  deptNo Int
  Primary empno
  deriving Show
Mortgage
  hudProjectNumber Int
  premiseId Text Maybe
  propertyName Text Maybe
  propertyStreet Text Maybe
  propertyCity Text Maybe
  propertyState Text Maybe
  propertyZip Int Maybe
  units Int Maybe
  initialEndorsementDate Text Maybe
  finalEndorsementDate Text Maybe
  originalMortgageAmount Text Maybe
  firstPaymentDate Text Maybe
  maturityDate Text Maybe
  termInMonths Text Maybe
  interestRate Int Maybe
  holderName Text Maybe
  holderCity Text Maybe
  holderState Text Maybe
  servicerName Text Maybe
  servicerCity Text Maybe
  servicerState Text Maybe
  sectionOfActCode Text Maybe
  soaCategory/SubCategory Text Maybe
  term_type Text Maybe
  terminationTypeDescription Text Maybe
  type Text Maybe
  term_date Text Maybe
  te Text Maybe
  tc Text Maybe
  status Text Maybe
  Primary hudProjectNumber
  deriving Show
|]

deptFields = [ ("DEPTNO", PersistInt64 . read . unpack)
             , ("DNAME", PersistText)
             , ("LOC", PersistText)
             ]

empFields = [ ("EMPNO", PersistInt64 . read . unpack)
             , ("ENAME", PersistText)
             , ("JOB", PersistText)
             , ("MGR", verifyField (PersistInt64 . read . unpack))
             , ("HIREDATE", PersistText)
             , ("SAL", PersistDouble . read . unpack)
             , ("COMM", verifyField (PersistDouble . read . unpack) )
             , ("DEPTNO", PersistInt64 . read . unpack)
             ]

-- insertRecord :: (MonadIO m,
--                  Show b, Show a,
--                  PersistStore backend,
--                  backend ~ PersistEntityBackend a,
--                  PersistEntity a) => Either b a -> ReaderT backend m ()
insertRecord (Left msg) = liftIO $ putStrLn $ "Error: " ++ show msg
insertRecord (Right record) = do
  key <- insertEntity record
  liftIO $ putStrLn $ "Inserted record with key " ++ show key

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

getRecord :: PersistEntity a => [Text] -> Either Text a
getRecord = fromPersistValues . (map (getPersistValue . MaybeInt))

getRecords :: (MonadResource m) => ConduitM [[T.Text]] [Either Text Mortgage] m ()
getRecords = CM.map (\lines -> map getRecord lines)

-- consumeRecords :: (MonadResource m,
--                    MonadIO m,
--                    PersistEntity a
--                   ) => Consumer [Either Text a] m ()
consumeRecords = CM.mapM_ (\records -> mapM_ (liftIO . printRecord) records)
-- consumeRecords = CM.mapM_ (\records -> liftIO $ runResourceT $ runNoLoggingT $ withMySQLConn conn $ runSqlConn $ do
--                                          runMigration migrateAll
--                                          -- let f = insertRecord
--                                          -- _
--                                          mapM_ insertRecord records
--                           )

printRecord :: (PersistEntity a, Show a) => Either Text a -> IO ()
printRecord (Left msg) = putStrLn $ unpack msg
printRecord (Right record) = print record

printLines :: (MonadResource m, MonadIO m) => Consumer [[T.Text]] m ()
printLines = CM.mapM_ (\lines -> mapM_ (liftIO . print) lines)

conn = defaultConnectInfo {connectUser = "josh", connectDatabase = "eIntern"}

main = do
  -- (path:args) <- getArgs
  -- records <- parseFile path "XROOT/STG_MORTGAGE" mortgageFields :: IO [Either Text Mortgage]
  -- let conn = defaultConnectInfo {connectUser = "josh", connectDatabase = "eIntern"}
  -- runResourceT $ runNoLoggingT $ withMySQLConn conn $ runSqlConn $ do
  --        runMigration migrateAll
  --        mapM_ (\record -> insertRecord record) records
  (path:args) <- getArgs
  runResourceT $ CB.sourceFile path $$ getLines $= getRecords $= consumeRecords