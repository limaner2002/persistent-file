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
import Text.Read (readEither)

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
  holderName Double Maybe
  holderCity Text Maybe
  holderState Text Maybe
  servicerName Text Maybe
  servicerCity Text Maybe
  servicerState Text Maybe
  sectionOfActCode Text Maybe
  soaCategory/SubCategory Text Maybe
  term_type Text Maybe
  terminationTypeDescription Int Maybe
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
mortgageFields = [PersistInt64 . readEither,
                  PersistText,
                  PersistText,
                  PersistText,
                  PersistText,
                  PersistText,
                  PersistInt64 . read . unpack,
                  PersistInt64 . read . unpack,
                  PersistText,
                  PersistText,
                  PersistText,
                  PersistText,
                  PersistText,
                  PersistText,
                  PersistInt64 . read . unpack,
                  PersistDouble . read . unpack,
                  PersistText,
                  PersistText,
                  PersistText,
                  PersistText,
                  PersistText,
                  PersistText,
                  PersistText,
                  PersistText,
                  PersistInt64 . read . unpack,
                  PersistText,
                  PersistText,
                  PersistText,
                  PersistText,
                  PersistText,
                  PersistText]

-- insertRecord :: (Show a, PersistEntity a) => Either Text a -> m a
insertRecord (Left msg) = liftIO $ print msg
insertRecord (Right record) = insertEntity record >> return ()

fieldValues :: PersistEntity a => [Text] -> [Text -> PersistValue] -> Either Text a
fieldValues fieldNames fieldFuncs = fromPersistValues fieldValues
    where
      fieldValues = zipWith id fieldFuncs fieldNames

getRecords :: (MonadResource m) => ConduitM [[T.Text]] [Either Text Mortgage] m ()
getRecords = CM.map (\lines -> map (\x -> fieldValues x mortgageFields) lines)

consumeRecords :: (MonadResource m, MonadIO m, PersistEntity a, Show a) => Consumer [Either Text a] m ()
consumeRecords = CM.mapM_ (\records -> mapM_ (liftIO . printRecord) records)

printRecord :: (PersistEntity a, Show a) => Either Text a -> IO ()
printRecord (Left msg) = print $ unpack msg
printRecord (Right record) = print record

printLines :: (MonadResource m, MonadIO m) => Consumer [[T.Text]] m ()
printLines = CM.mapM_ (\lines -> mapM_ (liftIO . print) lines)

main = do
  -- (path:args) <- getArgs
  -- records <- parseFile path "EMP" empFields :: IO [Either Text Emp]
  -- let conn = defaultConnectInfo {connectUser = "josh", connectDatabase = "eIntern"}
  -- runResourceT $ runNoLoggingT $ withMySQLConn conn $ runSqlConn $ do
  --        runMigration migrateAll
  --        mapM_ (\record -> insertRecord record) records
  (path:args) <- getArgs
  putStrLn path
  runResourceT $ CB.sourceFile path $$ parseHeader $= getLines $= getRecords $= consumeRecords