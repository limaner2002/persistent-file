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
  premiseId Text
  propertyName Text
  propertyStreet Text
  propertyCity Text
  propertyState Text
  propertyZip Int
  units Int
  initialEndorsementDate Text
  finalEndorsementDate Text
  originalMortgageAmount Text
  firstPaymentDate Text
  maturityDate Text
  termInMonths Text
  interestRate Int
  holderName Double
  holderCity Text
  holderState Text
  servicerName Text
  servicerCity Text
  servicerState Text
  sectionOfActCode Text
  soaCategory/SubCategory Text
  term_type Text
  terminationTypeDescription Int
  type Text
  term_date Text
  te Text
  tc Text
  status Text
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
mortgageFields = [PersistInt64 . read . unpack,
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

main = do
  (path:args) <- getArgs
  records <- parseFile path "EMP" empFields :: IO [Either Text Emp]
  let conn = defaultConnectInfo {connectUser = "josh", connectDatabase = "eIntern"}
  runResourceT $ runNoLoggingT $ withMySQLConn conn $ runSqlConn $ do
         runMigration migrateAll
         mapM_ (\record -> insertRecord record) records
  -- mapM_ printRecord records