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
import XMLParser
import Database.Persist.MySQL
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runNoLoggingT)

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
Defect
  nhtsaId Int
  make String
  model String
  year String
  compname String
  mfrName String
  odate String
  cdate String
  campno String
  subject String
  summary String
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

-- insertRecord :: (Show a, PersistEntity a) => Either Text a -> m a
insertRecord (Left msg) = liftIO $ print msg
insertRecord (Right record) = insertEntity record >> return ()

main = do
  (path:args) <- getArgs
  records <- parseFile path "EMP" empFields :: IO [Either Text Emp]
  let conn = defaultConnectInfo {connectUser = "josh", connectDatabase = "eIntern"}
  runResourceT $ runNoLoggingT $ withMySQLConn conn $ runSqlConn $ do
         runMigration migrateAll
         mapM_ (\record -> insertRecord record) records
  -- mapM_ printRecord records