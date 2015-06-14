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

mkPersist sqlSettings [persistLowerCase|
Dept
  deptno Int
  dname String
  loc String
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

main = do
  (path:args) <- getArgs
  records <- parseFile path "EMP" empFields :: IO [Either Text Emp]
  -- let cursor = fromDocument doc
  -- let cursors =
  --       cursor $/ element "EMP"
  -- let records = map (\x -> parseRecord x empFields :: Either Text Emp) cursors
  mapM_ printRecord records