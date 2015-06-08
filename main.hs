{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

import Prelude hiding (readFile)
import XMLParser
import Database.Persist.TH
import System.Environment (getArgs)
import Data.Text (Text,unpack)
import Text.XML
import Database.Persist

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
  mgr Int
  hireDate String
  sal Double
  deptNo Int
  deriving Show
|]

createRecords :: PersistEntity a => [Text -> PersistValue] -> [[Text]] -> [Either Text a]
createRecords xs ys = map (createRecord xs) ys

main = do
  (path:args) <- getArgs
  Document prologue root epilogue <- readFile def path
  let recordTexts = map (parseXML 2) $ elementNodes root
      fieldFuncs = [ PersistInt64 . read . unpack
                   , PersistText
                   , PersistText
                   , PersistInt64 . read . unpack
                   , PersistText
                   , PersistDouble . read . unpack
                   , PersistInt64 . read . unpack
                   ]
  -- mapM_ (printRecord) (createRecords [PersistInt64 . read . unpack, PersistText, PersistText] recordTexts :: [Either Text Dept])
  mapM_ (printRecord) (createRecords fieldFuncs recordTexts :: [Either Text Emp])
