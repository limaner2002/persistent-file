{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module XMLParser where

import Data.Text (Text,unpack)
import Text.XML
import Prelude hiding (readFile)
import Database.Persist

printRecord :: (PersistEntity a, Show a) => Either Text a -> IO ()
printRecord (Left msg) = print $ unpack msg
printRecord (Right record) = print record

getContent :: Node -> Text
getContent (NodeContent t) = t

getChildren :: Node -> [Node]
getChildren (NodeElement e) = elementNodes e

createRecord :: PersistEntity a => [Text -> PersistValue] -> [Text] -> Either Text a
createRecord xs ys = fromPersistValues $ zipWith id xs ys

parseXML :: Int -> Node -> [Text]
parseXML 0 _ = []
parseXML 1 node = map getContent $ getChildren node
parseXML n node = concatMap (parseXML (n-1)) $ getChildren node