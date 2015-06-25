{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module XMLParser where

import Data.Text (Text, unpack, concat, pack, length)
import Prelude hiding (readFile, concat, length)

import Text.XML
import Text.XML.Cursor
import Database.Persist

verifyField :: (Text -> PersistValue) -> Text -> PersistValue
verifyField f txt
    | length txt > 0 = f txt
    | otherwise = PersistNull

parseRecord :: PersistEntity a => Cursor -> [(Name, Text -> PersistValue)] -> Either Text a
parseRecord cursor fields = do
  let fieldTexts = concatMap (\field -> checkField $ fieldText field) fields
          where
            fieldText field = cursor $/ element (fst field) &/ content
            checkField [] = [""]
            checkField fld = fld
  let fieldFuncs = map snd fields
  let fieldValues = zipWith id fieldFuncs fieldTexts -- $ trace (show fieldTexts) (fieldTexts)
  fromPersistValues fieldValues

parseFile :: PersistEntity a => FilePath -> Name -> [(Name, Text -> PersistValue)] -> IO [Either Text a]
parseFile path elementName fields = do
  doc <- readFile def path
  let cursor = fromDocument doc
  let cursors = cursor $/ element elementName
  return $ map (\x -> parseRecord x fields) cursors