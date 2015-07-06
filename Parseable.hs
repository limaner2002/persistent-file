{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Parseable (readEither',
                  Parseable (..),
                  TParse (..),
                  FieldText,
                  FieldName
                 )
    where

import System.Environment
import Data.Text.Read
import qualified Data.Text as T

data TParse a = TParse { tParser :: (T.Text -> Either String (a, T.Text)) }

type FieldText = T.Text
type FieldName = T.Text

class Parseable a where
    tRead :: Parseable a => T.Text -> Either T.Text a
    tParse :: Parseable a => TParse a

readEither' :: TParse a -> FieldText -> Either T.Text a
readEither' (TParse parser) txt =
    case parser txt of
      Left _ -> Left errorMessage
      Right (value, remaining) ->
          if remaining == T.empty
          then Right value
          else Left $ errorMessage
    where
      errorMessage = T.concat ["Error parsing ", txt]

instance Parseable Double where
    tRead x = readEither' tParse x
    tParse = TParse double

instance Parseable Int where
    tRead x = readEither' tParse x
    tParse = TParse decimal

instance Parseable a => Parseable (Maybe a) where
    tRead x
        | x == T.empty = Right Nothing
        | otherwise = do
      case (tRead x :: Parseable a => Either T.Text a) of
        Left msg -> Left msg
        Right v -> Right $ Just v
    tParse = undefined

instance Parseable T.Text where
    tRead x = Right x
    tParse = undefined

instance Parseable String where
    tRead x = Right $ T.unpack x
    tParse = undefined