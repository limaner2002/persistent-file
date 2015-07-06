{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Builder (
                mkBuilder,
                build,
                Buildable
               )where
-- module Builder where

import Language.Haskell.TH
import Database.Persist.TH
import Database.Persist
import Control.Applicative
import qualified Data.Text as T --(unpack, pack, Text, concat)
--import Text.Read (readEither)
import Data.Text.Read
import Data.Monoid (mconcat)
import Parseable

class Buildable a where
    build :: [T.Text] -> Either T.Text a

mkReadEither :: String -> [Attr] -> FieldName -> Name -> Q Exp
mkReadEither tp attrs fieldName varName
    | "Maybe" `elem` attrs = [| reportFieldName fieldName tp (tRead $(varE varName) :: Either T.Text (Maybe $(conT (mkName tp)))) |]
    | otherwise = [| reportFieldName fieldName tp (tRead $(varE varName) :: Either T.Text $(conT (mkName tp))) |]

reportFieldName fieldName tp result =
    case result of
      Left msg -> Left $ T.concat [msg, " for field '", fieldName, "'",
                                   " with type ", tp]
      Right v -> Right v

getTypes :: EntityDef -> [T.Text]
getTypes entity = do
  let fields = entityFields entity
  let tpName (FTTypeCon _ tName) = tName
  map (tpName . fieldType) fields

mkReads :: EntityDef -> [Name] -> Q [Exp]
mkReads entity varNames = do
  mapM (\(field, varName) ->
            mkReadEither (fType field) (fAttr field) (fieldName field) varName) $ zip fields varNames
  where
    fType = T.unpack . tpName . fieldType
    fields = entityFields entity
    tpName (FTTypeCon _ tName) = tName
    fAttr = fieldAttrs
    fieldName = unHaskellName . fieldHaskell

mkBuilder :: [EntityDef] -> Q [Dec]
mkBuilder entities = fmap mconcat $ mapM mkBuild entities

-- The field reporting should be here
mkBuild :: EntityDef -> Q [Dec]
mkBuild entDef = do
  app1E <- [|(<$>)|]
  applyE <- [|(<*>)|]
  entNameE <- conE entName
  names <- sequence $ replicate (length fields) $ newName "x"
  (exp1:exps) <- mkReads entDef names

  let pat = listP $ map varP names
  let fun = return $
            foldl (\x y -> UInfixE x applyE y) (UInfixE entNameE app1E (exp1)) $ exps

  [d| instance Buildable $(conT entName) where
        build $(pat) = $(fun)
        build [] = Left (T.pack "Empty list")
        build x = Left (T.pack $ "Failed on:\n" ++ show x)
    |]
  where
    entName = (mkName . T.unpack . unHaskellName . entityHaskell) entDef
    fields = entityFields entDef
