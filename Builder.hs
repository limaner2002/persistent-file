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
import Text.Read (readEither)
import Data.Monoid (mconcat)

class Buildable a where
    build :: [T.Text] -> Either T.Text a

readEither' :: Read a => T.Text -> Either T.Text a
readEither' t =
    case readEither (T.unpack t) of
      Left msg -> Left $ T.concat [(T.pack $ msg ++ ": "), t]
      Right d -> Right d

mkReadEither :: String -> [Attr] -> Name -> Q Exp
mkReadEither tp attrs name
    | tp == "String" = inspectStr attrs
    | otherwise = inspect attrs
    where
      inspectStr attrs
          | "Maybe" `elem` attrs = [| Right (Just (T.unpack $(varE name))) |]
          | otherwise = [| Right (T.unpack $(varE name))|]
      inspect attrs
          | "Maybe" `elem` attrs = [| readEither' $(varE name) :: Either T.Text (Maybe $(conT (mkName tp))) |]
          | otherwise = [| readEither' $(varE name) :: Either T.Text $(conT (mkName tp)) |]

getTypes :: EntityDef -> [T.Text]
getTypes entity = do
  let fields = entityFields entity
  let tpName (FTTypeCon _ tName) = tName
  map (tpName . fieldType) fields

mkReads :: EntityDef -> [Name] -> Q [Exp]
mkReads entity names = do
  mapM (\(tp, attrs, name) ->
                      mkReadEither (T.unpack tp) attrs name) $ zip3 types fAttrs names
  where    
    types = map (tpName . fieldType) fields
    fields = entityFields entity
    tpName (FTTypeCon _ tName) = tName
    fAttrs = map fieldAttrs fields

mkBuilder :: [EntityDef] -> Q [Dec]
mkBuilder entities = fmap mconcat $ mapM mkBuild entities

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
