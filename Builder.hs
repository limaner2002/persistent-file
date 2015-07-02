{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Builder where

import Language.Haskell.TH
import Database.Persist.TH
import Database.Persist
import Control.Applicative
import Data.Text (unpack, Text)
import Text.Read (readEither)

class Buildable a where
    build :: [String] -> Either String a

mkReadEither :: String -> Name -> Q Exp
mkReadEither tp name
    | tp == "String" = [| Right $(varE name)|]
    | otherwise = [| readEither $(varE name) :: Either String $(conT (mkName tp)) |]

getTypes :: EntityDef -> [Text]
getTypes entity = do
  let fields = entityFields entity
  let tpName (FTTypeCon _ tName) = tName
  map (tpName . fieldType) fields

mkReads :: EntityDef -> [Name] -> Q [Exp]
mkReads entity names = do
  let tps = getTypes entity
  sequence $ map (\(tp, name) ->
                      mkReadEither (unpack tp) name) $ zip tps names

mkBuilder :: EntityDef -> Q [Dec]
mkBuilder entDef = do
  app1E <- [|(<$>)|]
  applyE <- [|(<*>)|]
  entNameE <- conE entName
  names <- sequence $ replicate (length fields) $ newName "x"
  (exp1:exps) <- mkReads entDef names

  let pat = listP $ map varP names
  let fun = return $
            foldl (\x y -> UInfixE x applyE y) (UInfixE entNameE app1E (exp1)) $ exps

  [d| instance Buildable $(conT entName) where
        build $(pat) = $(fun)|]
  where
    entName = (mkName . unpack . unHaskellName . entityHaskell) entDef
    fields = entityFields entDef
