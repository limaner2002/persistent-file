{-# LANGUAGE OverloadedStrings #-}
module FlatFileParser where
import System.Environment
import Data.List.Split
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CM
import qualified Data.ByteString.Char8 as C8
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Text.Read
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T

type Delim = T.Text
data PotentialType = MaybeString T.Text | MaybeDouble T.Text | MaybeInt T.Text
                   deriving Read
data DiscoveredType =
    DiscoveredInt Int |
    DiscoveredDouble Double |
    DiscoveredString T.Text

instance Show DiscoveredType where
    show (DiscoveredInt _) = "Int"
    show (DiscoveredDouble _) = "Double"
    show (DiscoveredString _) = "String"

tReadMaybe :: Read a => T.Text -> Maybe a
tReadMaybe = readMaybe . T.unpack

splitLine :: Delim -> T.Text -> [T.Text]
splitLine delim colStr = T.splitOn delim colStr

readType :: PotentialType -> (DiscoveredType, String)
readType (MaybeInt str) = case tReadMaybe str of
                           Just result -> (DiscoveredInt result,
                                           "PersistInt64 . read . unpack")
                           Nothing -> readType (MaybeDouble str)
readType (MaybeDouble str) = case tReadMaybe str of
                              Just result -> (DiscoveredDouble result,
                                              "PersistDouble . read . unpack")
                              Nothing -> readType (MaybeString str)
readType (MaybeString str) = (DiscoveredString str, "PersistText")

toCamelCase :: T.Text -> T.Text
toCamelCase txt = T.concat $ firstWord ++ map T.toTitle rest
    where
      firstWord = take 1 lowers
      rest = drop 1 lowers
      lowers = map T.toLower ws
      ws = T.words txt

getType :: T.Text -> (DiscoveredType, String)
getType str = readType (MaybeInt str)

parseHeader :: (MonadResource m) => ConduitM C8.ByteString C8.ByteString m ()
parseHeader = CM.take 1

getLines :: (MonadResource m) => ConduitM C8.ByteString [[T.Text]] m ()
getLines = CM.map (\chunk -> do
                     let lines = splitLine "\n" $ decodeUtf8 chunk
                     map (splitLine ",") lines
                  )

getTypes :: (MonadResource m, MonadIO m) => Consumer [[T.Text]] m ()
getTypes = CM.mapM_ (\lines -> do
                        let header = concatMap (map toCamelCase) $ take 1 lines
                        let firstLine = take 1 $ drop 1 lines
                        let typeTuples = concatMap (\xs -> map getType xs) firstLine
                        let types = map fst typeTuples
                        let typeFuncs = map snd typeTuples
                        let attrs = zipWith (\x y -> "  " ++ T.unpack x ++ " " ++ show y ++ " Maybe") header types
                        liftIO $ mapM_ putStrLn attrs
                        liftIO $ mapM_ (\fcn -> putStrLn $ fcn ++ ",") typeFuncs
                     )

-- parseFlatFile :: PersistEntity a => FilePath -> [Text -> PersistValue] -> IO [Either Text a]
-- parseFlatFile path fields = do
--   runResourceT $ CB.sourceFile path $$ parseHeader $=

-- main = do
--   (path:args) <- getArgs
--   putStrLn "Compiled"
--   runResourceT $ CB.sourceFile path $$ parseHeader $= getLines $= getTypes

