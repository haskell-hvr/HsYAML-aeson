module Main where

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.YAML.Aeson as Yaml
import qualified Data.Aeson as Aeson
import System.Exit (exitFailure)

main :: IO ()
main = do
    contents <- Lazy.getContents
    case Yaml.decodeValue contents of
        Left e -> do
            print e
            exitFailure
        Right x -> (Lazy.putStr . Aeson.encode) x
