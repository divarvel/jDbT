{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString    as BS
import           Data.Yaml.Parser   (readYamlFile)
import           System.Environment (getArgs)

import           JDBT.Parser
import           JDBT.Dot
import           JDBT.SQL

main :: IO ()
main = do
    args <- getArgs
    yml <- readYamlFile $ head args
    case extractTypes yml of
        Left err -> putStrLn $ "error: " ++ err
        Right ts -> BS.putStr $ case args of
            [_, "dot"] -> dataToDot ts
            _ -> dataToSQL ts

