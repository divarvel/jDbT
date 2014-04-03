{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO       as TIO
import           Data.Yaml.Parser   (readYamlFile)
import           System.Environment (getArgs)

import           JDBT.Dot
import           JDBT.Parser
import           JDBT.SQL

main :: IO ()
main = do
    args <- getArgs
    yml <- readYamlFile $ head args
    case extractTypes yml of
        Left err -> putStrLn $ "error: " ++ err
        Right ts -> TIO.putStr $ case args of
            [_, "dot"] -> dataToDot ts
            _ -> dataToSQL ts

