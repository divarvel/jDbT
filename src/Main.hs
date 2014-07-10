{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO       as TIO
import           Data.Yaml
import           System.Environment (getArgs)

import           JDBT.Dot
import           JDBT.Parser        ()
import           JDBT.SQL

main :: IO ()
main = do
    args <- getArgs
    schema <- decodeFileEither $ head args
    case schema of
        Left err -> putStrLn $ "error: " ++ show err
        Right ts -> TIO.putStr $ case args of
            [_, "dot"] -> dataToDot ts
            _ -> dataToSQL ts

