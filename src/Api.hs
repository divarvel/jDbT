{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import           Data.Yaml
import QuickWebApp

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           JDBT.Parser        ()
import           JDBT.SQL

yamlToSQL :: T.Text -> T.Text
yamlToSQL input = either T.pack id sql
  where
    schema = decodeEither . T.encodeUtf8 $ input
    sql = dataToSQL <$> schema

main :: IO ()
main = interactWeb yamlToSQL
