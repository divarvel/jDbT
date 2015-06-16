{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Control.Applicative
-- import           Data.Maybe
import qualified Data.Text.Lazy          as T
-- import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.IO       as TL
import qualified Data.ByteString.Lazy as BL
import           Data.Yaml
import           System.Environment

import           Web.Scotty

import           JDBT.Parser        ()
import           JDBT.SQL

import           Paths_jdbt

-- yamlToSQL :: T.Text -> T.Text
-- yamlToSQL input = either T.pack id sql
--   where
--     schema = decodeEither . T.encodeUtf8 $ input
--     sql = dataToSQL <$> schema

main :: IO ()
main = do
  port <- maybe 8080 read <$> lookupEnv "PORT"
  home <- TL.readFile =<< getDataFileName "data/index.html"
  scotty port $ do
    get "/" $ html home
    post "/" $ do
      schema <- decodeEither . BL.toStrict <$> param "schema"
      text $ either T.pack (T.fromStrict . dataToSQL) schema

