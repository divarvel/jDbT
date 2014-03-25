{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Text.Libyaml (Tag, Tag(..))
import Data.Yaml.Parser (YamlValue, YamlValue(..), readYamlFile)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import Data.Foldable


data Type = Tb Table | En DbEnum

data DbEnum = DbEnum T.Text [BS.ByteString]

data Table = Table T.Text [Field] [Constraint] deriving (Show)

data Field = Field T.Text BS.ByteString Bool (Maybe BS.ByteString) deriving (Show)

data Constraint = Constraint (Maybe T.Text) deriving (Show)

extractTypes :: YamlValue -> Either String [Type]
extractTypes (Mapping vs _) = let
    ts = (fmap (uncurry extractType) vs :: [Either String Type])
    tts = sequence ts
    in tts
extractTypes _ = Left "invalid top value"


extractType :: T.Text -> YamlValue -> Either String Type
extractType name (Mapping vs _) = fmap Tb $ extractTable name vs
extractType name (Sequence vs _) = fmap En $ extractEnum name vs
extractType _ _ = Left "invalid type value"

extractEnum :: T.Text -> [YamlValue] -> Either String DbEnum
extractEnum name vs = let
    vvs = mapM extractEnumValue vs
    in fmap (DbEnum name) vvs
    where
        extractEnumValue (Scalar t _ _ _) = Right t
        extractEnumValue _ = Left "invalid enum value"

extractTable :: T.Text -> [(T.Text, YamlValue)] -> Either String Table
extractTable name vs = let
    fs = fmap (uncurry extractField) vs
    ffs = sequence fs
    in fmap (\fields -> Table name fields []) ffs

extractField :: T.Text -> YamlValue -> Either String Field
extractField name (Scalar bs t _ _) = extractSimpleField name bs t
extractField name (Mapping vs _) = extractComplexField name vs
extractField name (Sequence _ _) = Left $ "invalid value for field " <> T.unpack name <> ": sequence"
extractField name (Alias _ ) = Left $ "invalid value for field " <> T.unpack name <> ": alias"

extractSimpleField :: T.Text -> BS.ByteString -> Tag -> Either String Field
extractSimpleField name bs _ = Right $ Field name bs False Nothing

extractComplexField :: T.Text -> [(T.Text, YamlValue)] -> Either String Field
extractComplexField _ _ = Left "todo"


extractConstraint :: YamlValue -> Either String Constraint
extractConstraint _ = Left "todo"


typeToSQL :: Type -> BS.ByteString
typeToSQL (Tb t) = tableToSQL t
typeToSQL (En e) = enumToSQL e


enumToSQL :: DbEnum -> BS.ByteString
enumToSQL (DbEnum n vs) = let
    nbs = TE.encodeUtf8 n
    prefix = "create type " <> nbs <> " as enum("
    vals = map ((<> "'") . ("'" <>)) vs
    suffix = ");\n\n"
    in prefix <> (BS.intercalate ", " vals) <> suffix

tableToSQL :: Table -> BS.ByteString
tableToSQL (Table n fs _) = let
    nbs = TE.encodeUtf8 n
    prefix = "create table " <> nbs <> " (\n"
    pk_line = nbs <> "_id uuid primary key"
    lns = pk_line : fmap fieldToSQL fs
    suffix = "\n);\n\n"
    in prefix <> (BS.intercalate ",\n" $ fmap ("    " <>) lns) <> suffix

fieldToSQL :: Field -> BS.ByteString
fieldToSQL (Field n t nu d) = let
    nbs = TE.encodeUtf8 n
    nn = if nu then "null" else "not null"
    df = maybe "" ("default " <>) d
    in BS.intercalate " " $ filter (/= "") [nbs, t, nn, df]


main :: IO ()
main = do
    args <- getArgs
    yml <- readYamlFile $ head args
    case extractTypes yml of
        Left err -> putStrLn $ "error: " ++ err
        Right ts -> BS.putStr $ foldMap typeToSQL ts

