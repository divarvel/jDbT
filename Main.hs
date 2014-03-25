{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Text.Libyaml (Tag, Tag(..))
import Data.Yaml.Parser (YamlValue, YamlValue(..), readYamlFile)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import Data.Foldable (foldMap)


data FieldConstraint = NotNull | Pk | Fk T.Text T.Text | Unique | Other BS.ByteString deriving (Show)

data TableConstraint = TableConstraint [T.Text] FieldConstraint deriving (Show)

data Type = Tb Table | En DbEnum deriving (Show)

data DbEnum = DbEnum T.Text [BS.ByteString] deriving (Show)

data Table = Table T.Text [Field] [TableConstraint] deriving (Show)

data Field = Field T.Text BS.ByteString (Maybe BS.ByteString) [FieldConstraint] deriving (Show)


--------------------------------------------------------------------------------
-- YAML to data
--
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
    fs = (Right $ Field (name <> "_id") "uuid" Nothing [Pk]) : fmap (uncurry extractField) vs
    ffs = sequence fs
    in fmap (\fields -> Table name fields []) ffs

extractField :: T.Text -> YamlValue -> Either String Field
extractField name (Scalar bs t _ _) = extractSimpleField name bs t
extractField name (Mapping vs _) = extractComplexField name vs
extractField name (Sequence _ _) = Left $ "invalid value for field " <> T.unpack name <> ": sequence"
extractField name (Alias _ ) = Left $ "invalid value for field " <> T.unpack name <> ": alias"

extractSimpleField :: T.Text -> BS.ByteString -> Tag -> Either String Field
extractSimpleField name bs _ =
    if "_id" `T.isSuffixOf` name
    then
        extractFkField (T.take (T.length name - 3) name) bs
    else
        Right $ Field name bs Nothing [ NotNull ]

extractFkField :: T.Text -> BS.ByteString -> Either String Field
extractFkField table_name bs = let
    field_name = table_name <> "_id"
    cst = Fk table_name field_name
    ccst = if bs == "null" then [cst] else [NotNull, cst]
    in Right $ Field field_name "uuid" Nothing ccst

extractComplexField :: T.Text -> [(T.Text, YamlValue)] -> Either String Field
extractComplexField _ _ = Left "todo"


--------------------------------------------------------------------------------
-- Data to SQL
--
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
tableToSQL (Table n fs cs) = let
    nbs = TE.encodeUtf8 n
    prefix = "create table " <> nbs <> " (\n"
    lines = fmap fieldToSQL fs ++ fmap tableConstraintToSQL cs
    suffix = "\n);\n\n"
    in prefix <> (BS.intercalate ",\n" $ fmap ("    " <>) lines) <> suffix

tableConstraintToSQL :: TableConstraint -> BS.ByteString
tableConstraintToSQL (TableConstraint fs Pk) = TE.encodeUtf8 $ "primary key ("<> T.intercalate ", " fs <>")"
tableConstraintToSQL (TableConstraint fs Unique) = TE.encodeUtf8 $ "unique ("<> T.intercalate ", " fs <>")"
tableConstraintToSQL (TableConstraint _ (Other t)) = "check " <> t
tableConstraintToSQL _ = "" -- TODO Check what else could make sense

fieldToSQL :: Field -> BS.ByteString
fieldToSQL (Field n t d cst) = let
    nbs = TE.encodeUtf8 n
    df = maybe "" ("default " <>) d
    in BS.intercalate " " $ filter (/= "") ([nbs, t, df] ++ fmap fieldConstraintToSQL cst)

fieldConstraintToSQL :: FieldConstraint -> BS.ByteString
fieldConstraintToSQL Pk = "primary key"
fieldConstraintToSQL NotNull = "not null"
fieldConstraintToSQL (Fk table field)  = TE.encodeUtf8 $ "references " <> table <>"(" <> field <> ")"
fieldConstraintToSQL Unique = "unique"
fieldConstraintToSQL (Other t) = "check " <> t


main :: IO ()
main = do
    args <- getArgs
    yml <- readYamlFile $ head args
    case extractTypes yml of
        Left err -> putStrLn $ "error: " ++ err
        Right ts -> BS.putStr $ foldMap typeToSQL ts

