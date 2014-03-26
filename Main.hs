{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Text.Libyaml (Tag, Tag(..))
import Data.Char (isAlpha)
import Data.Maybe (catMaybes, mapMaybe)
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


inferFieldConstraints :: String -> T.Text -> [FieldConstraint]
inferFieldConstraints modifiers name = catMaybes [ notNull, unique, fk ]
    where
        notNull = if '?' `elem` modifiers then Nothing else Just NotNull
        unique  = if '+' `elem` modifiers then Just Unique else Nothing
        fk      = if "_id" `T.isSuffixOf` name then
            let f_table = T.take (T.length name - 3) name
            in Just $ Fk f_table name
            else Nothing

extractSimpleField :: T.Text -> BS.ByteString -> Tag -> Either String Field
extractSimpleField name ftype _ =
    let modifiers = takeWhile (not . isAlpha) . T.unpack $ name
        realName = T.drop (length modifiers) name
        constraints = inferFieldConstraints modifiers realName
    in Right $ Field realName ftype Nothing constraints

extractComplexField :: T.Text -> [(T.Text, YamlValue)] -> Either String Field
extractComplexField _ _ = Left "todo"


--------------------------------------------------------------------------------
-- Data to SQL
--
dataToSQL :: [Type] -> BS.ByteString
dataToSQL = foldMap typeToSQL

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
    lns = fmap fieldToSQL fs ++ fmap tableConstraintToSQL cs
    suffix = "\n);\n\n"
    in prefix <> (BS.intercalate ",\n" $ fmap ("    " <>) lns) <> suffix

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

--------------------------------------------------------------------------------
-- Data to Dot
--
dataToDot :: [Type] -> BS.ByteString
dataToDot ts = let
    prefix = "digraph G {\n graph [ rankdir =\"TB\" ]"
    suffix = "\n}\n\n"
    in prefix <> foldMap typeToDot ts <> dataDepsToDot ts <> suffix

typeToDot :: Type -> BS.ByteString
typeToDot (Tb t) = tableToDot t
typeToDot (En e) = enumToDot e

entityToDot :: T.Text -> BS.ByteString -> BS.ByteString
entityToDot name content = let
    prefix = TE.encodeUtf8 $ name <> " [ label=<<TABLE BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"0\" WIDTH=\"100\">\n"
    header = dotLine "LEFT" "#BBBBBB" $ TE.encodeUtf8 name
    suffix = "</TABLE>> shape=\"plaintext\" ];\n\n"
    in prefix <> header <> content <> suffix

dotLine :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
dotLine align color name = "<TR><TD ALIGN=\"" <> align <>"\" BGCOLOR=\"" <> color <> "\" WIDTH=\"100\">" <> name <> "</TD></TR>"


enumToDot :: DbEnum -> BS.ByteString
enumToDot (DbEnum n vs) = entityToDot n $ foldMap enumValueToDot vs
    where
        enumValueToDot = dotLine "LEFT" "#CCCCFF"

tableToDot :: Table -> BS.ByteString
tableToDot (Table n fs _) = entityToDot n $ foldMap fieldToDot fs

fieldToDot :: Field -> BS.ByteString
fieldToDot (Field n t _ cs)
    | any isPk cs = dotLine "LEFT" "#FFCCCC" content
    | any isFk cs = dotLine "LEFT" "#CCFFCC" content
    | otherwise = dotLine "LEFT" "#FFFFFF" content
    where
        isPk Pk = True
        isPk _  = False
        isFk (Fk _ _) = True
        isFk _        = False
        content = TE.encodeUtf8 n <> ": " <> t

dataDepsToDot :: [Type] -> BS.ByteString
dataDepsToDot = foldMap (uncurry depToDot) . concatMap entityDeps
    where
        entityDeps (Tb t) = tableDeps t
        entityDeps (En _) = []
        tableDeps (Table n fs _) = concatMap (fieldToDeps n) fs
        fieldToDeps n (Field _ _ _ cs) = mapMaybe (constraintToDep n) cs
        constraintToDep n1 (Fk n2 _) = Just (n1, n2)
        constraintToDep _   _        = Nothing
        depToDot a b = TE.encodeUtf8 $ a <> " -> " <> b <> "\n"


main :: IO ()
main = do
    args <- getArgs
    yml <- readYamlFile $ head args
    case extractTypes yml of
        Left err -> putStrLn $ "error: " ++ err
        Right ts -> BS.putStr $ case args of
            [_, "dot"] -> dataToDot ts
            _ -> dataToSQL ts

