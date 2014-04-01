{-# LANGUAGE OverloadedStrings #-}

module JDBT.Parser ( extractTypes ) where


import           Control.Applicative   ((<$>), (<*>))
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Char             (isAlpha)
import           Data.List             (partition)
import           Data.Maybe            (catMaybes)
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Data.Yaml.Parser      (YamlValue, YamlValue (..))
import           Text.Libyaml          (Tag, Tag (..))

import           JDBT.Types

extractScalars :: [YamlValue] -> Either String [BS.ByteString]
extractScalars = mapM extractScalar
    where
        extractScalar (Scalar bs _ _ _) = Right bs
        extractScalar _                 = Left "not a scalar"

extractFieldNames :: [YamlValue] -> Either String [T.Text]
extractFieldNames = fmap (fmap TE.decodeUtf8) . extractScalars

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
    vvs = extractScalars vs
    in fmap (DbEnum name) vvs

extractTable :: T.Text -> [(T.Text, YamlValue)] -> Either String Table
extractTable name vs = let
    (constraintValues, fieldValues) = partition (("__" `T.isPrefixOf`) . fst) vs
    constraints = mapM (uncurry extractTableConstraint) constraintValues
    fields = mapM (uncurry extractField) fieldValues
    makeTable fs cs =
        if (hasPrimaryKey fs cs) then
            Table name fs cs
        else let
            fname = name <> "_id"
            pkField = Field fname "uuid" Nothing [ Pk ]
            in Table name (pkField : fs) cs
    in makeTable <$> fields <*> constraints

hasPrimaryKey :: [Field] -> [TableConstraint] -> Bool
hasPrimaryKey fields constraints = let
    fieldFk = any isPkField fields
    tableFk = any isTablePk constraints
    in fieldFk || tableFk

extractTableConstraint :: T.Text -> YamlValue -> Either String TableConstraint
extractTableConstraint "__pk"     (Sequence vs _)   = fmap (\fields -> TableConstraint fields Pk) $ extractFieldNames vs
extractTableConstraint "__pk"     (Scalar bs _ _ _) = Right $ TableConstraint [ TE.decodeUtf8 bs ] Pk
extractTableConstraint "__pk"     _                 = Left "invalid primary key constraint"
extractTableConstraint "__unique" (Sequence vs _)   = fmap (\fields -> TableConstraint fields Unique) $ extractFieldNames vs
extractTableConstraint "__unique" (Scalar bs _ _ _) = Right $ TableConstraint [ TE.decodeUtf8 bs ] Unique
extractTableConstraint "__unique" _                 = Left "invalid unicity constraint"
extractTableConstraint "__check"  (Scalar bs _ _ _) = Right $ TableConstraint [] $ Other bs
extractTableConstraint _          _                 = Left "invalid table constraint"

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
        isReference = any isFk constraints
        values = C8.split '|' ftype
        (fieldType, defVal) = case values of
            t : d : _ -> (t, Just d)
            t : _ -> (t, Nothing)
            _ -> ("", Nothing)

    in Right $ Field realName (if isReference then "uuid" else fieldType) defVal constraints

extractComplexField :: T.Text -> [(T.Text, YamlValue)] -> Either String Field
extractComplexField _ _ = Left "todo"

