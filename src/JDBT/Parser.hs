{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module JDBT.Parser where

import           Control.Applicative
import           Data.Char           (isAlphaNum)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import           Data.Maybe          (catMaybes)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Traversable    (traverse)
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.Yaml

import           JDBT.Types

instance FromJSON Schema where
    parseJSON = fmap Schema . extractTypes

-- * Top Parsers
extractTypes :: Value -> Parser [Type]
extractTypes (Object o) = H.elems <$> H.traverseWithKey extractType o
extractTypes _ = fail "Wrong top-level element, extexcted an Object"

extractType :: Text -> Value -> Parser Type
extractType name (Object o) = Tb <$> extractTable name o
extractType name (Array a) = En <$> extractEnum name a
extractType _ _ = fail "Wrong element, expected Object or Array"

-- ** Enumerations
extractEnum :: Text -> Vector Value -> Parser DbEnum
extractEnum name v = DbEnum <$> pure name <*> fmap V.toList (V.mapM extractScalar v)

-- ** Tables
extractTable :: Text -> HashMap Text Value -> Parser Table
extractTable name vs = let
    constraintValues = H.filterWithKey (const . ("__" `T.isPrefixOf`)) vs
    fieldValues = vs `H.difference` constraintValues
    constraints = H.traverseWithKey extractTableConstraint constraintValues
    fields = H.traverseWithKey extractField fieldValues
    makeTable fs cs =
        if hasPrimaryKey fs cs then
            Table name fs cs
        else let
            fname = name <> "_id"
            pkField = Field fname "uuid" Nothing [ Pk ]
            in Table name (pkField : fs) cs
    in makeTable <$> fmap H.elems fields <*> fmap H.elems constraints

-- *** Fields
extractField :: Text -> Value -> Parser Field
extractField name (String t) = extractSimpleField name t
extractField name (Null) = extractSimpleField name "uuid"
extractField name (Object o) = extractComplexField name o
extractField name _ = fail ("invalid value for field " <> T.unpack name <> "expected String or Object")

extractSimpleField :: Text -> Text -> Parser Field
extractSimpleField name ftype =
    let (modifiers,realName) = T.break (\c -> isAlphaNum c || '_' ==c) name
        constraints = inferFieldConstraints modifiers realName
        isReference = any isFk constraints
        values = T.splitOn "|" ftype
        (fieldType, defVal) = case values of
            t : d : _ -> (t, Just d)
            t : _ -> (t, Nothing)
            _ -> ("", Nothing)
    in return $ Field realName (if isReference then "uuid" else fieldType) defVal constraints


inferFieldConstraints :: Text -> T.Text -> [FieldConstraint]
inferFieldConstraints modifiers name = catMaybes [ notNull, unique, fk ]
    where
        notNull = if "?" `T.isInfixOf` modifiers then Nothing else Just NotNull
        unique  = if "+" `T.isInfixOf` modifiers then Just Unique else Nothing
        fk      = if "_id" `T.isSuffixOf` name then
            let f_table = T.take (T.length name - 3) name
            in Just $ Fk f_table name
            else Nothing

extractComplexField :: Text -> HashMap Text Value -> Parser Field
extractComplexField name values =
    let scalarFields = H.filterWithKey (const . (`elem` ["type", "default"])) values
        mandatoryLookup t = maybe (fail "missing value") return . H.lookup t
    in do
        pairs <- extractScalarPairs scalarFields
        ftype <- mandatoryLookup "type" pairs
        let defVal = H.lookup "default" pairs
        constraints <- maybe (return []) extractComplexFieldConstraints $ H.lookup "constraints" values
        return $ Field name ftype defVal constraints

extractComplexFieldConstraints :: Value -> Parser [ FieldConstraint ]
extractComplexFieldConstraints (Object o) = H.elems <$> H.traverseWithKey extractComplexFieldConstraint o
extractComplexFieldConstraints _              = fail "invalid field constraints"

extractComplexFieldConstraint :: Text -> Value -> Parser FieldConstraint
extractComplexFieldConstraint "unique" _ = return Unique
extractComplexFieldConstraint "pk" _ = return Pk
extractComplexFieldConstraint "fk" (String t) = let
        elems = T.split (== ' ') t
    in case elems of
        (tname : fname : []) -> return $ Fk tname fname
        _                    -> fail "invalid fk definition"
extractComplexFieldConstraint "not null" (String t) = return NotNull
extractComplexFieldConstraint "check" (String t) = return $ Other t
extractComplexFieldConstraint _    _ = fail "invalid field constraint"

extractTableConstraint :: Text -> Value -> Parser TableConstraint
extractTableConstraint "__pk"     (Array v)   =  flip TableConstraint Pk . V.toList <$> traverse extractFieldName v
extractTableConstraint "__pk"     (String t) = return $ TableConstraint [ t ] Pk
extractTableConstraint "__pk"     _                 = fail "invalid primary key constraint"
extractTableConstraint "__unique" (Array v)   = flip TableConstraint Unique . V.toList <$> traverse extractFieldName v
extractTableConstraint "__unique" (String t) = return $ TableConstraint [ t ] Unique
extractTableConstraint "__unique" _                 = fail "invalid unicity constraint"
extractTableConstraint "__check"  (String t) = return $ TableConstraint [] $ Other t
extractTableConstraint _          _                 = fail "invalid table constraint"


-- * Auxiliary parsers
extractScalar :: Value -> Parser Text
extractScalar (String t) = return t
extractScalar _          = fail "Expected a String"

extractScalarPairs :: HashMap Text Value -> Parser (HashMap Text Text)
extractScalarPairs = traverse extractScalar

extractFieldName :: Value -> Parser FieldName
extractFieldName (String t) = return t
extractFieldName _          = fail "Expected a String"
