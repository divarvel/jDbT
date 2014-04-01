{-# LANGUAGE OverloadedStrings #-}

module JDBT.SQL where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Foldable         (foldMap)
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE


import           JDBT.Types

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
    fieldLines = fmap fieldToSQL fs
    constraintLines = fmap (uncurry tableConstraintToSQL) $ zip [0..] cs
    allLines = fieldLines ++ constraintLines
    indent = fmap ("    " <>)
    suffix = "\n);\n\n"
    in prefix <> (BS.intercalate ",\n" $ indent allLines) <> suffix

tableConstraintToSQL :: Int -> TableConstraint -> BS.ByteString
tableConstraintToSQL _ (TableConstraint fs Pk) = TE.encodeUtf8 $ "primary key ("<> T.intercalate ", " fs <>")"
tableConstraintToSQL _ (TableConstraint fs Unique) = TE.encodeUtf8 $ "unique ("<> T.intercalate ", " fs <>")"
tableConstraintToSQL idx (TableConstraint _ (Other t)) = "constraint cst_" <> (C8.pack $ show idx) <> " check (" <> t <> ")"
tableConstraintToSQL _ _ = "" -- TODO Check what else could make sense

fieldToSQL :: Field -> BS.ByteString
fieldToSQL (Field n t d cst) = let
    nbs = TE.encodeUtf8 n
    df = maybe "" (\v -> "default " <> v <> "::" <> t) d
    in BS.intercalate " " $ filter (/= "") ([nbs, t, df] ++ fmap fieldConstraintToSQL cst)

fieldConstraintToSQL :: FieldConstraint -> BS.ByteString
fieldConstraintToSQL Pk = "primary key"
fieldConstraintToSQL NotNull = "not null"
fieldConstraintToSQL (Fk table field)  = TE.encodeUtf8 $ "references " <> table <>"(" <> field <> ")"
fieldConstraintToSQL Unique = "unique"
fieldConstraintToSQL (Other t) = "check " <> t

