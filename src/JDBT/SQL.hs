{-# LANGUAGE OverloadedStrings #-}

module JDBT.SQL where

import           Data.Foldable (foldMap)
import           Data.Maybe (mapMaybe)
import           Data.Monoid   ((<>))
import qualified Data.Text     as T


import           JDBT.Types

--------------------------------------------------------------------------------
-- Data to SQL
--
dataToSQL :: Schema -> T.Text
dataToSQL s@(Schema t) = let
    types = foldMap typeToSQL t
    deferredConstraints = foldMap fkToSql . collectAllFks $ tables s
    in types <> "\n" <> deferredConstraints

typeToSQL :: Type -> T.Text
typeToSQL (Tb t) = tableToSQL t
typeToSQL (En e) = enumToSQL e


enumToSQL :: DbEnum -> T.Text
enumToSQL (DbEnum n vs) = let
    nbs = n
    prefix = "create type " <> nbs <> " as enum("
    vals = map ((<> "'") . ("'" <>)) vs
    suffix = ");\n\n"
    in prefix <> (T.intercalate ", " vals) <> suffix

tableToSQL :: Table -> T.Text
tableToSQL (Table n fs cs) = let
    nbs = n
    prefix = "create table " <> nbs <> " (\n"
    fieldLines = fmap fieldToSQL fs
    constraintLines = fmap (uncurry tableConstraintToSQL) $ zip [0..] cs
    allLines = fieldLines ++ constraintLines
    indent = fmap ("    " <>)
    suffix = "\n);\n\n"
    in prefix <> (T.intercalate ",\n" $ indent allLines) <> suffix


collectTableFkS :: Table -> [(TableName, FieldName, TableName, FieldName)]
collectTableFkS (Table tn fs _) = let
    getFk (Fk table field) = Just (table, field)
    getFk _ = Nothing
    makeQuad fn (x, y) = (tn, fn, x, y)
    fieldFks (Field fn _ _ cs) = fmap (makeQuad fn) $ mapMaybe getFk cs
    in fs >>= fieldFks

collectAllFks :: [Table] -> [(TableName, FieldName, TableName, FieldName)]
collectAllFks = (>>= collectTableFkS)

fkToSql :: (TableName, FieldName, TableName, FieldName) -> T.Text
fkToSql (fromTable, fromField, toTable, toField) = 
  "alter table only \n" <> fromTable <> " add constraint " <> toTable <> "_" <> toField <> "_fkey " <>
  "foreign key (" <> fromField <> ") references " <> toTable <> "(" <> toField <> ");\n"

tableConstraintToSQL :: Int -> TableConstraint -> T.Text
tableConstraintToSQL _ (TableConstraint fs Pk) = "primary key ("<> T.intercalate ", " fs <>")"
tableConstraintToSQL _ (TableConstraint fs Unique) = "unique ("<> T.intercalate ", " fs <>")"
tableConstraintToSQL idx (TableConstraint _ (Other t)) = "constraint cst_" <> (T.pack $ show idx) <> " check (" <> t <> ")"
tableConstraintToSQL _ _ = "" -- TODO Check what else could make sense

fieldToSQL :: Field -> T.Text
fieldToSQL (Field n t d cst) = let
    nbs =  n
    df = maybe "" (\v -> "default " <> v <> "::" <> t) d
    in T.intercalate " " $ filter (/= "") ([nbs, t, df] ++ fmap fieldConstraintToSQL cst)

fieldConstraintToSQL :: FieldConstraint -> T.Text
fieldConstraintToSQL Pk = "primary key"
fieldConstraintToSQL NotNull = "not null"
--fieldConstraintToSQL (Fk table field)  = "references " <> table <>"(" <> field <> ")"
fieldConstraintToSQL (Fk _ _)  = ""
fieldConstraintToSQL Unique = "unique"
fieldConstraintToSQL (Other t) = "check (" <> t <> ")"

