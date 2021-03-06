
module JDBT.Types where

import           Data.Maybe (mapMaybe)
import           Data.Text (Text)

newtype Schema = Schema [Type]

type TableName = Text
type FieldName = Text

data FieldConstraint = NotNull
                     | Pk
                     | Fk TableName FieldName
                     | Unique
                     | Other Text
                     deriving (Show)

data TableConstraint = TableConstraint [FieldName] FieldConstraint
                     deriving (Show)


data Type = Tb Table
          | En DbEnum
          deriving (Show)

data DbEnum = DbEnum Text [Text]
            deriving (Show)

data Table = Table TableName [Field] [TableConstraint]
           deriving (Show)

data Field = Field FieldName Text (Maybe Text) [FieldConstraint]
           deriving (Show)


tables :: Schema -> [Table]
tables (Schema ts) = mapMaybe getTable ts
  where
    getTable (Tb t) = Just t
    getTable _ = Nothing

isPk :: FieldConstraint -> Bool
isPk Pk = True
isPk _  = False

isPkField :: Field -> Bool
isPkField (Field _ _ _ fcts) = any isPk fcts

isTablePk :: TableConstraint -> Bool
isTablePk (TableConstraint _ c) = isPk c

isFk :: FieldConstraint -> Bool
isFk (Fk _ _) = True
isFk _        = False

hasPrimaryKey :: [Field] -> [TableConstraint] -> Bool
hasPrimaryKey fields constraints = let
    fieldFk = any isPkField fields
    tableFk = any isTablePk constraints
    in fieldFk || tableFk
