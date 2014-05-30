
module JDBT.Types where

import qualified Data.Text       as T

newtype Schema = Schema [Type]

type TableName = T.Text
type FieldName = T.Text

data FieldConstraint = NotNull
                     | Pk
                     | Fk TableName FieldName
                     | Unique
                     | Other T.Text
                     deriving (Show)

data TableConstraint = TableConstraint [FieldName] FieldConstraint
                     deriving (Show)


data Type = Tb Table
          | En DbEnum
          deriving (Show)

data DbEnum = DbEnum T.Text [T.Text]
            deriving (Show)

data Table = Table TableName [Field] [TableConstraint]
           deriving (Show)

data Field = Field FieldName T.Text (Maybe T.Text) [FieldConstraint]
           deriving (Show)



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
