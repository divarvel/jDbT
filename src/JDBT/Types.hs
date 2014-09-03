module JDBT.Types where

import Data.Text (Text)

data Schema = Schema Defaults [Decl] deriving (Show)

data Defaults = Defaults PkPolicy Type deriving (Show)

data PkPolicy = Auto | Manual deriving (Show, Eq)
type Type = Text
type Value = Text

data Decl = En DbEnum
          | Tb Table
          deriving (Show)

data DbEnum = DbEnum Type [Value] deriving (Show)

data Table = Table TableName [Field] [Constraint] deriving (Show)

type TableName = Text
type FieldName = Text

data Field = Field FieldName Type (Maybe Value) Nullable deriving (Show)

data Nullable = Null | NotNull deriving (Show, Eq)

data Constraint = Pk [FieldName]
                | Fk FieldName (TableName, FieldName)
                | Unique [FieldName]
                | Other Text
                deriving (Show)
