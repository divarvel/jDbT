
module JDBT.Types where

import qualified Data.ByteString as BS
import qualified Data.Text       as T

data FieldConstraint = NotNull
                     | Pk
                     | Fk T.Text T.Text
                     | Unique
                     | Other BS.ByteString
                     deriving (Show)

data TableConstraint = TableConstraint [T.Text] FieldConstraint
                     deriving (Show)

data Type = Tb Table
          | En DbEnum
          deriving (Show)

data DbEnum = DbEnum T.Text [BS.ByteString]
            deriving (Show)

data Table = Table T.Text [Field] [TableConstraint]
           deriving (Show)

data Field = Field T.Text BS.ByteString (Maybe BS.ByteString) [FieldConstraint]
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
