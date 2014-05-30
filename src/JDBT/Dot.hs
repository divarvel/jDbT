{-# LANGUAGE OverloadedStrings #-}

module JDBT.Dot (dataToDot) where

import           Data.Foldable (foldMap)
import           Data.Maybe    (mapMaybe)
import           Data.Monoid   ((<>))
import qualified Data.Text     as T


import           JDBT.Types

dataToDot :: Schema -> T.Text
dataToDot (Schema ts) = let
    prefix = "digraph G {\n graph [ rankdir =\"TB\" ]"
    suffix = "\n}\n\n"
    in prefix <> foldMap typeToDot ts <> dataDepsToDot ts <> suffix

typeToDot :: Type -> T.Text
typeToDot (Tb t) = tableToDot t
typeToDot (En e) = enumToDot e

entityToDot :: T.Text -> T.Text -> T.Text
entityToDot name content = let
    prefix = name <> " [ label=<<TABLE BORDER=\"1\" CELLBORDER=\"0\" CELLSPACING=\"0\" WIDTH=\"100\">\n"
    header = dotLine "LEFT" "#BBBBBB" name
    suffix = "</TABLE>> shape=\"plaintext\" ];\n\n"
    in prefix <> header <> content <> suffix

dotLine :: T.Text -> T.Text -> T.Text -> T.Text
dotLine align color name = "<TR><TD ALIGN=\"" <> align <>"\" BGCOLOR=\"" <> color <> "\" WIDTH=\"100\">" <> name <> "</TD></TR>"


enumToDot :: DbEnum -> T.Text
enumToDot (DbEnum n vs) = entityToDot n $ foldMap enumValueToDot vs
    where
        enumValueToDot = dotLine "LEFT" "#CCCCFF"

tableToDot :: Table -> T.Text
tableToDot (Table n fs _) = entityToDot n $ foldMap fieldToDot fs

fieldToDot :: Field -> T.Text
fieldToDot (Field n t _ cs)
    | any isPk cs = dotLine "LEFT" "#FFCCCC" content
    | any isFk cs = dotLine "LEFT" "#CCFFCC" content
    | otherwise = dotLine "LEFT" "#FFFFFF" content
    where
        content =  n <> ": " <> t

dataDepsToDot :: [Type] -> T.Text
dataDepsToDot = foldMap (uncurry depToDot) . concatMap entityDeps
    where
        entityDeps (Tb t) = tableDeps t
        entityDeps (En _) = []
        tableDeps (Table n fs _) = concatMap (fieldToDeps n) fs
        fieldToDeps n (Field _ _ _ cs) = mapMaybe (constraintToDep n) cs
        constraintToDep n1 (Fk n2 _) = Just (n1, n2)
        constraintToDep _   _        = Nothing
        depToDot a b =  a <> " -> " <> b <> "\n"
