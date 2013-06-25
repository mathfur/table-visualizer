{-# Language TemplateHaskell,  QuasiQuotes,  FlexibleContexts #-}

module Parser where

import Prelude hiding (lines)
import Text.Peggy
import Data.Either (rights)
import Data.List (elemIndex)
import Data.Text (pack)
import Data.Maybe (catMaybes)

import Schema hiding (table_name)

[peggy|
lines :: [Line]
  = line*

line :: Line
  = node_line
  / link_line

-- Example:
-- "Blog" [shape=Mrecord,  label="{Blog|title :string\lcreated_at :datetime\lupdated_at :datetime\l}"]
-- "Blog" -> "BlogImage" [label="images",  arrowtail=crow,  arrowhead=dot, dir=both]

node_line :: Line
  = model_name "[shape=Mrecord," 'label=' pairs ']' { NodeLine $1 $2 }
  / model_name "[]" { NodeLine $1 [] }

link_line :: Line
  = model_name "->" model_name '[' [^\]]* ']' { LinkLine $1 $2 }

pairs :: [(String, String)]
  = '\"{' [a-zA-Z0-9]+ '|' pair ('\\l' pair)* '\\l}\"' { [$2] ++ $3 }

pair :: (String, String)
  = table_name type_name

table_name ::: String
  = [a-zA-Z0-9_-]+

type_name ::: String
  = ':' [a-zA-Z0-9_-]+

model_name ::: String
  = '\"' [a-zA-Z0-9_-]+ '\"'
|]

data Line = NodeLine String [(String, String)]
          | LinkLine String String
          deriving (Show, Eq)

parse_line :: String -> Either ParseError Line
parse_line = parseString line "<stdin>"

parse :: [String] -> [Line]
parse = rights . map parse_line

-- data TableDef = TableDef { table_name :: Text, columns :: [ColumnDef]} deriving (Show, Eq)
-- data ColumnDef = ColumnDef { colName :: Text, colType :: Text } deriving (Show, Eq)

getTableDef :: [Line] -> [TableDef]
getTableDef lines = concat $ map f lines
  where
    f :: Line -> [TableDef]
    f (NodeLine name cols) = [TableDef (pack name) (map g cols)]
    f (LinkLine name cols) = []
    g :: (String, String) -> ColumnDef
    g (x, y) = ColumnDef (pack x) (pack y)

getLinkIndexs :: [Line] -> [(Int, Int)]
getLinkIndexs lines = catMaybes (map (getIndexPiar (getTableNames lines)) $ getLinks lines)
    where
      getTableNames :: [Line] -> [String]
      getTableNames lines = concat $ map getTableName lines
      getTableName :: Line -> [String]
      getTableName (NodeLine n _) = [n]
      getTableName (LinkLine _ _) = []
      getLinks :: [Line] -> [(String, String)]
      getLinks = concatMap getLink
      getLink :: Line -> [(String, String)]
      getLink (NodeLine _ _) = []
      getLink (LinkLine s1 s2) = [(s1, s2)]
      getIndexPiar :: [String] -> (String, String) -> Maybe (Int, Int)
      getIndexPiar ss (s1, s2) = do
        e1 <- elemIndex s1 ss
        e2 <- elemIndex s2 ss
        return (e1, e2)

-- parser_pair :: String -> Either ParseError (String, String)
-- parser_pair = parseString pair "<stdin>"
-- 
-- parser_pairs :: String -> Either ParseError [(String, String)]
-- parser_pairs = parseString pairs "<stdin>"
-- 
-- parser_link_line :: String -> Either ParseError Line
-- parser_link_line = parseString link_line "<stdin>"
-- 
-- parser_node_line :: String -> Either ParseError Line
-- parser_node_line = parseString node_line "<stdin>"
-- 
-- parser_line :: String -> Either ParseError Line
-- parser_line = parseString line "<stdin>"
-- 
-- parser_lines :: String -> Either ParseError [Line]
-- parser_lines = parseString lines "<stdin>"
