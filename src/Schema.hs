module Schema where

import System.FilePath
import Prelude
import Language.Haskell.TH
import Data.Text

import Text.Parsec hiding ((<|>))
import Text.Parsec.Text (Parser)
import Text.Parsec.Expr
import Control.Applicative hiding (many)
import Data.Aeson

data TableDef = TableDef Text [ColumnDef] deriving (Show, Eq)
data ColumnDef = ColumnDef { colName :: Text, colType :: Text } deriving (Show, Eq)

instance ToJSON TableDef where
    toJSON (TableDef name cols) = object ["table_name" .= name, "columns" .= toJSON cols]

instance ToJSON ColumnDef where
    toJSON (ColumnDef name typ) = object ["name" .= name, "type" .= typ]

readTableDefsFromFile :: FilePath -> IO [TableDef]
readTableDefsFromFile fp = do
     contents <- readFile fp
     return $ strToTableDefs $ pack contents

strToTableDefs :: Text -> [TableDef]
strToTableDefs sp = do
    case getTableNameAndInnerBlock sp of
        Left _ -> []
        Right defs -> defs

getTableNameAndInnerBlock :: Text -> Either ParseError [TableDef]
getTableNameAndInnerBlock input = parse table_defs "(sourcename)" input

table_def_and_before :: Parser TableDef
table_def_and_before = (try $ table_def) <|> (anyChar *> table_def_and_before)

table_defs :: Parser [TableDef]
table_defs = many (try table_def_and_before)

-- create_table "pages", :force => true do |t|
table_def :: Parser TableDef
table_def = do
    table_name <- (sp *> str "create_table" *> rstring <* (optionMaybe $ c ',' *> hash) <* sp <* str "do" <* sp <* str "|t|" <* sp)
    columns <- many (try $ column_def)
    sp *> str "end" <* sp
    many (try $ index_def)
    return $ TableDef table_name columns

--    t.column "name", :string, :null => false
column_def :: Parser ColumnDef
column_def = ColumnDef <$> (str "t.column" *> rstring) <*> (c ',' *> rsymbol <* (optionMaybe $ c ',' *> hash))

-- add_index "pages", ["name"], :name => "pages_name_index"
index_def :: Parser ()
index_def = do
    sp *> str "add_index" *> rstring *> c ',' *> blanket (rstring `sepBy` (c ',')) <* (optionMaybe $ c ',' *> hash) <* sp
    return ()

rstring :: Parser Text
rstring = sp *> (pack <$> (dquote $ many1 letter_p)) <* sp

hash :: Parser [(Text, Text)]
hash = sp *> (((,) <$> (try rstring <|> try rsymbol) <*> (str "=>" *> rvalue)) `sepBy` (c ',')) <* sp

rvalue :: Parser Text
rvalue = sp *> ((try $ pack <$> str "true") <|> (try $ pack <$> str "false") <|> try rdigits <|> try rstring <|> try rsymbol) <* sp

rsymbol :: Parser Text
rsymbol = sp *> c ':' *> (pack <$> many1 letter_p) <* sp

rdigits :: Parser Text
rdigits = pack <$> many1 digit

letter_p :: Parser Char
letter_p = (alphaNum <|> c '_' <|> c '-')

blanket :: Parser a -> Parser a
blanket p = sp *> c '[' *> sp *> p <* sp <* c ']' <* sp

----------------
sp :: Parser ()
sp = spaces

str :: String -> Parser String
str = string 

c :: Char -> Parser Char
c = char

dquote :: Parser a -> Parser a
dquote p = c '"' *> p <* c '"'
