{-# Language TemplateHaskell,  QuasiQuotes,  FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification, DeriveFunctor #-}

module Parser where

import Prelude hiding (lines)
import Text.Peggy
import Data.Either (rights)
import Data.List (elemIndex)
import Data.Text (pack)
import Data.Maybe (catMaybes)
import Data.Text hiding (map,concat,concatMap)
import Control.Applicative ((<$>), (<*>))
import Data.Monoid
import Data.Aeson

import Schema

-- Example:
-- "Blog" [shape=Mrecord,  label="{Blog|title :string\lcreated_at :datetime\lupdated_at :datetime\l}"]
-- "Blog" -> "BlogImage" [label="images",  arrowtail=crow,  arrowhead=dot, dir=both]

[peggy|
line :: Line Text
  = node_line { Line [$1] [] }
  / link_line { Line [] [$1] }

node_line :: TableDef
  = model_name "[shape=Mrecord," 'label=' pairs ']' { TableDef $1 $2 }
  / model_name "[]" { TableDef $1 [] }

link_line :: LinkInfo Text
  = model_name "->" model_name '[' [^\]]* ']' { LinkInfo $1 $2 }

pairs :: [ColumnDef]
  = '\"{' [a-zA-Z0-9]+ '|' pair ('\\l' pair)* '\\l}\"' { [$2] ++ $3 }

pair :: ColumnDef
  = table_name_ type_name { ColumnDef $1 $2 }

table_name_ ::: Text
  = [a-zA-Z0-9_-]+ { pack $1 }

type_name ::: Text
  = ':' [a-zA-Z0-9_-]+ { pack $1 }

model_name ::: Text
  = '\"' [a-zA-Z0-9_-]+ '\"' { pack $1 }
|]

data Line a = Line { defs :: [TableDef],  infos :: [LinkInfo a] } deriving (Show, Eq)

instance (ToJSON a) => ToJSON (Line a) where
    toJSON (Line ds ifs) = object [
                                  "nodes" .= toJSON ds,
                                  "links" .= toJSON (
                                                    map (\is -> object [
                                                                       "source" .= toJSON (source is),
                                                                       "target" .= toJSON (target is)
                                                                       ]) ifs
                                                    )
                                  ]

instance Monoid (Line a) where
    mempty = Line [] []
    mappend (Line a1 b1) (Line a2 b2) = Line (a1 ++ a2) (b1 ++ b2)

data LinkInfo a = LinkInfo { source :: a, target :: a } deriving (Show, Eq, Functor)

parse :: [Text] -> Line Text
parse ts = mconcat $ rights $ map parse_line ts
  where
    parse_line :: Text -> Either ParseError (Line Text)
    parse_line = parseString line "<stdin>" . unpack

getLinkIndexs :: Line Text -> Line Int
getLinkIndexs ls = Line (defs ls) $ f ((map table_name . defs) ls) (infos ls)
  where
    f :: [Text] -> [LinkInfo Text] -> [LinkInfo Int]
    f ss = catMaybes . map (getIndexPair ss)
    getIndexPair :: [Text] -> LinkInfo Text -> Maybe (LinkInfo Int)
    getIndexPair ss (LinkInfo s1 s2) = do
      e1 <- elemIndex s1 ss
      e2 <- elemIndex s2 ss
      return $ LinkInfo e1 e2
