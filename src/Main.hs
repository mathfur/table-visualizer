{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Text hiding (map)
import Tree
import Prelude              (IO, print, ($), (++))
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (makeApplication)

import Parser

main :: IO ()
main = do
    defaultMain (fromArgs parseExtra) makeApplication

-- main :: IO ()
-- main = do
--     print $ parser_pair $ "title :string"
--     print $ parser_pairs $ "\"{Blog|title :string\\lcreated_at :datetime\\lupdated_at :datetime\\l}\""
--     print $ parser_node_line $ "\"Blog\" [shape=Mrecord,  label=\"{Blog|title :string\\lcreated_at :datetime\\lupdated_at :datetime\\l}\"]"
--     print $ parser_link_line $ "\"Blog\" -> \"BlogImage\" [label=\"images\",  arrowtail=crow,  arrowhead=dot, dir=both]"
