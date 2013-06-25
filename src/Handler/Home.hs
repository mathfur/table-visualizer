{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Handler.Home where

import Import
import Data.Maybe
import Data.List
import Data.Text (split, pack, unpack)
import System.IO

import Schema
import Parser

getHomeR :: Handler RepHtml
getHomeR = do
    lines <- liftIO $ (parse . map unpack) <$> (split (== '\n') <$> pack <$> readFile "table_def.dot")
    let defs = getTableDef lines
    let links = getLinkIndexs lines
    let json = object ["nodes" .= toJSON defs, "links" .= toJSON (map convertToSourceAndTarget links)]
    liftIO $ print json
    defaultLayout $ do
         toWidget [julius| var graph = #{json} |]

convertToSourceAndTarget :: (Int, Int) -> Value
convertToSourceAndTarget (i, j) = object ["source" .= toJSON i, "target" .= toJSON j]

postHomeR :: Handler RepHtml
postHomeR = do
    defaultLayout $ do
        setTitle "title"
        toWidget [hamlet| Hello |]

getEchoR :: String -> Handler RepHtml
getEchoR _ = do
    defaultLayout $ do
        setTitle "title"
        toWidget [hamlet| Hello |]
