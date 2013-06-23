{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Handler.Home where

import Import
import Data.Maybe
import Data.List

import Schema

getHomeR :: Handler RepHtml
getHomeR = do
    let links = [("pages", "users"), ("users", "confs")]
    defs <- liftIO $ readTableDefsFromFile "sample_schema.rb"
    let table_names = map table_name defs
    let links' = catMaybes $ (convertTableNameToIndex table_names) <$> links
    let links'' = map (\(src, dst) -> object ["source" .= src, "target" .= dst]) links'
    let json = object ["nodes" .= toJSON defs, "links" .= toJSON links'' ]
    defaultLayout $ do
         toWidget [julius| var graph = #{json} |]

convertTableNameToIndex :: [Text] -> (Text, Text) -> Maybe (Int, Int)
convertTableNameToIndex names (name1, name2) = do
  l <- elemIndex name1 names
  r <- elemIndex name2 names
  return (l, r)

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
