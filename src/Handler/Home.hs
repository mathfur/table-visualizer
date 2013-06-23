{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Handler.Home where

import Import

import Tree
import Schema

getHomeR :: Handler RepHtml
getHomeR = do
    let t = Branch "confs" [Branch "pages" [Node "users"]]
    defs <- liftIO $ readTableDefsFromFile "sample_schema.rb"
    defaultLayout $ do
        setTitle "title"
        toWidget [julius| var foo = #{extendTreeToJSON defs t}; alert("foo") |]
        toWidget [hamlet| <h1> Hello |]

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
