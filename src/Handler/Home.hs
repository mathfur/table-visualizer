{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Handler.Home where

import Import
import Data.Text (split, pack)
import System.IO

import Parser

getHomeR :: Handler RepHtml
getHomeR = do
    ls <- liftIO $ parse <$> (split (== '\n') <$> pack <$> readFile "table_def.dot")
    let json = toJSON $ getLinkIndexs ls
    defaultLayout $ do
         toWidget [julius| var graph = #{json} |]

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
