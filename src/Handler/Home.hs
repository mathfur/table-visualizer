{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Database.Persist.Sqlite

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let text_pair = Just ("foo", "bar") :: Maybe (Text, Text)
        handlerName = "postHomeR" :: Text
        submission = Nothing :: Maybe (FileInfo, FileInfo, Text, Maybe Text)
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "ここにタイトル"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, FileInfo, Text, Maybe Text)
sampleForm = renderDivs $ (,,,)
    <$> fileAFormReq "Choose a file"
    <*> fileAFormReq "Choose a file2"
    <*> areq textField "What's on the file?(A)" (Just "デフォルト値")
    <*> aopt textField "What's on the file?(B)" Nothing

getEchoR :: String -> Handler RepHtml
getEchoR s = do
    users <- selectUsers
    liftIO $ print users
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let text_pair = Just ("foo", "bar") :: Maybe (Text, Text)
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "echoタイトル2351"
        $(widgetFile "new_page")
      where
        selectUsers :: Handler [Entity User]
        selectUsers = runDB $ rawSql "SELECT ?? FROM user" []
