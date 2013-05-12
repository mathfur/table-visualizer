{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration)
import Network.HTTP.Conduit (newManager, def)
import Control.Monad.Logger (runLoggingT)
import System.IO (stdout)
import System.Log.FastLogger (mkLogger)

import Handler.Home

mkYesodDispatch "App" resourcesApp

makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf

    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = Logger $ appLogger foundation
        }

    app <- toWaiAppPlain foundation
    return $ logWare app

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    logger <- mkLogger True stdout
    let foundation = App conf s p manager dbconf logger

    runLoggingT
        (Database.Persist.Store.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation

getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
