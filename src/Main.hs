{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Text hiding (map)
import Tree
import Prelude              (IO)
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (makeApplication)

main :: IO ()
main = do
    defaultMain (fromArgs parseExtra) makeApplication
