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
    -- let table_infos = map createTableInfo [
    --                     "organizations name address",
    --                     "groups name",
    --                     "group_memberships member_id group_id",
    --                     "users first_name last_name age",
    --                     "reports title contents user_id",
    --                     "report_comments contents report_id",
    --                     "readings user_id report_id count"
    --                   ]
    -- let t = Branch "organizations" [
    --                                Branch "groups" [
    --                                                Branch "group_memberships" [Node "users"]
    --                                                ],
    --                                Branch "reports" [
    --                                                 Branch "report_comments" [Node "readings"]
    --                                                 ]
    --                                ]
    -- let extend_t = extendTree table_infos t
    -- putStrLn $ "<html>\n<body>\n<svg viewBox='0 -13 1000 1000'>\n" ++ unpack (draw 0 0 extend_t) ++ "</svg></body></html>"
