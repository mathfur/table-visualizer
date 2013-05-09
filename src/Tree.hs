{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Tree where

import Data.Text hiding (map, find, zip, maximum, tail, head)
import Prelude hiding (concat, length)
import Data.List hiding (concat, length)
import Control.Monad.State
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Char (isSpace)

import Cursor

type TableName = Text
data Column = Column { colName :: Text } -- TODO: あとでインデックスやNOT NULLも足す

data Tree a = Node a
            | Branch a [Tree a]

data TableInfo = TableInfo TableName [Column]

createTableInfo :: Text -> TableInfo
createTableInfo t = TableInfo (head ts) (map Column $ tail ts)
    where
        ts = split isSpace t

extendTree ::  [TableInfo] -> Tree TableName -> Tree (Maybe TableInfo)
extendTree table_infos (Node name)      = Node $ getTableByName table_infos name
extendTree table_infos (Branch n trees) = Branch (getTableByName table_infos n) $ map (extendTree table_infos) trees

getTableByName :: [TableInfo] -> TableName -> Maybe TableInfo
getTableByName tables name = find (\(TableInfo name' _) -> name == name') tables

runDrawTree :: (Drawable a) => Tree a -> ReaderT Env (State Cursor) Text
runDrawTree (Node a) = do
    x <- getX
    y <- getY
    let inner = draw x y a
    succY $ height a
    addMarginY
    return inner
runDrawTree (Branch n ts) = do
    x <- getX
    y <- getY
    let t1 = draw x y n
    let  d = width n
    m <- askMargin
    t2 <- inIndent (d+m) $ (mapM runDrawTree ts)>>=(return.concat)
    addMarginY
    return $ t1 `append` t2

-----------------------------------------------------
-- | Drawable

class Drawable a where
    draw :: Int -> Int -> a -> Text
    width :: a -> Int
    height :: a -> Int

instance Drawable Text where
    draw x y t = wrapText x (y + height t) t
    width s = 12 * length s
    height _ = 14

instance Drawable a => Drawable (Tree a) where
    draw x y tree = fst $ runState (runReaderT (runDrawTree tree) (Env { margin = 8 })) $ (Cursor [x] y)
    width (Node a)  = width a
    width (Branch n ts) = width n + (maximum $ map width ts)
    height (Node a)  = height a
    height (Branch n ts) = max (height n) $ sum $ map height ts

instance Drawable a => Drawable (Maybe a) where
    draw x y (Just a) = draw x y a
    draw _ _ Nothing = "(NA)"
    width (Just a) = width a
    width Nothing = 0
    height (Just a) = height a
    height Nothing = 0

-----------------------------------------------------
wrapText :: Int -> Int -> Text -> Text
wrapText x y t = (pack $ "<text x=" ++ show x ++ " y=" ++ show y ++ ">") `append` t `append` (pack "</text>\n")

rect :: Int -> Int -> Int -> Int -> Text
rect x y w h = pack ("<rect opacity='0.5' fill='red' x='" ++ show x ++ "' y='" ++ show y ++ "' width='" ++ show w ++ "' height='" ++ show h ++ "'/>")

instance Drawable TableInfo where
    draw x y t = let eps = 2 in rect (x-eps) (y-eps) (width t + 2*eps) (height t + 2*eps)
        `append` (concat $ map (\(i, s) -> draw x (y + (height s)*i) s) $ zip [0..] $ nameAndColumns t)
    width t = maximum $ map width $ nameAndColumns t
    height t = sum $ map height $ nameAndColumns t

nameAndColumns :: TableInfo -> [Text]
nameAndColumns (TableInfo n columns) = [n] ++ (map colName columns)
