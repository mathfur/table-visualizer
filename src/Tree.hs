{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Tree where

import Import hiding (Env, concat, length)

import Data.Text hiding (map, find, zip, maximum, tail, head, concat)
import Prelude hiding (concat, length)
import Data.List hiding (length)
import Control.Monad.State
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Char (isSpace)

import Cursor
import Schema

type TableName = Text

data Tree a = Node a
          | Branch a [Tree a]

createTableDef :: Text -> TableDef
createTableDef t = TableDef (head ts) (map (\n -> ColumnDef n "unknown") $ tail ts)
  where
      ts = split isSpace t

extendTree ::  [TableDef] -> Tree TableName -> Tree (Maybe TableDef)
extendTree defs (Node name)      = Node $ getTableByName defs name
extendTree defs (Branch n trees) = Branch (getTableByName defs n) $ map (extendTree defs) trees

getTableByName :: [TableDef] -> TableName -> Maybe TableDef
getTableByName tables name = find (\(TableDef name' _) -> name == name') tables

runDrawTree :: (Drawable a) => Tree a -> ReaderT Env (State Cursor) [Widget]
runDrawTree (Node a) = do
    x <- getX
    y <- getY
    let inner = drawHamlet x y a
    succY $ height a
    addMarginY
    return inner
runDrawTree (Branch n ts) = do
    x <- getX
    y <- getY
    let t1 = drawHamlet x y n
    let  d = width n
    m <- askMargin
    t2 <- inIndent (d+m) $ (mapM runDrawTree ts)>>=(return.concat)
    addMarginY
    return $ t1 ++ t2

-----------------------------------------------------
-- | Drawable

class Drawable a where
    drawHamlet :: Int -> Int -> a -> [Widget]
    width :: a -> Int
    height :: a -> Int

instance Drawable Text where
    drawHamlet x y t = [wrapText x (y + height t) t]
    width s = 12 * length s
    height _ = 14

instance Drawable a => Drawable (Tree a) where
    drawHamlet x y tree = fst $ runState (runReaderT (runDrawTree tree) (Env { margin = 8 })) $ (Cursor [x] y)
    width (Node a)  = width a
    width (Branch n ts) = width n + (maximum $ map width ts)
    height (Node a)  = height a
    height (Branch n ts) = max (height n) $ sum $ map height ts

instance Drawable a => Drawable (Maybe a) where
    drawHamlet x y (Just a) = drawHamlet x y a
    drawHamlet _ _ Nothing = [[whamlet| <text> (NA)|]]
    width (Just a) = width a
    width Nothing = 0
    height (Just a) = height a
    height Nothing = 0

-----------------------------------------------------
wrapText :: Int -> Int -> Text -> Widget
wrapText x y t = [whamlet| <text x=#{x} y=#{y}> #{t} |]

rect :: Int -> Int -> Int -> Int -> Widget
rect x y w h = [whamlet| <rect opacity=0.5 fill=red x=#{x} y=#{y} width=#{w} height=#{h}> |]

instance Drawable TableDef where
    drawHamlet x y t = let eps = 2 in [rect (x-eps) (y-eps) (width t + 2*eps) (height t + 2*eps)]
        ++ (concat $ map (\(i, s) -> drawHamlet x (y + (height s)*i) s) $ zip [0..] $ nameAndColumns t)
    width t = maximum $ map width $ nameAndColumns t
    height t = sum $ map height $ nameAndColumns t

nameAndColumns :: TableDef -> [Text]
nameAndColumns (TableDef n columns) = [n] ++ (map colName columns)

toWidget2 :: (Drawable a) => a -> GWidget App App ()
toWidget2 extend_t = do
    toWidget [whamlet|<h1> Hello3|]
    let ws = drawHamlet 0 0 extend_t
    toWidget [whamlet|<svg viewBox="0 -13 1000 1000">
             $forall w <- ws
               ^{w}
             |]
    toWidget [lucius|h1 { color: green } |]
