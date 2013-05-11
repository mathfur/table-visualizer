{-# OPTIONS_GHC -Wall #-}

module Cursor where

import Prelude
import Control.Monad.State
import Control.Monad.Trans.Reader (ReaderT, ask)

data Cursor = Cursor [Int] Int deriving (Show, Eq)
data Env = Env { margin :: Int }

succX :: Int -> ReaderT Env (State Cursor) ()
succX xd = do
    Cursor (x:xs) y <- get
    put $ Cursor ((x + xd):xs) y
    return ()

succY :: Int -> ReaderT Env (State Cursor) ()
succY yd = do
    Cursor xs y <- get
    put $ Cursor xs (y+yd)
    return ()

getX :: ReaderT Env (State Cursor) Int
getX = do
    Cursor (x:_) _ <- get
    return x

getY :: ReaderT Env (State Cursor) Int
getY = do
    Cursor _ y <- get
    return y

back :: ReaderT Env (State Cursor) ()
back = do
    Cursor (_:xs) y <- get
    put $ Cursor xs y
    return ()

forward :: Int -> ReaderT Env (State Cursor) ()
forward d = do
    Cursor (x:xs) y <- get
    put $ Cursor ((x+d):x:xs) y
    return ()

inIndent :: Int -> ReaderT Env (State Cursor) a -> ReaderT Env (State Cursor) a
inIndent size foo = do
   forward size
   x <- foo
   back
   return x

askMargin :: ReaderT Env (State Cursor) Int
askMargin = do
    Env { margin = d } <- ask
    return d

addMarginY :: ReaderT Env (State Cursor) ()
addMarginY = do
    m <- askMargin
    succY m
    return ()
