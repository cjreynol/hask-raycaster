{-|
Module      : Main
Description : 
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
    ) where

import Control.Monad    (unless)

import SDL

import Direction        (getDirVector)
import DisplayState     (DisplayState(..), draw)
import EventHandling    (getMoveDir, isEscPress, isQuitEvent)

import RaycasterState   (RaycasterState, changeVel, defaultRaycasterState,
                            updatePos)


main :: IO ()
main = do
    initializeAll
    w <- createWindow "Testing" defaultWindow
    r <- createRenderer w (-1) defaultRenderer
    appLoop (DisplayState w r) defaultRaycasterState
    quit

appLoop :: DisplayState -> RaycasterState -> IO ()
appLoop dState rcState = do
    events <- pollEvents
    let isQuit = any isEscPress events || any isQuitEvent events

    let delta = sum $ map (getDirVector . getMoveDir) events
    let nextState = updatePos $ changeVel delta rcState

    draw dState nextState
    unless isQuit $ appLoop dState nextState

