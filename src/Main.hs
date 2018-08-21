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

import Control.Monad        (unless)
import Data.Array           (Array, (!), array)
import Data.Text            (Text)
import Data.Word            (Word32)
import Foreign.C.Types      (CInt)

import SDL.Event            (pollEvents)
import SDL.Init             (initializeAll, quit)
import SDL.Time             (delay, ticks)
import SDL.Video            (createRenderer, createWindow, defaultWindow)
import SDL.Video.Renderer   (defaultRenderer)

import Direction            (getDirVector)
import DisplayState         (DisplayState(..), draw)
import EventHandling        (getMoveDir, isEscPress, isQuitEvent)
import RaycasterState       (RaycasterState, changeVel, defaultRaycasterState,
                                updatePos)


main :: IO ()
main = do
    initializeAll
    w <- createWindow windowTitle defaultWindow
    r <- createRenderer w renderingDriverIndex defaultRenderer
    putStrLn $ show $ arr ! (0, 1)
    start <- ticks
    appLoop (DisplayState w r) defaultRaycasterState start
    quit

appLoop :: DisplayState -> RaycasterState -> Word32 -> IO ()
appLoop dState rcState loopStart = do
    events <- pollEvents
    let isQuit = any isEscPress events || any isQuitEvent events

    let delta = sum $ map (getDirVector . getMoveDir) events
    let nextState = updatePos $ changeVel delta rcState

    draw dState nextState
    newTime <- ticks
    let diff = newTime - loopStart
    if diff < msPerFrame then delay (msPerFrame - diff) else return ()
    unless isQuit $ appLoop dState nextState newTime

windowTitle :: Text
windowTitle = "Testing"

msPerFrame :: Word32
msPerFrame = 1000 `div` 60

renderingDriverIndex :: CInt
renderingDriverIndex = -1

arr :: Array (Int, Int) Bool
arr = array ((0, 0), (1, 1)) [((0, 0), True), 
                                ((0, 1), False), 
                                ((1, 0), True), 
                                ((1, 1), True)]
