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
import Data.Text            (Text)
import Foreign.C.Types      (CInt)

import Data.Matrix          (identity)
import SDL.Event            (pollEvents)
import SDL.Framerate        (Framerate, Manager, delay_, destroyManager, 
                                manager, set)
import SDL.Init             (initializeAll, quit)
import SDL.Video            (createRenderer, createWindow, defaultWindow)
import SDL.Video.Renderer   (defaultRenderer)

import Direction            (getDirVector)
import DisplayState         (DisplayState(..), draw)
import EventHandling        (getMoveDir, getTurnDir, isEscPress, isQuitEvent)
import RaycasterState       (RaycasterState, changeVel, defaultRaycasterState,
                                rotateView, updatePos)
import TurnDir              (getTurnDirMatrix)


main :: IO ()
main = do
    initializeAll
    w <- createWindow windowTitle defaultWindow
    r <- createRenderer w renderingDriverIndex defaultRenderer
    m <- manager
    set m frameRate
    appLoop (DisplayState w r) defaultRaycasterState m
    destroyManager m
    quit

appLoop :: DisplayState -> RaycasterState -> Manager -> IO ()
appLoop dState rcState man = do
    events <- pollEvents
    let isQuit = any isEscPress events || any isQuitEvent events

    let delta = sum $ map (getDirVector . getMoveDir) events
    let rotateMat = foldr (*) (identity 2) $ map (getTurnDirMatrix . getTurnDir) events
    let nextState = rotateView rotateMat $ updatePos $ changeVel delta rcState

    draw dState nextState
    delay_ man
    unless isQuit $ appLoop dState nextState man

windowTitle :: Text
windowTitle = "Testing"

frameRate :: Framerate
frameRate = 60

renderingDriverIndex :: CInt
renderingDriverIndex = -1

