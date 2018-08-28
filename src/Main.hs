{-|
Module      : Main
Description : The main loop that runs the raycaster
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

module Main (
    main
    ) where

import Control.Monad        (unless)
import Data.Matrix          (identity)

import SDL.Event            (pollEvents)
import SDL.Init             (initializeAll, quit)

import Direction            (getDirVector)
import DisplayState         (DisplayState, cleanUpDisplayState, clearDisplay, 
                                defaultDisplayState, fpsDelay, updateDisplay)
import EventHandling        (getMoveDir, getTurnDir, isEscPress, isQuitEvent)
import RaycasterState       (RaycasterState, changeVel, defaultRaycasterState,
                                rotateView, updatePos)
import Rendering            (drawRaycastedView, drawTopDown)
import TurnDir              (getTurnDirMatrix)


main :: IO ()
main = do
    initializeAll
    dState <- defaultDisplayState
    rcState <- defaultRaycasterState
    appLoop dState rcState
    cleanUpDisplayState dState
    quit

appLoop :: DisplayState -> RaycasterState -> IO ()
appLoop dState rcState = do
    events <- pollEvents
    let isQuit = any isEscPress events || any isQuitEvent events
        delta = sum $ map (getDirVector . getMoveDir) events
        rotateMat = foldr (*) (identity 2) $ 
                        map (getTurnDirMatrix . getTurnDir) events
        nextState = rotateView rotateMat $ updatePos $ changeVel delta rcState

    clearDisplay dState
    --drawTopDown dState nextState
    drawRaycastedView dState nextState
    updateDisplay dState

    fpsDelay dState
    unless isQuit $ appLoop dState nextState

