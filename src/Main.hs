{-|
Module      : Main
Description : 
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (
    main
    ) where

import Control.Monad    (unless)
import Foreign.C.Types  (CInt)

import SDL


main :: IO ()
main = do
    initializeAll
    w <- createWindow "Testing" defaultWindow
    r <- createRenderer w (-1) $ RendererConfig SoftwareRenderer False
    s <- getWindowSurface w
    i <- loadBMP "assets/guy.bmp" 
    let x =  P (V2 0 0)
    rendererDrawColor r $= V4 255 255 255 255
    let state = RCState w r s i x
    appLoop state
    quit

appLoop :: RCState -> IO ()
appLoop state = do
    events <- pollEvents
    let isQuit = any isEscPress events
    let upPressed = any isUpPress events
    let nextState = if upPressed then movePos moveSpeed state else state

    draw nextState
    unless isQuit $ appLoop nextState

draw :: RCState -> IO ()
draw state = do
            clear $ rcRenderer state
            _ <- surfaceBlit (rcImage state) Nothing (rcScreen state) (Just $ rcPos state)
            updateWindowSurface $ rcWindow state
            present $ rcRenderer state

isKeyPress :: Keycode -> Event -> Bool
isKeyPress keycode event = 
    case eventPayload event of
        KeyboardEvent keyboardEvent -> 
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == keycode
        _ -> False

isEscPress :: Event -> Bool
isEscPress = isKeyPress KeycodeEscape

isUpPress :: Event -> Bool
isUpPress = isKeyPress KeycodeUp

data RCState = RCState { 
      rcWindow      :: Window
    , rcRenderer    :: Renderer
    , rcScreen      :: Surface
    , rcImage       :: Surface
    , rcPos         :: Point V2 CInt
    }

movePos :: V2 CInt -> RCState -> RCState
movePos delta RCState{..} = RCState rcWindow rcRenderer rcScreen rcImage (rcPos + (P delta))

moveSpeed :: V2 CInt
moveSpeed = V2 5 5

