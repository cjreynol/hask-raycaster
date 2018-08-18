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

import  Control.Monad   (unless, when)
import  SDL


main :: IO ()
main = do
    initializeAll
    w <- createWindow "Testing" defaultWindow
    r <- createRenderer w (-1) defaultRenderer
    rendererDrawColor r $= V4 0 0 255 255
    let state = RCState w r 0
    appLoop state
    quit

appLoop :: RCState -> IO ()
appLoop state = do
    events <- pollEvents
    let isQuit = any isEscPress events
    let upPressed = any isUpPress events
    let nextState = if upPressed then increaseCount state else state
    clear $ rcRenderer state
    present $ rcRenderer state
    when upPressed $ putStrLn $ show . rcCount $ nextState
    unless isQuit $ appLoop nextState


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
    , rcCount       :: Int
    }

increaseCount :: RCState -> RCState
increaseCount RCState{..} = RCState rcWindow rcRenderer (rcCount + 1)
