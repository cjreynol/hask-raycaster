{-|
Module      : Main
Description : 
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import  Control.Monad   (unless)
import  SDL


main :: IO ()
main = do
    initializeAll
    w <- createWindow "Testing" defaultWindow
    r <- createRenderer w (-1) defaultRenderer
    rendererDrawColor r $= V4 0 0 255 255
    let state = PaintState w r
    appLoop state

appLoop :: PaintState -> IO ()
appLoop state = do
    events <- pollEvents
    let qPressed = any isQPress events
    clear $ psRenderer state
    present $ psRenderer state
    unless qPressed $ appLoop state

isKeyPress :: Keycode -> Event -> Bool
isKeyPress keycode event = 
    case eventPayload event of
        KeyboardEvent keyboardEvent -> 
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == keycode
        _ -> False

isQPress :: Event -> Bool
isQPress = isKeyPress KeycodeQ

data PaintState = PaintState { 
      psWindow        :: Window
    , psRenderer      :: Renderer
    }

