{-|
Module      : Main
Description : 
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}
module Main where

import  Control.Monad   (unless)
import  Data.Text       (pack)
import  SDL 


main :: IO ()
main = do
    initializeAll
    w <- createWindow (pack "Testing") defaultWindow
    r <- createRenderer w (-1) defaultRenderer
    rendererDrawColor r $= V4 0 0 255 255
    let state = PaintState { window = w, renderer = r }
    appLoop state

appLoop :: PaintState -> IO ()
appLoop state = do
    events <- pollEvents
    let qPressed = any isQPress events
    clear $ renderer state
    present $ renderer state
    unless qPressed (appLoop state)

handleEvents :: PaintState -> Events -> Bool
isQPress :: Event -> Bool
isQPress event = 
    case eventPayload event of
        KeyboardEvent keyboardEvent -> 
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
        _ -> False

data PaintState = PaintState
    { window        :: Window
    , renderer      :: Renderer
    }

