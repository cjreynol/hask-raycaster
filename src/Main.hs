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

    win <- createWindow "Testing" defaultWindow
    rend <- createRenderer win (-1) defaultRenderer
    let pos =  P $ V2 0 0
    let size = V2 25 25

    let state = RCState win rend pos size

    appLoop state
    quit

appLoop :: RCState -> IO ()
appLoop state = do
    events <- pollEvents
    let isQuit = any isEscPress events || any isQuitEvent events

    let delta = sum $ map (getDirVector . getMoveDir) events
    let nextState = updatePos delta state

    draw nextState
    unless isQuit $ appLoop nextState

draw :: RCState -> IO ()
draw state = do
            let rend = rcRenderer state
            rendererDrawColor rend $= V4 255 255 255 255
            clear rend
            rendererDrawColor rend $= V4 0 0 0 255
            fillRect rend $ Just (Rectangle (rcPos state) (rcSize state))
            present rend

isKeyPress :: Keycode -> Event -> Bool
isKeyPress keycode event = 
    case eventPayload event of
        KeyboardEvent keyboardEvent -> 
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == keycode
        _ -> False

getMoveDir :: Event -> Maybe Direction
getMoveDir event 
    | isUpPress event = Just DUp
    | isRightPress event = Just DRight
    | isDownPress event = Just DDown
    | isLeftPress event = Just DLeft
    | otherwise = Nothing

isEscPress :: Event -> Bool
isEscPress = isKeyPress KeycodeEscape

isUpPress :: Event -> Bool
isUpPress = isKeyPress KeycodeUp

isRightPress :: Event -> Bool
isRightPress = isKeyPress KeycodeRight

isDownPress :: Event -> Bool
isDownPress = isKeyPress KeycodeDown

isLeftPress :: Event -> Bool
isLeftPress = isKeyPress KeycodeLeft

isQuitEvent :: Event -> Bool
isQuitEvent event 
    | (eventPayload event) == QuitEvent = True
    | otherwise = False

data RCState = RCState { 
      rcWindow      :: Window
    , rcRenderer    :: Renderer
    , rcPos         :: Point V2 CInt
    , rcSize        :: V2 CInt
    }

data Direction = DUp | DRight | DDown | DLeft
    deriving Eq

getDirVector :: Maybe Direction -> V2 CInt
getDirVector (Just dir)
    | dir == DUp = V2 0 (-moveSpeed)
    | dir == DRight = V2 moveSpeed 0
    | dir == DDown = V2 0 moveSpeed
    | dir == DLeft = V2 (-moveSpeed) 0
    | otherwise = error "Impossible direction case"
getDirVector (Nothing) = V2 0 0

updatePos :: V2 CInt -> RCState -> RCState
updatePos delta RCState{..} = RCState rcWindow rcRenderer (rcPos + (P delta)) rcSize

moveSpeed :: CInt
moveSpeed = 5

