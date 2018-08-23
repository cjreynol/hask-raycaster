{-|
Module      : EventHandling
Description : The functions for managing SDL events and responses
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

module EventHandling (
      getMoveDir
    , getTurnDir
    , isEscPress
    , isQuitEvent
    ) where

import SDL.Event                    (Event(..), EventPayload(..), 
                                        InputMotion(..), 
                                        keyboardEventKeyMotion, 
                                        keyboardEventKeysym)
import SDL.Input.Keyboard           (keysymKeycode)
import SDL.Input.Keyboard.Codes

import Direction                    (Direction(..))
import TurnDir                      (TurnDir(..))


getMoveDir :: Event -> Maybe Direction
getMoveDir event 
    | isWPress event = Just DirUp
    | isDPress event = Just DirRight
    | isSPress event = Just DirDown
    | isAPress event = Just DirLeft
    | otherwise = Nothing

getTurnDir :: Event -> Maybe TurnDir
getTurnDir event
    | isLeftPress event = Just TLeft
    | isRightPress event = Just TRight
    | otherwise = Nothing

isKeyPress :: Keycode -> Event -> Bool
isKeyPress keycode event = 
    case eventPayload event of
        KeyboardEvent keyboardEvent -> 
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == keycode
        _ -> False

isEscPress :: Event -> Bool
isEscPress = isKeyPress KeycodeEscape

isWPress :: Event -> Bool
isWPress = isKeyPress KeycodeW

isDPress :: Event -> Bool
isDPress = isKeyPress KeycodeD

isSPress :: Event -> Bool
isSPress = isKeyPress KeycodeS

isAPress :: Event -> Bool
isAPress = isKeyPress KeycodeA

isLeftPress :: Event -> Bool
isLeftPress = isKeyPress KeycodeLeft

isRightPress :: Event -> Bool
isRightPress = isKeyPress KeycodeRight

isQuitEvent :: Event -> Bool
isQuitEvent event 
    | (eventPayload event) == QuitEvent = True
    | otherwise = False

