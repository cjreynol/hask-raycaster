{-|
Module      : EventHandling
Description : The functions for managing SDL events and responses
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

module EventHandling (
      getMoveDir
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


getMoveDir :: Event -> Maybe Direction
getMoveDir event 
    | isUpPress event = Just DirUp
    | isRightPress event = Just DirRight
    | isDownPress event = Just DirDown
    | isLeftPress event = Just DirLeft
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

