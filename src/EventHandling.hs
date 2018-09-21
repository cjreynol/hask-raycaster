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

import SDL.Event                    (Event(eventPayload), 
                                        EventPayload(KeyboardEvent, 
                                                        QuitEvent), 
                                        InputMotion(Pressed), 
                                        keyboardEventKeyMotion, 
                                        keyboardEventKeysym)
import SDL.Input.Keyboard           (keysymKeycode)
import SDL.Input.Keyboard.Codes

import Direction                    (Direction(DirDown, 
                                                DirLeft, 
                                                DirRight, 
                                                DirUp))
import TurnDir                      (TurnDir(TLeft, TRight))


-- | Convert certain key presses to the direction datatype.
getMoveDir :: Event -> Maybe Direction
getMoveDir event 
    | isWPress event = Just DirUp
    | isDPress event = Just DirRight
    | isSPress event = Just DirDown
    | isAPress event = Just DirLeft
    | otherwise = Nothing

-- | Convert certain key presses to the turn direction datatype.
getTurnDir :: Event -> Maybe TurnDir
getTurnDir event
    | isLeftPress event = Just TLeft
    | isRightPress event = Just TRight
    | otherwise = Nothing

-- | Detect if the Event is an SDL Quit event.
isQuitEvent :: Event -> Bool
isQuitEvent event 
    | (eventPayload event) == QuitEvent = True
    | otherwise = False

-- | Detect if the escape key has been pressed.
isEscPress :: Event -> Bool
isEscPress = isKeyPress KeycodeEscape

isKeyPress :: Keycode -> Event -> Bool
isKeyPress keycode event = 
    case eventPayload event of
        KeyboardEvent keyboardEvent -> 
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == keycode
        _ -> False

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

