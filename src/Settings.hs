{-|
Module      : Settings
Description : All of the constant or adjustable values used in the program
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

{-# LANGUAGE OverloadedStrings #-}

module Settings where

import Data.Text            (Text)
import Data.Word            (Word8)
import Foreign.C.Types      (CInt)

import SDL.Framerate        (Framerate)
import SDL.Primitive        (Color, Radius)
import SDL.Vect             (V4(..))


-- SDL2/Application settings
windowTitle :: Text
windowTitle = "Testing"

frameRate :: Framerate
frameRate = 60

renderingDriverIndex :: CInt
renderingDriverIndex = -1

-- options
playerSize :: Radius
playerSize = 5

-- speed
moveSpeed :: Double
moveSpeed = 1

turnSpeed :: Double     -- in radians
turnSpeed = pi / 8

-- colors
backgroundColor :: V4 Word8
backgroundColor = V4 255 255 255 255

playerColor :: Color
playerColor = V4 0 0 0 255

dirColor :: Color
dirColor = V4 205 0 0 255

camColor :: Color
camColor = V4 20 205 20 255

