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
import SDL.Vect             (V2(V2), V4(V4))


-- SDL2/Application settings
windowTitle :: Text
windowTitle = "Testing"

windowSize :: V2 CInt   -- width x height
windowSize = V2 800 640

frameRate :: Framerate
frameRate = 60

renderingDriverIndex :: CInt
renderingDriverIndex = -1

defaultLayoutFile :: FilePath
defaultLayoutFile = "test.lay"

-- options
playerSize :: Radius
playerSize = 5

startPos :: V2 Double
startPos = V2 10 10

-- speed
moveSpeed :: Double
moveSpeed = 0.1

turnSpeed :: Double     -- in radians
turnSpeed = pi / 16

-- colors
backgroundColor :: V4 Word8
backgroundColor = V4 255 255 255 255

playerColor :: Color
playerColor = V4 0 0 0 255

dirColor :: Color
dirColor = V4 205 0 0 255

camColor :: Color
camColor = V4 20 205 20 255

wall1Color :: Color
wall1Color = V4 205 0 0 255

wall1DarkColor :: Color
wall1DarkColor = V4 100 0 0 255

wall2Color :: Color
wall2Color = V4 20 205 20 255

wall2DarkColor :: Color
wall2DarkColor = V4 0 100 0 255

