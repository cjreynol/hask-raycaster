{-|
Module      : DisplayState
Description : Holds all of the data needed for the visuals
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

module DisplayState (
      DisplayState(DisplayState)
    , draw
    ) where

import SDL                      (($=))
import SDL.Primitive            (Color, Radius, fillCircle, line)
import SDL.Vect                 (V4(..))
import SDL.Video                (Window)
import SDL.Video.Renderer       (Renderer, clear, present, rendererDrawColor)

import RaycasterState           (RaycasterState(..), toPos)


data DisplayState = DisplayState { 
      window      :: Window
    , renderer    :: Renderer
    }

draw :: DisplayState -> RaycasterState -> IO ()
draw dState rcState = do
    let rend = renderer dState
    let playerPos = viewPos rcState
    let dirVect = playerPos + (viewDirVec rcState)
    let camVect = dirVect + (viewCamVec rcState)
    let camVect2 = dirVect + (-1 * (viewCamVec rcState)) -- mirrored across dir Vect

    rendererDrawColor rend $= V4 255 255 255 255
    clear rend
    fillCircle rend (toPos playerPos) playerSize playerColor
    line rend (toPos playerPos) (toPos dirVect) dirColor
    line rend (toPos dirVect) (toPos camVect) camColor
    line rend (toPos dirVect) (toPos camVect2) camColor
    present rend

playerColor :: Color
playerColor = V4 0 0 0 255

dirColor :: Color
dirColor = V4 255 0 0 255

camColor :: Color
camColor = V4 255 0 255 255

playerSize :: Radius
playerSize = 5

