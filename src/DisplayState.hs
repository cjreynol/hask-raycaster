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
import SDL.Vect                 (V2(..), V4(..))
import SDL.Video                (Window)
import SDL.Video.Renderer       (Rectangle(..), Renderer, clear, fillRect, 
                                    present, rendererDrawColor)

import RaycasterState           (RaycasterState(..))


data DisplayState = DisplayState { 
      window      :: Window
    , renderer    :: Renderer
    }

draw :: DisplayState -> RaycasterState -> IO ()
draw dState rcState = do
    let rend = renderer dState
    rendererDrawColor rend $= V4 255 255 255 255
    clear rend
    rendererDrawColor rend $= V4 0 0 0 255
    fillRect rend $ Just (Rectangle (viewPos rcState) (V2 10 10))
    present rend

