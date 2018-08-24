{-|
Module      : DisplayState
Description : Holds all of the data needed for the visuals
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

module DisplayState (
      DisplayState(DisplayState)
    , cleanUpDisplayState
    , defaultDisplayState
    , draw
    , fpsDelay
    ) where

import SDL                      (($=))
import SDL.Framerate            (Manager, delay_, manager, destroyManager, 
                                    set)
import SDL.Primitive            (fillCircle, line)
import SDL.Video                (Window, createRenderer, createWindow, 
                                    defaultWindow, destroyWindow)
import SDL.Video.Renderer       (Renderer, clear, defaultRenderer, present, 
                                    rendererDrawColor)

import RaycasterState           (RaycasterState(..), toPos)
import Settings                 (backgroundColor, camColor, dirColor, 
                                    frameRate, playerColor, playerSize, 
                                    renderingDriverIndex, windowTitle)


data DisplayState = DisplayState { 
      window        :: Window
    , renderer      :: Renderer
    , fpsManager    :: Manager
    }

defaultDisplayState :: IO DisplayState
defaultDisplayState = do
    w <- createWindow windowTitle defaultWindow
    r <- createRenderer w renderingDriverIndex defaultRenderer
    m <- manager
    set m frameRate
    return $ DisplayState w r m

draw :: DisplayState -> RaycasterState -> IO ()
draw dState rcState = do
    let rend = renderer dState
        playerPos = viewPos rcState
        dirVect = playerPos + (viewDirVec rcState)
        camVect = dirVect + (viewCamVec rcState)
        camVect2 = dirVect - (viewCamVec rcState) -- mirrored across dirVect

    rendererDrawColor rend $= backgroundColor
    clear rend
    line rend (toPos dirVect) (toPos camVect) camColor
    line rend (toPos dirVect) (toPos camVect2) camColor
    line rend (toPos playerPos) (toPos dirVect) dirColor
    fillCircle rend (toPos playerPos) playerSize playerColor
    present rend

fpsDelay :: DisplayState -> IO ()
fpsDelay dState = delay_ $ fpsManager dState

cleanUpDisplayState :: DisplayState -> IO ()
cleanUpDisplayState dState = do
    destroyManager $ fpsManager dState
    destroyWindow $ window dState
