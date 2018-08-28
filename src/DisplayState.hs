{-|
Module      : DisplayState
Description : Holds all of the data needed for the visuals
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

module DisplayState (
      DisplayState(..)
    , cleanUpDisplayState
    , clearDisplay
    , defaultDisplayState
    , fpsDelay
    , updateDisplay
    ) where

import SDL                  (($=))
import SDL.Framerate        (Manager, delay_, manager, destroyManager, 
                                set)
import SDL.Video            (Window, WindowConfig(..), createRenderer, 
                                createWindow, defaultWindow, destroyWindow)
import SDL.Video.Renderer   (Renderer, clear, defaultRenderer, present, 
                                rendererDrawColor)

import RaycasterState       (RaycasterState(..), toPos)
import Settings             (backgroundColor, frameRate, 
                                renderingDriverIndex,  windowSize, 
                                windowTitle)


data DisplayState = DisplayState { 
      window        :: Window
    , renderer      :: Renderer
    , fpsManager    :: Manager
    }

defaultDisplayState :: IO DisplayState
defaultDisplayState = do
    w <- createWindow windowTitle windowConfig
    r <- createRenderer w renderingDriverIndex defaultRenderer
    m <- manager
    set m frameRate
    return $ DisplayState w r m

windowConfig :: WindowConfig
windowConfig = defaultWindow { windowInitialSize = windowSize }

clearDisplay :: DisplayState -> IO ()
clearDisplay dState = do
    let rend = renderer dState
    rendererDrawColor rend $= backgroundColor
    clear rend
    
updateDisplay :: DisplayState -> IO ()
updateDisplay dState = present (renderer dState)

fpsDelay :: DisplayState -> IO ()
fpsDelay dState = delay_ $ fpsManager dState

cleanUpDisplayState :: DisplayState -> IO ()
cleanUpDisplayState dState = do
    destroyManager $ fpsManager dState
    destroyWindow $ window dState

