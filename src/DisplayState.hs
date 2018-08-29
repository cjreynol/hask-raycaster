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

import Settings             (backgroundColor, frameRate, 
                                renderingDriverIndex,  windowSize, 
                                windowTitle)


-- | Holds the needed SDL types for drawing on the screen.
data DisplayState = DisplayState { 
      window        :: Window
    , renderer      :: Renderer
    , fpsManager    :: Manager
    }

-- | Matching demo settings to the default RaycasterState.
defaultDisplayState :: IO DisplayState
defaultDisplayState = do
    w <- createWindow windowTitle defaultWindowConfig
    r <- createRenderer w renderingDriverIndex defaultRenderer
    m <- manager
    set m frameRate
    return $ DisplayState w r m

defaultWindowConfig :: WindowConfig
defaultWindowConfig = defaultWindow { windowInitialSize = windowSize }

-- | Wipe out the screen with the background color.
clearDisplay :: DisplayState -> IO ()
clearDisplay dState = do
    let rend = renderer dState
    rendererDrawColor rend $= backgroundColor
    clear rend
    
-- | Prompt the window to display the rendering actions taken since the last 
--  update.
updateDisplay :: DisplayState -> IO ()
updateDisplay dState = present (renderer dState)

-- | Pause enough time to cap the FPS at the amount set on Manager creation.
fpsDelay :: DisplayState -> IO ()
fpsDelay dState = delay_ $ fpsManager dState

-- | Call the proper functions for shutting down the state for SDL datatypes.
cleanUpDisplayState :: DisplayState -> IO ()
cleanUpDisplayState dState = do
    destroyManager $ fpsManager dState
    destroyWindow $ window dState

