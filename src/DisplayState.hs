{-|
Module      : DisplayState
Description : Holds all of the data needed for the visuals
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

module DisplayState (
      DisplayState(DisplayState)
    , cleanUpDisplayState
    , clearDisplay
    , defaultDisplayState
    , drawTopDown
    , drawRaycastedView
    , fpsDelay
    , updateDisplay
    ) where

import SDL                      (($=))
import SDL.Framerate            (Manager, delay_, manager, destroyManager, 
                                    set)
import SDL.Primitive            (fillCircle, line)
import SDL.Vect                 (V2(V2))
import SDL.Video                (Window, createRenderer, createWindow, 
                                    defaultWindow, destroyWindow)
import SDL.Video.Renderer       (Renderer, clear, defaultRenderer, present, 
                                    rendererDrawColor)

import Layout                   (Layout, Tile(..), (!))
import RaycasterState           (RaycasterState(..), toPos)
import Settings                 (backgroundColor, camColor, dirColor, 
                                    frameRate, playerColor, playerSize, 
                                    renderingDriverIndex, windowSize, 
                                    windowTitle)


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

clearDisplay :: DisplayState -> IO ()
clearDisplay dState = do
    let rend = renderer dState
    rendererDrawColor rend $= backgroundColor
    clear rend
    
updateDisplay :: DisplayState -> IO ()
updateDisplay dState = present (renderer dState)

drawTopDown :: DisplayState -> RaycasterState -> IO ()
drawTopDown dState rcState = do
    let rend = renderer dState
        playerPos = viewPos rcState
        dirVect = playerPos + (viewDirVec rcState)
        camVect = dirVect + (viewCamVec rcState)
        camVect2 = dirVect - (viewCamVec rcState) -- mirrored across dirVect

    line rend (toPos dirVect) (toPos camVect) camColor
    line rend (toPos dirVect) (toPos camVect2) camColor
    line rend (toPos playerPos) (toPos dirVect) dirColor
    fillCircle rend (toPos playerPos) playerSize playerColor

drawRaycastedView :: DisplayState -> RaycasterState -> IO ()
drawRaycastedView dState rcState = do
    let rend = renderer dState
        world = layout rcState
        (V2 w h) = windowSize
        width = (fromIntegral w) :: Double
        
        dists = map (\i -> raycast i rcState world) 
                    [2 * x / width - 1 | x <- [0..width]]
    return ()

type Distance = Double

raycast :: Double -> RaycasterState -> Layout -> (Distance, Tile)
raycast i rcState layout = undefined
    where
        playerPos@(V2 posX posY) = viewPos rcState
        squareX = (fromIntegral $ floor posX) :: Double
        squareY = (fromIntegral $ floor posY) :: Double
        dirVect@(V2 dirX dirY) = playerPos + (viewDirVec rcState)
        (V2 camX camY) = dirVect + (viewCamVec rcState)
        rayDirX = dirX + camX * i
        rayDirY = dirY + camY * i
        deltaDistX = abs $ 1 / rayDirX
        deltaDistY = abs $ 1 / rayDirY
        sideDistX = if rayDirX < 0 
                        then (posX - squareX) * deltaDistX 
                        else (squareX + 1 - posX) * deltaDistX 
        sideDistY = if rayDirY < 0 
                        then (posY - squareY) * deltaDistY 
                        else (squareY + 1 - posY) * deltaDistY 
        stepX = (if rayDirX < 0 then -1 else 1) :: Double
        stepY = (if rayDirY < 0 then -1 else 1) :: Double

fpsDelay :: DisplayState -> IO ()
fpsDelay dState = delay_ $ fpsManager dState

cleanUpDisplayState :: DisplayState -> IO ()
cleanUpDisplayState dState = do
    destroyManager $ fpsManager dState
    destroyWindow $ window dState

