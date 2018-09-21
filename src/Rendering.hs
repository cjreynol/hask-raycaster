{-|
Module      : Rendering
Description : Handles all of the drawing and computation to facilitate it
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

module Rendering (
      drawTopDown
    , drawRaycastedView
    ) where

import Data.Maybe       (fromJust, isJust, isNothing)

import SDL.Primitive    (Pos, fillCircle, line)
import SDL.Vect         (V2(V2))

import DisplayState     (DisplayState(renderer))
import Layout           (Tile(Open), tileToColor, maybeGetTile)
import RaycasterState   (RaycasterState(layout, viewPos, viewCamVec, 
                                        viewDirVec), 
                            toPos)
import Settings         (camColor, dirColor, playerColor, playerSize, 
                            windowSize)


-- | Draw a top-down view of the player and their view vectors on the screen.
-- 
--  Initially used for early testing of the game's update loop, will be 
--  refactored to be minimap-style view in the future.
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

-- | Calculate and draw the walls using raycasting.
drawRaycastedView :: DisplayState -> RaycasterState -> IO ()
drawRaycastedView dState rcState = do
    let rend = renderer dState
        (V2 w _) = windowSize
        width = (fromIntegral w) :: Double
        xs = [0..width]
        
        distTileSides = map (\i -> raycast i rcState) 
                        [2 * x / width - 1 | x <- xs]
        posColors = zipWith (\x (dist, tile, xSide) -> 
                                (lineCalc dist x, tileToColor tile xSide))
                            xs distTileSides
    _ <- sequence $ fmap (\((p1, p2), col) -> line rend p1 p2 col) posColors
    return ()

type Distance = Double
type XSide = Bool

lineCalc :: Distance -> Double -> (Pos, Pos)
lineCalc dist x = ((toPos $ V2 x start'), (toPos $ V2 x end'))
    where
        (V2 _ h) = windowSize
        height = fromIntegral h
        lHeight = height / dist
        start = -lHeight / 2 + height / 2
        start' = if start < 0 then 0 else start
        end = lHeight / 2 + height / 2
        end' = if end >= height then height - 1 else end

raycast :: Double -> RaycasterState -> (Distance, Tile, XSide)
raycast i rcState = helper sideDists 
                            (fromIntegral mapX, fromIntegral mapY) 
                            True
    where
        world = layout rcState
        playerPos@(V2 posX posY) = viewPos rcState
        (mapX, mapY) = (floor posX :: Int, floor posY :: Int)
        dirVect@(V2 dirX dirY) = playerPos + (viewDirVec rcState)
        (V2 camX camY) = dirVect + (viewCamVec rcState)
        (rayDirX, rayDirY) = (dirX + camX * i, dirY + camY * i)
        (deltaDistX, deltaDistY) = (abs $ 1 / rayDirX, abs $ 1 / rayDirY)
        sideDists = ((if rayDirX < 0 
                        then (posX - (fromIntegral mapX)) * deltaDistX 
                        else ((fromIntegral mapX) + 1 - posX) * deltaDistX), 
                    (if rayDirY < 0 
                        then (posY - (fromIntegral mapY)) * deltaDistY 
                        else ((fromIntegral mapY) + 1 - posY) * deltaDistY))
        (stepX, stepY) = ((if rayDirX < 0 then -1 else 1) :: Double, 
                        (if rayDirY < 0 then -1 else 1) :: Double)

        helper :: (Distance, Distance) -> (Double, Double) 
                    -> XSide -> (Distance, Tile, XSide)
        helper (rayX, rayY) m@(mX, mY) xHit
            | isJust (maybeGetTile world (floor mX, floor mY)) 
                && fromJust (maybeGetTile world (floor mX, floor mY)) /= Open = 
                    (distCorrection m xHit, 
                        fromJust $ maybeGetTile world (floor mX, floor mY),
                        xHit)
            | isNothing $ maybeGetTile world (floor mX, floor mY) = 
                    (1, Open, True)
            | rayX < rayY = helper (rayX + deltaDistX, rayY)
                                    (mX + stepX, mY)
                                    True
            | otherwise = helper (rayX, rayY + deltaDistY)
                                    (mX, mY + stepY)
                                    False
        distCorrection :: (Double, Double) -> XSide -> Distance
        distCorrection (mX, mY) xHit 
            | xHit = (mX - posX + (1 - stepX) / 2) / rayDirX
            | otherwise = (mY - posY + (1 - stepY) / 2) / rayDirY

