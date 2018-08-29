{-|
Module      : RaycasterState
Description : Holds all of the data needed for the raycaster
Copyright   : (c) Chad Reynolds, 2018
License     : MIT
-}

{-# LANGUAGE RecordWildCards #-}

module RaycasterState (
      RaycasterState(..)
    , changeVel
    , defaultRaycasterState
    , rotateView
    , toPos
    , updatePos
    ) where

import Data.Matrix      (Matrix)
import SDL.Primitive    (Pos)
import SDL.Vect         (V2)

import Layout           (Layout, getLayout)
import Settings         (defaultLayoutFile, startPos, startVel, startDir, 
                            startCam)
import TurnDir          (multVector)


-- | The state needed to calculate the player's view of the world.
data RaycasterState = RaycasterState { 
      viewPos       :: V2 Double
    , viewVel       :: V2 Double
    , viewDirVec    :: V2 Double
    , viewCamVec    :: V2 Double
    , layout        :: Layout
    }

-- | A demo state based on the Settings module and test.layout file.
defaultRaycasterState :: IO RaycasterState
defaultRaycasterState = do
    let pos = startPos
        vel = startVel
        dirVec = startDir
        camVec = startCam
    l <- getLayout defaultLayoutFile
    return $ RaycasterState pos vel dirVec camVec l

-- | Update the velocity of the state by the given amount.
changeVel :: V2 Double -> RaycasterState -> RaycasterState
changeVel delta r@RaycasterState{..} = r { viewVel = viewVel + delta }

-- | Rotate the view vectors by the given matrix.
-- 
--  Expects the a rotation matrix generated from TurnDir module function.
rotateView :: Matrix Double -> RaycasterState -> RaycasterState
rotateView m r@RaycasterState{..} = r { viewDirVec = multVector viewDirVec m,
                                        viewCamVec = multVector viewCamVec m }

-- | Convenience function to convert from SDL2 type to SDL2-gfx type.
toPos :: V2 Double -> Pos
toPos = fmap round

-- | Updates the position based on the current velocity.
updatePos :: RaycasterState -> RaycasterState
updatePos r@RaycasterState{..} = r { viewPos = viewPos + viewVel }

