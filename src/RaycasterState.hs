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
import SDL.Vect         (V2(..))

import TurnDir          (multVector)


data RaycasterState = RaycasterState { 
      viewPos       :: V2 Double
    , viewVel       :: V2 Double
    , viewDirVec    :: V2 Double
    , viewCamVec    :: V2 Double
    }

defaultRaycasterState :: RaycasterState
defaultRaycasterState = RaycasterState (V2 25 25) (V2 0 0) (V2 0 25) (V2 25 0)

updatePos :: RaycasterState -> RaycasterState
updatePos RaycasterState{..} = RaycasterState (viewPos + viewVel) 
                                    viewVel viewDirVec viewCamVec

changeVel :: V2 Double -> RaycasterState -> RaycasterState
changeVel delta RaycasterState{..} = RaycasterState viewPos 
                                            (viewVel + delta)
                                            viewDirVec
                                            viewCamVec

rotateView :: Matrix Double -> RaycasterState -> RaycasterState
rotateView m RaycasterState{..} = RaycasterState viewPos
                                            viewVel
                                            (multVector viewDirVec m)
                                            (multVector viewCamVec m)

toPos :: V2 Double -> Pos
toPos = fmap round

