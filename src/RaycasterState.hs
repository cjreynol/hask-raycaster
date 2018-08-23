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
    , toPos
    , updatePos
    ) where

import Foreign.C.Types  (CInt)

import SDL.Primitive    (Pos)
import SDL.Vect         (Point(..), V2(..))


data RaycasterState = RaycasterState { 
      viewPos       :: V2 Double
    , viewVel       :: V2 Double
    , viewDirVec    :: V2 Double
    , viewCamVec    :: V2 Double
    , rectangles    :: [Point V2 CInt] 
    }

defaultRaycasterState :: RaycasterState
defaultRaycasterState = RaycasterState (V2 0 0) (V2 0 0) (V2 0 25) (V2 25 0) []

updatePos :: RaycasterState -> RaycasterState
updatePos RaycasterState{..} = RaycasterState (viewPos + viewVel) 
                                    viewVel viewDirVec viewCamVec rectangles

changeVel :: V2 Double -> RaycasterState -> RaycasterState
changeVel delta RaycasterState{..} = RaycasterState viewPos 
                                            (viewVel + delta)
                                            viewDirVec
                                            viewCamVec
                                            rectangles

toPos :: V2 Double -> Pos
toPos = fmap round

