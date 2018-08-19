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
    , updatePos
    ) where

import Data.Int         (Int32)
import Foreign.C.Types  (CInt(..))

import SDL.Vect         (Point(..), V2(..))


data RaycasterState = RaycasterState { 
      viewPos       :: Point V2 CInt
    , viewVel       :: V2 CInt
    , rectangles    :: [Point V2 CInt] 
    }

defaultRaycasterState :: RaycasterState
defaultRaycasterState = RaycasterState (P (V2 0 0)) (V2 0 0) []

updatePos :: RaycasterState -> RaycasterState
updatePos RaycasterState{..} = RaycasterState (viewPos + (P viewVel)) 
                                    viewVel rectangles

changeVel :: V2 Int -> RaycasterState -> RaycasterState
changeVel (V2 x y) RaycasterState{..} = RaycasterState viewPos 
                                            (viewVel + 
                                                (V2 
                                                (CInt $ (fromIntegral x :: Int32))
                                                (CInt $ (fromIntegral y :: Int32))))
                                            rectangles

