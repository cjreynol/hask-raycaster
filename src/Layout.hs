{-|
Module      : Layout
Description : The datatype for the "world" that is being rendered
Copyright   : (c) Chad Reynolds, 2018
License     : MIT

Layout is built to read files with a .lay extention.  
These files have nothing except lines, delimited by "\n" characters, 
consisting of numbers, delimited by " " characters, which correspond to some 
value in the final layout.  No blank lines are expected, but they will be 
filtered out and ignored.
-}

module Layout (
      Layout
    , Tile(..)
    , getLayout
    , tileToColor
    , maybeGetTile
    ) where

import Data.Array           (Array, (!), array, bounds)

import SDL.Primitive        (Color)

import Settings             (openColor, wall1Color, wall1DarkColor, 
                                wall2Color, wall2DarkColor)


-- | A convenience type for the specific arrays used by the raycaster.
type Layout = Array (Int, Int) Tile

-- | Possible tile types when reading in a layout.
data Tile = Open | Wall1 | Wall2
    deriving (Eq, Show)

-- | Read in a file and convert it to a layout.
getLayout :: FilePath -> IO Layout
getLayout f = do
    fileString <- readFile f
    return $ (createLayout . (createAssocList . parseFileString)) fileString

-- | Convert a Tile and information if it was hit on the x-axis check to Color.
tileToColor :: Tile -> Bool -> Color
tileToColor (Wall1) (True) = wall1Color
tileToColor (Wall1) (False) = wall1DarkColor
tileToColor (Wall2) (True) = wall2Color
tileToColor (Wall2) (False) = wall2DarkColor
tileToColor (Open) _ = openColor

-- | Convert from strings in file to Tile.
-- 
-- Serves as the informal key to the layout file format.
toTile :: String -> Tile
toTile "0" = Open
toTile "1" = Wall1
toTile "2" = Wall2
toTile _ = Open

-- | Attempt to retrieve a Tile from the given Layout, returning Nothing if 
--  the index is out of the bounds of the array.
-- 
--  Currently I believe this is the source of the error being thrown in the 
--  program when the view goes out of bounds.
maybeGetTile :: Layout -> (Int, Int) -> Maybe Tile
maybeGetTile lay i 
    | i >= low && i <= high = Just $ lay ! i
    | otherwise = Nothing
    where 
        (low, high) = bounds lay

parseFileString :: String -> [[Tile]]
parseFileString s = (map . map) toTile $ map words $ filter ((/=) "") $ lines s

createAssocList :: [[Tile]] -> [((Int, Int), Tile)]
createAssocList ts = helper ts (1, 1) []
    where
        helper :: [[Tile]] -> (Int, Int) -> [((Int, Int), Tile)] -> 
                    [((Int, Int), Tile)]
        helper [] _ l = l
        helper ([]:ys) p l = helper ys p l
        helper ((x:[]):ys) p@(_, b) l = helper ys (1, b + 1) $ (p, x) : l
        helper ((x:xs):ys) p@(a, b) l = helper (xs:ys) (a + 1, b) $ (p, x) : l

createLayout :: [((Int, Int), Tile)] -> Layout
createLayout l@(x:_) = array ((1, 1), (fst x)) l
createLayout [] = error "Should not have empty association list."

