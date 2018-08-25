{-|
Module      : Layout
Description : 
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
    , Tile
    , (!)
    , getLayout
    ) where

import Data.Array   (Array, (!), array)


type Layout = Array (Int, Int) Tile

data Tile = Open | Wall1 | Wall2
    deriving (Eq, Show)

toTile :: String -> Tile
toTile "0" = Open
toTile "1" = Wall1
toTile "2" = Wall2
toTile _ = Open

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

getLayout :: FilePath -> IO Layout
getLayout f = do
    fileString <- readFile f
    return $ (createLayout . (createAssocList . parseFileString)) fileString
