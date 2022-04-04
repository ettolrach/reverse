module Board (Colour (..), Board.flip) where

import Prelude hiding (flip)

data Colour = Black | White
    deriving (Eq, Show)

data Space = Empty | Counter Colour
    deriving (Eq, Show)

type Grid = [Space]
type Coordinate = (Int, Int)
type Direction = (Int, Int)

flip :: Colour -> Colour
flip Black = White
flip White = Black

indexToCoord :: Int -> Int -> Coordinate
indexToCoord i width = (i `mod` width, i `div` width)

coordToIndex :: Coordinate -> Int -> Int
coordToIndex (x,y) width = x + y * width

spaceAtIndex :: Grid -> Coordinate -> Int -> Space
spaceAtIndex g (x,y) width = g !! coordToIndex (x,y) width

emptyGrid :: Int -> Grid
emptyGrid 0 = []
emptyGrid n = Empty : emptyGrid (n-1)

directions :: [Coordinate]
directions = [(0,-1), (1,-1), (1,0), (1,1), (0,1), (-1,1), (-1, 0), (-1,-1)]

getIndiciesToFlip :: Grid -> Int -> Coordinate -> Colour -> [Coordinate]
getIndiciesToFlip g width (x,y) c = concat [inDirection g width (x,y) c (dx,dy) | (dx, dy) <- directions]
  where
    inDirection :: Grid -> Int -> Coordinate -> Colour -> Direction -> [Coordinate]
    inDirection g width (x,y) c (dx,dy)
      -- If it's out of bounds.
      | x + dx > width || y + dy > width = []
      -- If the colour of the counter isn't the desired one.
      | spaceAtIndex g (x + dx, y + dy) width /= Counter c = []
      -- Otherwise, it's a valid coordinate and the function should keep checking the next one.
      | otherwise = (x + dx, y + dy) : inDirection g width (x + dx, y + dy) c (dx,dy)
