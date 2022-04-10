module Board (Colour (..), Space (..), Game (..), Grid (..)) where

import Prelude hiding (flip)

data Colour = Black | White
    deriving (Eq, Show)

data Space = Empty | Counter Colour
    deriving (Eq, Show)

type Grid = [Space]
type Coordinate = (Int, Int)
type Direction = (Int, Int)

data Game = Game
  { board :: Grid
  , width :: Int
  , activePlayer :: Colour }

flip :: Colour -> Colour
flip Black = White
flip White = Black

indexToCoord :: Int -> Int -> Coordinate
indexToCoord i width = (i `mod` width, i `div` width)

coordToIndex :: Coordinate -> Int -> Int
coordToIndex (x,y) width = x + y * width

spaceAtIndex :: Grid -> Coordinate -> Int -> Space
spaceAtIndex g (x,y) width = g !! coordToIndex (x,y) width

directions :: [Coordinate]
directions = [(0,-1), (1,-1), (1,0), (1,1), (0,1), (-1,1), (-1, 0), (-1,-1)]

getIndiciesToFlip :: Game -> Coordinate -> [Coordinate]
getIndiciesToFlip Game { board = g, width = w, activePlayer = c } (x,y) = concat [inDirection g w (x,y) c (dx,dy) | (dx, dy) <- directions]
  where
    inDirection :: Grid -> Int -> Coordinate -> Colour -> Direction -> [Coordinate]
    inDirection g width (x,y) c (dx,dy)
      -- If the clicked coordinate isn't empty, it's an illegal move.
      | spaceAtIndex g (x, y) width /= Empty = []
      -- If it's out of bounds.
      | x + dx >= width || y + dy >= width || x + dx < 0 || y + dy < 0 = []
      -- If the colour of the counter isn't the desired one.
      | spaceAtIndex g (x + dx, y + dy) width /= Counter (flip c) = []
      -- Otherwise, it's a valid coordinate and the function should keep checking the next one.
      | otherwise = (x + dx, y + dy) : inDirection g width (x + dx, y + dy) c (dx,dy)

legal :: Game -> Coordinate -> Bool
legal game coord = getIndiciesToFlip game coord /= []

movesAvailable :: Game -> Bool
movesAvailable Game { board = g, width = w, activePlayer = c } = or [legal (Game g w c) (x, y) | y <- [0..w], x <- [0..w]]
