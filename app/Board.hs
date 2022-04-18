module Board (Colour (..), Space (..), Coordinate, Grid (..), Game (..), gridString, colourStr, colourStrLower, flip, movesAvailable, makeMove, winner) where

import Prelude hiding (flip)
import qualified Data.Sequence as Seq
import qualified Data.Foldable

data Colour = Black | White
    deriving (Eq)

data Space = Empty | Counter Colour
    deriving (Eq)

type Grid = Seq.Seq Space
type Coordinate = (Int, Int)
type Direction = (Int, Int)

data Game = Game
  { board :: Grid
  , width :: Int
  , activePlayer :: Colour }

instance Show Colour where
  show Black = "B"
  show White = "W"

instance Show Space where
  show Empty = " "
  show (Counter c) = show c

instance Show Game where
  show Game { board = g, width = w, activePlayer = c } = gridString (Game g w c) ++ "\n" ++ info
    where
      info = "Width: " ++ show w ++ "; Active: " ++ show c

gridString :: Game -> String
gridString Game { board = g, width = w, activePlayer = c } = intersperseEvery w '\n' gStrList
  where
    gStrList = concatMap show (Data.Foldable.toList g)

intersperseEvery :: Int -> a -> [a] -> [a]
intersperseEvery n c l = go n l
  where
    go _ [] = []
    go 0 l = c : go n l
    go r (x:xs) = x : go (r-1) xs

colourStr :: Colour -> String
colourStr Black = "Black"
colourStr White = "White"

colourStrLower :: Colour -> String
colourStrLower Black = "black"
colourStrLower White = "white"

flip :: Colour -> Colour
flip Black = White
flip White = Black

indexToCoord :: Int -> Int -> Coordinate
indexToCoord i width = (i `mod` width, i `div` width)

coordToIndex :: Coordinate -> Int -> Int
coordToIndex (x,y) width = x + y * width

spaceAtIndex :: Grid -> Coordinate -> Int -> Maybe Space
spaceAtIndex g (x,y) width = g Seq.!? coordToIndex (x,y) width

directions :: [Coordinate]
directions = [(0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1)]

getIndiciesToFlip :: Game -> Coordinate -> [Coordinate]
-- Use a list comprehension to calculate the indicies in each of the 8 directions.
getIndiciesToFlip Game { board = g, width = w, activePlayer = c } (x,y) = concat [inDirection g w (x,y) c (dx,dy) | (dx, dy) <- directions]
  where
    inDirection :: Grid -> Int -> Coordinate -> Colour -> Direction -> [Coordinate]
    inDirection g width (x,y) c (dx,dy)
      -- If the clicked coordinate isn't empty, it's an illegal move.
      | spaceAtIndex g (x, y) width /= Just Empty = []
      -- If it's out of bounds, then it need not be flipped.
      | x + dx >= width || y + dy >= width || x + dx < 0 || y + dy < 0 = []
      -- If the colour of the counter isn't the desired one, then it also doesn't need to be flipped.
      | spaceAtIndex g (x + dx, y + dy) width /= Just (Counter (flip c)) = []
      -- Otherwise, it's a valid coordinate and the function should keep checking the next one.
      | otherwise = (x + dx, y + dy) : inDirection g width (x + dx, y + dy) c (dx,dy)

legal :: Game -> Coordinate -> Bool
legal game coord = getIndiciesToFlip game coord /= []

movesAvailable :: Game -> Bool
movesAvailable Game { board = g, width = w, activePlayer = c } = or [legal (Game g w c) (x, y) | y <- [0..w], x <- [0..w]]

placeCounter :: Game -> Coordinate -> Game
placeCounter Game { board = g, width = w, activePlayer = c } coord = Game (Seq.update (coordToIndex coord w) (Counter (flip c)) g) w c

updateBoard :: Game -> [Coordinate] -> Game
updateBoard game [] = game
updateBoard game ((x,y):cs) = updateBoard (placeCounter game (x,y)) cs

-- This returns Nothing if the move was not legal.
makeMove :: Game -> Coordinate -> Maybe Game
makeMove Game { board = g, width = w, activePlayer = c } (x,y)
  | null totalFlip = Nothing
  | otherwise = Just $ updateBoard (Game g w c) totalFlip
  where
    totalFlip :: [Coordinate]
    totalFlip = getIndiciesToFlip (Game g w c) (x,y)

winner :: Game -> Maybe Colour
winner Game { board = g, width = w, activePlayer = c }
  | blackCounter > whiteCounter = Just Black
  | whiteCounter > blackCounter = Just White
  | otherwise = Nothing
  where
    blackCounter = length $ filter (== Counter Black) spaces
    whiteCounter = length $ filter (== Counter White) spaces
    spaces = Data.Foldable.toList g
