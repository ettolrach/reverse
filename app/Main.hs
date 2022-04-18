{-
Program: reverse
Available at: https://github.com/ettolrach/reverse

Copyright 2022 Charlotte Ausel

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. -}
module Main where

import Board
import SampleGames ( startingGame )
import Prelude hiding (flip)
import Text.Read ( readMaybe )
import Data.Maybe (isNothing)

printPrompt :: Game -> IO ()
printPrompt Game { board = g, width = w, activePlayer = c } =
    putStrLn $ gridString (Game g w c) ++ "\n" ++ colourStr c ++ ", please enter a coodinate (use the format \"x,y\"):"

printWinner :: Maybe Colour -> IO ()
printWinner (Just Black) = putStrLn "Black has won!"
printWinner (Just White) = putStrLn "White has won!"
printWinner Nothing = putStrLn "It is a draw."

strToCoord :: String -> Maybe Coordinate
strToCoord s = case (x,y) of
    (Nothing, Nothing) -> Nothing
    (Just _, Nothing) -> Nothing
    (Nothing, Just _) -> Nothing
    (Just x, Just y) -> Just (x,y)
  where
    (x,y) = case comma of
      Nothing -> (Nothing, Nothing)
      Just i -> (readMaybe (take i s) :: Maybe Int, readMaybe (drop (i + 1) s) :: Maybe Int)
    comma = findComma s
    findComma :: String -> Maybe Int
    findComma str = findCommaFrom str 0
    findCommaFrom :: String -> Int -> Maybe Int
    findCommaFrom [] _ = Nothing
    findCommaFrom (x:xs) n
      | x == ',' = Just n
      | otherwise = findCommaFrom xs (n + 1)

loop :: Game -> IO ()
loop Game {board = g, width = w, activePlayer = c}
  | movesAvailable (Game g w c) = do
    printPrompt (Game g w c)
    coordStr <- getLine
    let coord = strToCoord coordStr
    case coord of
        Nothing -> putStrLn "That is an invalid coordinate." >> loop (Game g w c)
        Just (x,y) -> case makeMove (Game g w c) (x, y) of
            Nothing -> putStrLn "That move is not legal." >> loop (Game g w c)
            Just newGame -> loop newGame
  | movesAvailable (Game g w (flip c)) = putStrLn $ colourStr c ++ " can't move, so it's " ++ (colourStrLower . flip) c ++ "'s turn."
  | otherwise = putStrLn "The winner is " >> printWinner (winner (Game g w c))

main :: IO ()
main = do
    let game = startingGame
    loop game
    putStrLn "Hello, Haskell!"
