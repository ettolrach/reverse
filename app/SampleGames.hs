module SampleGames where

import Board (Colour (..), Space (..), Game (..), Grid (..))
import qualified Data.Sequence as Seq

emptyGame :: Game
emptyGame = Game {board = Seq.fromList $ emptyGrid 8, width = 8, activePlayer = Black}
  where
    emptyGrid :: Int -> [Space]
    emptyGrid 0 = []
    emptyGrid n = Empty : emptyGrid (n-1)

startingGame :: Game
startingGame = Game {board = startingGrid, width = 8, activePlayer = Black}
  where
    startingGrid :: Grid
    startingGrid =
      Seq.fromList
      [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty
      , Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty
      , Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty
      , Empty, Empty, Empty, Counter Black, Counter White, Empty, Empty, Empty
      , Empty, Empty, Empty, Counter White, Counter Black, Empty, Empty, Empty
      , Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty
      , Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty
      , Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
