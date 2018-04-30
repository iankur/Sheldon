module DumbPlayer where

import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.List (sortBy)
import Properties
import FigureMoves
import Game
import Rank

-- | Returns a sequence of the best ranked moves.
moves :: Game -> [Game]
moves game =
  let moves = validGames game
  in if null moves
     then []
     else let
            rankedMoves = map (\g -> (g, rank g (gameColor g))) moves
            rankedMovesSorted = sortBy (\(_,rank1) (_,rank2) -> compare rank1 rank2) rankedMoves
            firstRank = snd . head $ rankedMovesSorted
            maxRankMoves = takeWhile (\(_,rank) -> rank == firstRank) rankedMovesSorted
          in map fst maxRankMoves

-- | Makes a move and returns the next game state.
makeMove :: Game -> Maybe Game
makeMove game = case moves game of
  [] -> Nothing
  h:_ -> Just h