module SmartPlayer where

import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.List (sortBy, maximumBy)
import Properties
import FigureMoves
import Game
import Rank

-- | Returns a sequence of the best ranked moves.
smartmoves :: Game -> [Game]
smartmoves game =
  let rankedMoves = giverank (prune 3 (gameTree game))
  in if null rankedMoves
     then []
     else let
            -- rankedMoves = map (\g -> (g, rank g (gameColor g))) moves
            rankedMovesSorted = sortBy (\(_,rank1) (_,rank2) -> compare rank1 rank2) rankedMoves
            firstRank = snd . head $ rankedMovesSorted
            maxRankMoves = takeWhile (\(_,rank) -> rank == firstRank) rankedMovesSorted
          in map fst maxRankMoves

makeSmartMove :: Game -> Maybe Game
makeSmartMove game = case smartmoves game of
  [] -> Nothing
  h:_ -> Just h