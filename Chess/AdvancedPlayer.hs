module AdvancedPlayer where

import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.List (sortBy, minimumBy)
import Color
import Figure
import Field
import Move
import FigureMoves
import Game
import Rank

-- | Returns a sequence of the best ranked moves.
compromisedmoves :: Game -> [Game]
compromisedmoves game =
  let rankedMoves = giverank (prune 3 (smallGameTree 3 game))
  in if null rankedMoves
     then []
     else let
            -- rankedMoves = map (\g -> (g, rank g (gameColor g))) moves
            rankedMovesSorted = sortBy (\(_,rank1) (_,rank2) -> compare rank1 rank2) rankedMoves
            firstRank = snd . head $ rankedMovesSorted
            maxRankMoves = takeWhile (\(_,rank) -> rank == firstRank) rankedMovesSorted
          in map fst maxRankMoves

makeCompromisedMove :: Game -> Maybe Game
makeCompromisedMove game = case compromisedmoves game of
    [] -> Nothing
    h:_ -> Just h