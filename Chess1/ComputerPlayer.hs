module ComputerPlayer where

import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.List (sortBy)
import Color
import Figure
import Field
import Move
import FigureMoves
import Game
import Rank

root :: Tree a -> a
root (Node x ts) = x

top :: Int -> [Game] -> [Game]
top k games = let
               rankedGames = map (\g -> (g, rank g (gameColor g))) games
               rankedGamesSorted = sortBy (\(_,rank1) (_,rank2) -> compare rank1 rank2) rankedGames
               topKGames = take k rankedGamesSorted
              in map fst topKGames

prunedReptree k f x = Node x (map (prunedReptree k f) (top k (f x)))

smallGameTree :: Int -> Game -> Tree Game
smallGameTree k game = prunedReptree k validGames game

prune :: Int -> Tree a -> Tree a
prune 0 (Node n sub) = Node n []
prune k (Node n sub) = Node n (map (prune (k-1)) sub)

size :: Tree a -> Int
size (Node root []) = 1
size (Node root subtrees) = 1 + sum (map size subtrees)
 
leaves :: Tree a -> [a]
leaves (Node root []) = [root]
leaves (Node root trees) = concat [leaves tree | tree <- trees]

maxrank :: [Game] -> Int
maxrank games = maximum [rank game (gameColor game) | game <- games]

giverank :: Tree Game -> [(Game, Int)]
giverank (Node game []) = [(game, rank game (gameColor game))]
giverank (Node game subtrees) = zip (map root subtrees) (map maxrank (map leaves subtrees))

printrank :: Tree Game -> [Int]
printrank tree = [snd x | x <- giverank tree]

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

-- | Returns a sequence of the best ranked moves.
compromisedmoves :: Game -> [Game]
compromisedmoves game =
  let rankedMoves = giverank (prune 5 (smallGameTree 3 game))
  in if null rankedMoves
     then []
     else let
            -- rankedMoves = map (\g -> (g, rank g (gameColor g))) moves
            rankedMovesSorted = sortBy (\(_,rank1) (_,rank2) -> compare rank1 rank2) rankedMoves
            firstRank = snd . head $ rankedMovesSorted
            maxRankMoves = takeWhile (\(_,rank) -> rank == firstRank) rankedMovesSorted
          in map fst maxRankMoves

-- | Makes a move and returns the next game state.
makeMove :: Game -> Maybe Game
makeMove game = case moves game of
  [] -> Nothing
  h:_ -> Just h

makeSmartMove :: Game -> Maybe Game
makeSmartMove game = case smartmoves game of
  [] -> Nothing
  h:_ -> Just h

makeCompromisedMove :: Game -> Maybe Game
makeCompromisedMove game = case compromisedmoves game of
    [] -> Nothing
    h:_ -> Just h

{-

ghci
:load Chess/ComputerPlayer.hs
let Just g1 = move (Field 7 2) (Field 7 4) Nothing GameStart
let Just g2 = move (Field 5 7) (Field 5 6) Nothing g1
let Just g3 = move (Field 6 2) (Field 6 4) Nothing g2
let Just g4 = move (Field 4 8) (Field 8 4) Nothing g3
length $ moves GameStart -- 2
length $ moves g1 -- 2
length $ moves g2 -- 2
length $ moves g3 -- 1
length $ moves g4 -- 0
let Just g1 = makeMove GameStart
let Just g2 = makeMove g1
let Just g3 = makeMove g2
let Just g4 = makeMove g3
g4
:q

-}
  
