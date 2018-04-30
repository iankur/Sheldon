module Rank where

import qualified Data.Map as Map
import Data.List (sort)
import Properties
import FigureMoves
import Game

-- | Returns the rank of a figure of the given type.
figureRank :: Figure -> Int
figureRank f | figureType f == Queen  = 900
             | figureType f == Rook   = 450
             | figureType f == Knight = 300
             | figureType f == Bishop = 300
             | figureType f == Pawn   = 100
             | otherwise              = 0


-- |  Returns the rank of the given field.
fieldRank :: Field -> Int
fieldRank (Field col row) = 2 * (min col (9-col))  * (min row (9-row))

-- | Returns the figure rank based on the figures it is defending.
figureDefendingOtherFiguresRank :: Game -> Field -> Figure -> Int
figureDefendingOtherFiguresRank game field figure = (length $ defendedDestinations game (figureMoves figure field True)) `div` 2

-- | Returns a rank value related to whether the King is under check or not.
checkRank :: Game -> Color -> Int
checkRank game color = if (gameColor game == other color) && isKingUnderCheck game then 50 else 0

-- | Calculates the position rank taking one color into account.
colorRank :: Game -> Color -> Int
colorRank game color =
  let ranks = [ r1+r2+r3 |
                (field,figure) <- Map.toList $ gameBoard game,
                figureColor figure == color,
                let r1 = figureRank figure,
                let r2 = fieldRank field,
                let r3 = figureDefendingOtherFiguresRank game field figure ]
  in sum ranks + checkRank game color

-- | Calculates the position rank from the point of view of a player.
rank :: Game -> Color -> Int
rank game color = colorRank game color - colorRank game (other color)

instance Eq Game where
  (==) game1 game2 = rank game1 (gameColor game1) == rank game2 (gameColor game2)
  (/=) game1 game2 = rank game1 (gameColor game1) /= rank game2 (gameColor game2)

instance Ord Game where
  compare game1 game2 = rank game1 (gameColor game1) `compare` rank game2 (gameColor game2)
  (<) game1 game2     = rank game1 (gameColor game1) < rank game2 (gameColor game2)
  (<=) game1 game2    = rank game1 (gameColor game1) <= rank game2 (gameColor game2)
  (>) game1 game2     = rank game1 (gameColor game1) > rank game2 (gameColor game2)
  (>=) game1 game2    = rank game1 (gameColor game1) >= rank game2 (gameColor game2)
  max game1 game2     | rank game1 (gameColor game1) > rank game2 (gameColor game2) = game1
                      | otherwise                                                   = game2
  min game1 game2     | rank game1 (gameColor game1) < rank game2 (gameColor game2) = game1
                      | otherwise                                                   = game2

top :: Int -> [Game] -> [Game]
top k games = take k (sort games)

prunedReptree k f x = Node x (map (prunedReptree k f) (top k (f x)))

smallGameTree :: Int -> Game -> Tree Game
smallGameTree k game = prunedReptree k validGames game

maxrank :: [Game] -> Int
maxrank games = maximum [rank game (gameColor game) | game <- games]

giverank :: Tree Game -> [(Game, Int)]
giverank (Node game []) = [(game, rank game (gameColor game))]
giverank (Node game subtrees) = zip (map root subtrees) (map maxrank (map leaves subtrees))
