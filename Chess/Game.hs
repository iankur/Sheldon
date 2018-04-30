module Game where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.List (find, sort, group)
import FigureMoves
import Properties
import Board

data Game = NullGame | GameStart | OngoingGame Color Board [Game] Move

data Tree a = Node a [Tree a] deriving (Show, Eq)

root :: Tree a -> a
root (Node x ts) = x

leaves :: Tree a -> [a]
leaves (Node root []) = [root]
leaves (Node root trees) = concat [leaves tree | tree <- trees]

size :: Tree a -> Int
size (Node root []) = 1
size (Node root subtrees) = 1 + sum (map size subtrees)

instance Show Game where
  show NullGame = "Just an instance of game"
  show GameStart = "White to begin:\n" ++ showBoard startingBoard
  show (OngoingGame color board _ lastMove) =
    (showBoard board) ++ "Last move: " ++ (show $ other color) ++ " " ++
    (show $ Properties.from lastMove) ++ " to " ++
    (show $ Properties.to lastMove) ++ "\n"

gameColor NullGame = White
gameColor GameStart = White
gameColor (OngoingGame color _ _ _) = color

gameHist NullGame = []
gameHist GameStart = []
gameHist (OngoingGame _ _ hist _) = hist

gameBoard NullGame = startingBoard
gameBoard GameStart = startingBoard
gameBoard (OngoingGame _ board _ _) = board

-- | Verifies if the given field is empty.
isFieldEmpty game field = not (Map.member field (gameBoard game))

-- | Returns free fields onto which the figure may be moved.
freeDestinations :: Game -> [[Field]] -> [Field]
freeDestinations game fieldss = concat (map (\fields -> takeWhile (isFieldEmpty game) fields) fieldss)

-- | Returns fields occupied by the enemy figures
-- (including the case when that figure is the King)
-- onto which the figure may be moved.
captureDestinations :: Game -> [[Field]] -> [Field]
captureDestinations game fieldss = concat (map (filter hasEnemyFigure . take 1 . dropWhile (isFieldEmpty game)) fieldss)
  where hasEnemyFigure field = figureColor (fromJust (Map.lookup field (gameBoard game))) == other (gameColor game)

-- | Returns fields occupied by the own figures
-- (including the case when that figure is the King)
-- onto which the figure may be moved.
defendedDestinations :: Game -> [[Field]] -> [Field]
defendedDestinations game fieldss = concat (map (filter hasSameColorFigure . take 1 . dropWhile (isFieldEmpty game)) fieldss)
  where hasSameColorFigure field = figureColor (fromJust (Map.lookup field (gameBoard game))) == (gameColor game)

-- | Returns a new game, updated with a move.
updateGame :: Game -> Move -> Game
updateGame game move =
  OngoingGame (other (gameColor game))
              (updateBoard (gameBoard game) move)
              (game:(gameHist game))
              move

-- | Verifies if the enemy King is under check.
-- | Map.elems gives a list of all the value elements of any map
isOtherKingUnderCheck game = not $ and (map isKingOnBoard (nextGames game))
  where isKingOnBoard g = or (map (== Figure King (other $ gameColor game)) (Map.elems (gameBoard g)))

-- | Verifies if the King of the player who is about to make a move is under check.
isKingUnderCheck :: Game -> Bool
isKingUnderCheck game =
  let newGame = OngoingGame (other $ gameColor game)
                            (gameBoard game)
                            (game:(gameHist game))
                            (RegularMove (Field 0 0) (Field 0 0))
  in isOtherKingUnderCheck newGame

-- | Verifies if the King of the player who is about to make a move is under Checkmate
isKingUnderCheckmate :: Game -> Bool
isKingUnderCheckmate game = isKingUnderCheck game && not (elem False (map isOtherKingUnderCheck (validGames game)))

castling game kingTo rookFrom rookTo otherCol =
  let color = gameColor game
      row = firstRow color
      hist = gameHist game
      board = gameBoard game
  in if Map.lookup (Field 5 row) board == Just (Figure King color) &&
        Map.lookup (Field rookFrom row) board == Just (Figure Rook color) &&
        Map.lookup (Field rookTo row) board == Nothing &&
        Map.lookup (Field kingTo row) board == Nothing &&
        Map.lookup (Field otherCol row) board == Nothing &&
        all (\g -> Map.lookup (Field 5 row) (gameBoard g) == Just (Figure King color)) hist &&
        all (\g -> Map.lookup (Field rookFrom row) (gameBoard g) == Just (Figure Rook color)) hist &&
        not (isOtherKingUnderCheck (updateGame game (RegularMove (Field 5 row) (Field rookTo row))))
     then [updateGame game (CastlingMove (Field 5 row) (Field kingTo row) (Field rookFrom row) (Field rookTo row))]
     else []

nextGames :: Game -> [Game]
nextGames game =
  [ nextGame |
    (from, figure) <- Map.toList (gameBoard game),
    figureColor figure == gameColor game,
    nextGame <- case figure of
      Figure fig _ | fig == Rook || fig == Bishop  ||
                     fig == Queen || fig == Knight || fig == King ->
                       (let fieldss = figureMoves figure from True
                        in map (\to -> updateGame game (RegularMove from to)) $
                           freeDestinations game fieldss ++ captureDestinations game fieldss) ++
                       (if fig == King
                        then castling game 3 1 4 2 ++ castling game 7 8 6 7
                        else [])
      Figure Pawn _ ->
        let regularAndPromotionMoves =
              concat [ freeDestinations game (figureMoves figure from False)
                     , captureDestinations game (figureMoves figure from True) ] >>=
                  \to -> if isLastRow to (gameColor game)
                         then map (\figureType -> updateGame game $ PromotionMove from to $
                                                  Figure figureType $ gameColor game)
                              [ Queen, Rook, Bishop, Knight ]
                         else [ updateGame game (RegularMove from to) ]
        in regularAndPromotionMoves]

-- | Filters out the next games in which the king is under check.
validGames :: Game -> [Game]
validGames game = filter (not . isOtherKingUnderCheck) $ nextGames game

reptree f x = Node x (map (reptree f) (f x))

-- | Creates a gametree of validgames
gameTree :: Game -> Tree Game
gameTree game = reptree validGames game

prune :: Int -> Tree a -> Tree a
prune 0 (Node n sub) = Node n []
prune k (Node n sub) = Node n (map (prune (k-1)) sub)
                                                                            
-- | Returns a new game state after moving a figure. If the given
-- move is not possible, it returns Nothing.
move :: Field -> Field -> Maybe Figure -> Game -> Maybe Game
move from to promotion game =
  find (const True) $ filter isMatching $ validGames game
  where isMatching (OngoingGame _ _ _ move) =
          case move of
            RegularMove f t -> f == from && t == to && isNothing promotion
            PromotionMove f t fig -> f == from && t == to && promotion == Just fig
            CastlingMove f t _ _ ->  f == from && t == to && isNothing promotion
