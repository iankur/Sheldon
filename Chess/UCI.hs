module UCI where

import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as Map
import Data.Char
import Data.List
import Data.List.Split
import Move
import Field
import Game
import Board
import Figure
import Color
import ComputerPlayer
--import SmartPlayer
--import AdvancedPlayer

-- Commands to engine from interface
data UciQuery = UciHello
              | UciIsReady
              | UciNewGame
              | UciPosition UciPos -- [UciMove]
              | UciGo [String]--SearchOption
              | UciStop
              | UciQuit
              | UciInvalid

-- UCI positions
data UciPos = UciStartPos [String]
            | UciFen String String String String String String

data UciMove = UciRegularMove
             | UciPromotionMove
             | UciEnPassantMove
             | UciCastlingMove

-- Commands to interface from engine
data UciResponse = RespId String String
                 | RespUciOk
                 | RespReadyOk
                 | RespBestMove Move
                 | RespInfo String
                 | RespOption String

-- handler for commands to engine
uciHello = UciHello
uciIsReady = UciIsReady
uciNewGame = UciNewGame
uciPosition pos = case head pos of
                    "startpos" -> UciPosition (UciStartPos (tail pos))
                    "fen" -> UciPosition (UciFen (pos!!1) (pos!!2) (pos!!3) (pos!!4) (pos!!5) (pos!!6))
                    otherwise -> UciInvalid

uciGo options = UciGo options
uciStop = UciStop
uciQuit = UciQuit
uciInvalid = UciInvalid

react :: UciQuery -> Game -> (Game, String)
react UciHello game = let s1 = "id name Sheldon 0.1\n"
                          s2 = "id author Team Sheldon\n"
                          s3 = "uciok"
                      in (game, s1++s2++s3)

react UciIsReady game = let s = "readyok"
                        in (game, s)

react UciNewGame _ = (GameStart, "")

--react (UciPosition (UciStartPos _)) game = (game,"")
react (UciPosition (UciStartPos [])) game = (game, "")
react (UciPosition (UciStartPos moves)) game = let lastmove = convertMove (last moves) game
                                                   refmove = findMove game
                                               in if lastmove == refmove then (game, "") else (applyMove lastmove game, show lastmove)

react (UciPosition (UciFen p1 p2 p3 p4 p5 p6)) game = let g = construct p1 p2 game
                                                      in (addMove game g, "")

react (UciGo _) game = let s = "bestmove "
                           newGame = fromJust (makeMove game)
                           OngoingGame _ _ _ move = newGame
                           s2 = s ++ (show $ Move.from move) ++ (show $ Move.to move)
                       in (newGame, s2)

react UciStop game = (game, "")
react UciQuit _ = (NullGame, "")
react UciInvalid game = (game, "info invalid move")

-- finds last move from Game state
findMove :: Game -> Move
findMove (OngoingGame _ _ _ lastMove) = lastMove
findMove _ = NullMove

-- Converts input string of type 'e2e4' to move
convertMove :: String -> Game -> Move
convertMove move game | len == 4 = normalMove move game
                      | len == 5 = promotionMove move game
                      | otherwise = NullMove
                      where len = length move

-- Normal move of type aibj
normalMove move game = let from = makeField (take 2 move)
                           to = makeField (drop 2 move)
                           (castling, rookFrom, rookTo) = checkCastlingMove from to game
                       in case castling of
                            True -> CastlingMove from to rookFrom rookTo
                            False -> case (checkEnPassantMove from to game) of
                                        True -> EnPassantMove from to to
                                        False -> RegularMove from to
                                    
-- returns promotional move for given string and game state
--promotionMove move game = NullMove
promotionMove move game = let from = makeField (take 2 move)
                              to = makeField (take 2 (drop 2 move))
                              figure = Figure (piece (last move)) (gameColor game)
                          in PromotionMove from to figure

-- makes Field from given string like 'a2'
makeField [col, row] = Field (ord col - (ord 'a') + 1) (ord row - (ord '0'))

-- checks if CastlingMove or not
checkCastlingMove :: Field -> Field -> Game -> (Bool, Field, Field)
checkCastlingMove from to game = let Just (Figure piece _) = Map.lookup from (gameBoard game)
                                     Field fromCol fromRow = from
                                     Field toCol toRow = to
                                     refRight = Field (fromCol+2) fromRow
                                     refLeft = Field (fromCol-2) fromRow
                                     rookRight = Field (toCol-1) toRow
                                     rookLeft = Field (toCol+1) toRow
                                 in if piece == King
                                    then if to == refRight
                                         then (True, Field 8 fromRow, rookRight)
                                         else if to == refLeft
                                              then (True, Field 1 fromRow, rookLeft)
                                              else (False, from, to)
                                    else (False, from, to)

-- [TODO] checks if EnPassantMove or not
checkEnPassantMove :: Field -> Field -> Game -> Bool
checkEnPassantMove from to game = False

-- Takes this move from current state of game to return new game
applyMove :: Move -> Game -> Game
applyMove move game = let clr = other (gameColor game)
                          hist = game:(gameHist game)
                          newBoard = updateBoard (gameBoard game) move
                      in (OngoingGame clr newBoard hist move)

-- Constructs current game state using fen and color string
construct :: String -> String -> Game -> Game
construct fen color game = OngoingGame clr (fen2Board fen) hist NullMove
                           where hist = gameHist game
                                 clr = if color == "b" then Black else White

-- Creates board from give fen string
fen2Board :: String -> Board
fen2Board s = let rows = splitOn "/" (expand s)
              in Map.fromList [(Field c r, fig col) | (r,row) <- zip (reverse [1..(length rows)]) rows, (c,col) <- zip [1..(length row)] row, col /= '1']

-- Expands a digit 'n' into string of ones of length n
expand :: String -> String
expand [] = []
expand (x:xs) | isDigit x = (replicate (ord x - ord '0') '1') ++ (expand xs)
              | otherwise =  x:(expand xs)

fig :: Char -> Figure
fig symbol = let val = ord symbol
             in if val >= 97
                then Figure (piece symbol) Black
                else Figure (piece symbol) White

piece :: Char -> FigureType
piece symbol = case toLower symbol of
                'p' -> Pawn
                'n' -> Knight
                'b' -> Bishop
                'r' -> Rook
                'q' -> Queen
                'k' -> King

addMove :: Game -> Game -> Game
addMove lastGame curGame = let lastBoard = gameBoard lastGame
                               curBoard = gameBoard curGame
                               move = getMove lastBoard curBoard
                               clr = gameColor curGame
                               hist = gameHist curGame
                            in (OngoingGame clr curBoard hist move)

-- Gets last move played by examining stored game state and game created by fen
getMove :: Board -> Board -> Move
getMove lastBoard curBoard = let (from,fromFig) = head (Map.toList (Map.difference lastBoard curBoard))
                                 to = head [t | (t,fig) <-  (Map.toList curBoard \\ (Map.toList lastBoard)), (fig == fromFig)]
                             in RegularMove from to
