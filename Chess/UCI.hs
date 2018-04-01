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

react (UciPosition (UciStartPos _)) game = (game,"")
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

construct :: String -> String -> Game -> Game
construct fen color game = OngoingGame clr (fen2Board fen) hist NullMove
                           where hist = gameHist game
                                 clr = if color == "b" then Black else White

fen2Board :: String -> Board
fen2Board s = let rows = splitOn "/" (expand s)
              in Map.fromList [(Field c r, fig col) | (r,row) <- zip (reverse [1..(length rows)]) rows, (c,col) <- zip [1..(length row)] row, col /= '1']

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

getMove :: Board -> Board -> Move
getMove lastBoard curBoard = let (from,fromFig) = head (Map.toList (Map.difference lastBoard curBoard))
                                 to = head [t | (t,fig) <-  (Map.toList curBoard \\ (Map.toList lastBoard)), (fig == fromFig)]
                             in RegularMove from to
