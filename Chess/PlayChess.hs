module PlayChess where

import Data.Maybe (fromJust, fromMaybe, isNothing)
import Color
import Figure
import Field
import Move
import Board
import FigureMoves
import Game
import Rank
import ComputerPlayer

play game = do
    putStr "Enter source column : "
    input1 <- getLine
    let cfrom = (read input1 :: Int)
    putStr "Enter source row : "
    input2 <- getLine
    let rfrom = (read input2 :: Int)
    putStr "Enter destination column : "
    input3 <- getLine
    let cto = (read input3 :: Int)
    putStr "Enter destination row : "
    input4 <- getLine
    let rto = (read input4 :: Int)
    let g = move (Field cfrom rfrom) (Field cto rto) Nothing game
    putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
    printBoard (gameBoard (fromJust g))
    putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
    let botmove = makeMove (fromJust g)
    printBoard (gameBoard (fromJust botmove))
    putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
    play (fromJust botmove)

playChess = play GameStart