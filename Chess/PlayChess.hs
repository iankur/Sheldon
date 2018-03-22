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

isequal :: Char -> Bool
isequal ch | ch == ' ' = True
           | otherwise = False 

isnotequal :: Char -> Bool
isnotequal ch | ch == ' ' = False
              | otherwise = True

split :: [Char] -> ([Char], [Char])
split input = do
    let new = dropWhile isequal input
    let input1 = takeWhile isnotequal new
    let new1 = dropWhile isnotequal new
    let new2 = dropWhile isequal new1
    let input2 = takeWhile isnotequal new2
    let x = (input1,input2)
    fromJust (return x)

getFigure :: [Char] -> Maybe Figure
getFigure str | fst figure == "Rook" && snd figure == "White"   = Just (Figure Rook White)
              | fst figure == "Knight" && snd figure == "White" = Just (Figure Knight White)
              | fst figure == "Bishop" && snd figure == "White" = Just (Figure Bishop White)
              | fst figure == "Queen" && snd figure == "White"  = Just (Figure Queen White)
              | fst figure == "King" && snd figure == "White"   = Just (Figure King White)
              | fst figure == "Pawn" && snd figure == "White"   = Just (Figure Pawn White)
              | fst figure == "Rook" && snd figure == "Black"   = Just (Figure Rook Black)
              | fst figure == "Knight" && snd figure == "Black" = Just (Figure Knight Black)
              | fst figure == "Bishop" && snd figure == "Black" = Just (Figure Bishop Black)
              | fst figure == "Queen" && snd figure == "Black"  = Just (Figure Queen Black)
              | fst figure == "King" && snd figure == "Black"   = Just (Figure King Black)
              | fst figure == "Pawn" && snd figure == "Black"   = Just (Figure Pawn Black)
              | otherwise                                       = Nothing
              where figure = split str

play1 game = do
    putStr "Enter source column and row : "
    input1 <- getLine
    let source = split input1
    let cfrom = (read (fst source) :: Int)
    let rfrom = (read (snd source) :: Int)
    putStr "Enter destination column and row : "
    input2 <- getLine
    let destination = split input2
    let cto = (read (fst destination) :: Int)
    let rto = (read (snd destination) :: Int)
    putStr "Enter figure and color of the promotional move if any : "
    input3 <- getLine
    let figure = getFigure input3
    let g = move (Field cfrom rfrom) (Field cto rto) figure game
    putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
    case g of
        Nothing    -> do
            putStrLn "Invalid move, please try another move"
            play1 game
        Just ngame -> do
            printBoard (gameBoard ngame)
            putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
            let botmove = makeMove ngame
            printBoard (gameBoard (fromJust botmove))
            putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
            play1 (fromJust botmove)

playChess = play1 GameStart