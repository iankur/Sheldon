module PlayChess where

import System.Console.Haskeline
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

splits :: [Char] -> ([Char], [Char])
splits input = do
    let new = dropWhile isequal input
    let input1 = take 1 new
    let new2 = drop 1 new
    let input2 = takeWhile isnotequal new2
    let x = (input1, input2)
    fromJust (return x)

getInt :: [Char] -> Int
getInt str | str == "a" = 1
           | str == "b" = 2
           | str == "c" = 3
           | str == "d" = 4
           | str == "e" = 5
           | str == "f" = 6
           | str == "g" = 7
           | str == "h" = 8
           | otherwise = -1

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

humanVsDumb game = do
    if isKingUnderCheckmate game
    then do
        putStrLn ("\nDumb Bot Wins\n")
    else do
        putStr "Enter source column and row : "
        input1 <- getLine
        let source = splits input1
        if fst source <= "h" && fst source >= "a" && snd source <= "8" && snd source >= "1" then do
        let cfrom = getInt (fst source)
        let rfrom = (read (snd source) :: Int)
        putStr "Enter destination column and row : "
        input2 <- getLine
        let destination = splits input2
        if fst destination <= "h" && fst destination >= "a" && snd destination <= "8" && snd destination >= "1" then do
        let cto = getInt (fst destination)
        let rto = (read (snd destination) :: Int)
        putStr "Enter figure and color of the promotional move if any : "
        input3 <- getLine
        let figure = getFigure input3
        let g = move (Field cfrom rfrom) (Field cto rto) figure game
        putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
        case g of
            Nothing    -> do
                putStrLn "Invalid move, please try another move"
                humanVsDumb game
            Just ngame -> do
                putStrLn (show ngame)
                putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
                if isKingUnderCheckmate ngame
                then do
                    putStrLn ("\nYou Win\n")
                else do
                    putStrLn ("Dumb Bot Move\n")
                    let botmove = makeMove ngame
                    putStrLn (show (fromJust botmove))
                    putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
                    humanVsDumb (fromJust botmove)
        else do
            putStrLn "\nInvalid data, please try again\n"
            humanVsDumb game
        else do
             putStrLn "\nInvalid data, please try again\n"
             humanVsDumb game

humanVsSmart game = do
    if isKingUnderCheckmate game
    then do
        putStrLn "\nSmart Bot Wins\n"
    else do
        putStr "Enter source column and row : "
        input1 <- getLine
        let source = splits input1
        if fst source <= "h" && fst source >= "a" && snd source <= "8" && snd source >= "1" then do
        let cfrom = getInt (fst source)
        let rfrom = (read (snd source) :: Int)
        putStr "Enter destination column and row : "
        input2 <- getLine
        let destination = splits input2
        if fst destination <= "h" && fst destination >= "a" && snd destination <= "8" && snd destination >= "1" then do
        let cto = getInt (fst destination)
        let rto = (read (snd destination) :: Int)
        putStr "Enter figure and color of the promotional move if any : "
        input3 <- getLine
        let figure = getFigure input3
        let g = move (Field cfrom rfrom) (Field cto rto) figure game
        putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
        case g of
            Nothing    -> do
                putStrLn "Invalid move, please try another move"
                humanVsSmart game
            Just ngame -> do
                putStrLn (show ngame)
                putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
                if isKingUnderCheckmate ngame
                then do
                    putStrLn ("\nYou Win\n")
                else do
                    putStrLn ("SmartBot Move\n")
                    let botmove = makeSmartMove ngame
                    putStrLn (show (fromJust botmove))
                    putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
                    humanVsSmart (fromJust botmove)
        else do
            putStrLn "\nInvalid data, please try again\n"
            humanVsSmart game
        else do
            putStrLn "\nInvalid data, please try again\n"
            humanVsSmart game

humanVsAdvanced game = do
    if isKingUnderCheckmate game
    then do
        putStrLn "\nAdvanced Bot Wins\n"
    else do
        putStr "Enter source column and row : "
        input1 <- getLine
        let source = splits input1
        if fst source <= "h" && fst source >= "a" && snd source <= "8" && snd source >= "1" then do
        let cfrom = getInt (fst source)
        let rfrom = (read (snd source) :: Int)
        putStr "Enter destination column and row : "
        input2 <- getLine
        let destination = splits input2
        if fst destination <= "h" && fst destination >= "a" && snd destination <= "8" && snd destination >= "1" then do
        let cto = getInt (fst destination)
        let rto = (read (snd destination) :: Int)
        putStr "Enter figure and color of the promotional move if any : "
        input3 <- getLine
        let figure = getFigure input3
        let g = move (Field cfrom rfrom) (Field cto rto) figure game
        putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
        case g of
            Nothing    -> do
                putStrLn "Invalid move, please try another move"
                humanVsAdvanced game
            Just ngame -> do
                putStrLn (show ngame)
                putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
                if isKingUnderCheckmate ngame
                then do
                    putStrLn ("\nYou Win\n")
                else do
                    putStrLn ("AdvancedBot Move\n")
                    let botmove = makeCompromisedMove ngame
                    putStrLn (show (fromJust botmove))
                    putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
                    humanVsAdvanced (fromJust botmove)
        else do
            putStrLn "\nInvalid data, please try again\n"
            humanVsAdvanced game
        else do
            putStrLn "\nInvalid data, please try again\n"
            humanVsAdvanced game

humanVsHuman game = do
    if isKingUnderCheckmate game
    then do
        if gameColor game == White
        then do
            putStrLn ("\nPlayer2 Wins\n")
        else do
            putStrLn ("\nPlayer1 Wins\n")
    else do
        if gameColor game == White 
        then do
            putStrLn "Player1"
            putStrLn "--------"
        else do
            putStrLn "Player2"
            putStrLn "--------" 
        putStr "Enter source column and row : "
        input1 <- getLine
        let source = splits input1
        if fst source <= "h" && fst source >= "a" && snd source <= "8" && snd source >= "1" then do
        let cfrom = getInt (fst source)
        let rfrom = (read (snd source) :: Int)
        putStr "Enter destination column and row : "
        input2 <- getLine
        let destination = splits input2
        if fst destination <= "h" && fst destination >= "a" && snd destination <= "8" && snd destination >= "1" then do
        let cto = getInt (fst destination)
        let rto = (read (snd destination) :: Int)
        putStr "Enter figure and color of the promotional move if any : "
        input3 <- getLine
        let figure = getFigure input3
        let g = move (Field cfrom rfrom) (Field cto rto) figure game
        putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
        case g of
            Nothing    -> do
                putStrLn "Invalid move, please try another move"
                humanVsHuman game
            Just ngame -> do
                putStrLn (show ngame)
                putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
                humanVsHuman ngame
        else do
            putStrLn "\nInvalid data, please try again\n"
            humanVsHuman game
        else do
            putStrLn "\nInvalid data, please try again\n"
            humanVsHuman game

dumbVsDumb game = do
    if isKingUnderCheckmate game
    then do
        putStrLn ("\nBlack Dumb Bot Wins\n")
    else do
        putStr "White DumbBot Move\n"
        -- input1 <- getLine
        let bot1move = makeMove game
        putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
        putStrLn (show (fromJust bot1move))
        putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
        if isKingUnderCheckmate (fromJust bot1move)
        then do
            putStrLn ("\nWhite Dumb Bot Wins\n")
        else do
            putStr "Black DumbBot Move\n"
            -- input2 <- getLine
            let bot2move = makeMove (fromJust bot1move)
            putStrLn (show (fromJust bot2move))
            putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
            dumbVsDumb (fromJust bot2move)

dumbVsSmart game = do
    if isKingUnderCheckmate game
    then do
        putStrLn ("\nSmart Bot Wins\n")
    else do
        putStr "DumbBot Move\n"
        -- input1 <- getLine
        let bot1move = makeMove game
        putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
        putStrLn (show (fromJust bot1move))
        putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
        if isKingUnderCheckmate (fromJust bot1move)
        then do
            putStrLn ("\nDumb Bot Wins\n")
        else do
            putStr "SmartBot Move\n"
            -- input2 <- getLine
            let bot2move = makeSmartMove (fromJust bot1move)
            putStrLn (show (fromJust bot2move))
            putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
            dumbVsSmart (fromJust bot2move)

dumbVsAdvanced game = do
    if isKingUnderCheckmate game
    then do
        putStrLn ("\nAdvanced Bot Wins\n")
    else do
        putStr "DumbBot Move\n"
        -- input1 <- getLine
        let bot1move = makeMove game
        putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
        putStrLn (show (fromJust bot1move))
        putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
        if isKingUnderCheckmate (fromJust bot1move)
        then do
            putStrLn ("\nDumb Bot Wins\n")
        else do
            putStr "AdvancedBot Move\n"
            -- input2 <- getLine
            let bot2move = makeCompromisedMove (fromJust bot1move)
            putStrLn (show (fromJust bot2move))
            putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
            dumbVsAdvanced (fromJust bot2move)

smartVsSmart game = do
    if isKingUnderCheckmate game
    then do
        putStrLn ("\nBlack Smart Bot Wins\n")
    else do
        putStr "White SmartBot Move\n"
        -- inut1 <- getLine
        let bot1move = makeSmartMove game
        putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
        putStrLn (show (fromJust bot1move))
        putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
        if isKingUnderCheckmate (fromJust bot1move)
        then do
            putStrLn ("\nWhite Smart Bot Wins\n")
        else do
            putStr "Black SmartBot Move"
            -- input2 <- getLine
            let bot2move = makeSmartMove (fromJust bot1move)
            putStrLn (show (fromJust bot2move))
            putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
            smartVsSmart (fromJust bot2move)

smartVsAdvanced game = do
    if isKingUnderCheckmate game
    then do
        putStrLn ("\nAdvanced Bot Wins\n")
    else do
        putStr "SmartBot Move\n"
        -- input1 <- getLine
        let bot1move = makeSmartMove game
        putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
        putStrLn (show (fromJust bot1move))
        putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
        if isKingUnderCheckmate (fromJust bot1move)
        then do
            putStrLn ("\nSmart Bot Wins\n")
        else do
            putStr "AdvancedBot Move\n"
            -- input2 <- getLine
            let bot2move = makeCompromisedMove (fromJust bot1move)
            putStrLn (show (fromJust bot2move))
            putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
            smartVsAdvanced (fromJust bot2move)

advancedVsAdvanced game = do
    if isKingUnderCheckmate game
    then do
        putStrLn ("\nBlack Advanced Bot Wins\n")
    else do
        putStrLn ("White AdvancedBot Move\n")
        -- input1 <- getLine
        let bot1move = makeCompromisedMove game
        putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
        putStrLn (show (fromJust bot1move))
        putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
        if isKingUnderCheckmate (fromJust bot1move)
        then do
            putStrLn ("\nWhite Advanced Bot Wins\n")
        else do
            putStrLn ("Black AdvancedBot Move\n")
            -- input2 <- getLine
            let bot2move = makeCompromisedMove (fromJust bot1move)
            putStrLn (show (fromJust bot2move))
            putStrLn "------------------------------------------------------------------------------------------------------------------------------------------------------\n"
            advancedVsAdvanced (fromJust bot2move)

            
playChess = do
    putStrLn ("\nPress 1 for Human with DumbBot\nPress 2 for Human with SmartBot\nPress 3 for Human with AdvancedBot\nPress 4 for Human with Human\n"
              ++ "Press 5 for DumbBot with DumbBot\nPress 6 for DumbBot with SmartBot\nPress 7 for DumbBot with AdvancedBot\n"
              ++ "Press 8 for SmartBot with SmartBot\nPress 9 for SmartBot with AdvancedBot\n"
              ++ "Press 10 for AdvancedBot with AdvancedBot\n------------------------------\n")
    putStr "Enter option : "
    option <- getLine
    if option == "1" 
    then do
        putStrLn (show GameStart)
        humanVsDumb GameStart
    else if option == "2" 
    then do
        putStrLn (show GameStart)
        humanVsSmart GameStart
    else if option == "3" 
    then do
        putStrLn (show GameStart)
        humanVsAdvanced GameStart
    else if option == "4" 
    then do
        putStrLn (show GameStart)
        humanVsHuman GameStart
    else if option == "5" 
    then do
        putStrLn (show GameStart)
        dumbVsDumb GameStart
    else if option == "6"
    then do
        putStrLn (show GameStart)
        dumbVsSmart GameStart
    else if option == "7"
    then do
        putStrLn (show GameStart)
        dumbVsAdvanced GameStart
    else if option == "8"
    then do
        putStrLn (show GameStart)
        smartVsSmart GameStart
    else if option == "9"
    then do
        putStrLn (show GameStart)
        smartVsAdvanced GameStart
    else if option == "10"
    then do
        putStrLn (show GameStart)
        advancedVsAdvanced GameStart
    else do
        putStrLn "\nInvalid Option, Please try one of the above options\n"
        playChess