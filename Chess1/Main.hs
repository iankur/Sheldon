module Main where

import UCI
import Game
import ComputerPlayer
import Board
import Move
import Color
import System.IO

main = loop NullGame

loop :: Game -> IO()
loop game = do line <- getLine
               let uci = parseQuery (words line)
               let (g, msg) = react uci game
               printMsg msg
--             returnMsg (head (words line)) msg
--             printGame g (head (words line))
               loop g

parseQuery :: [String] -> UciQuery
parseQuery query = case (head query) of
                        "uci" -> uciHello
                        "isready" -> uciIsReady
                        "ucinewgame" -> uciNewGame
                        "position" -> uciPosition (tail query)
                        "go" -> uciGo (tail query)
                        "stop" -> uciStop
                        "quit" -> uciQuit
                        _ -> uciInvalid

printMsg :: String -> IO()
printMsg msg | msg == "" = return ()
             | otherwise = do
                            putStrLn msg
                            hFlush stdout

printGame :: Game -> String -> IO()
printGame NullGame _ = return ()
printGame game command = case command of
                            "go" -> do
                                        printMove game
                                        printBoard (gameBoard game)
                            "position" -> do
                                            printMove game
                                            printBoard (gameBoard game)
                            _    -> return ()

printMove :: Game -> IO()
printMove GameStart = return ()
printMove (OngoingGame color _ _ lastMove) = putStrLn ("Last move: " ++ (show $ other color) ++
                                                " " ++ (show $ Move.from lastMove) ++ " to " ++
                                                (show $ Move.to lastMove))
