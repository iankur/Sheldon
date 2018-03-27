module UCI where

import Move

-- Commands to engine from interface
data UciQuery = UciHello
              | UciIsReady
              | UciNewGame
              | UciPosition Position
              | UciGo SearchOption
              | UciStop
              | UciQuit

-- Commands to interface from engine
data UciResponse = RespId String String
                 | RespUciOk
                 | RespReadyOk
                 | RespBestMove Move
                 | RespInfo String
                 | RespOption String

react :: UciQuery -> IO()
react UciHello = do
                    putStrLn "id name Sheldon 0.1"
                    putStrLn "id author Team Sheldon"
                    putStrLn "uciok"

react UciIsReady = putStrLn "readyok"
react UciNewGame = ""
react UciPosition position = ""
react UciGo searchOption = ""
react UciStop = ""
react UciQuit = ""
