module Main where

import UCI

main = loop

loop :: IO()
loop = do
        line <- getLine
        uci = parseQuery line
        react uci
        loop

parseQuery :: String -> UciQuery
parseQuery "uci" = UciHello
parseQuery "isready" = UciReadyOk
