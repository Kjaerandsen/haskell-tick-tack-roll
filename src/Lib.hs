module Lib
    ( printBoard
    ) where

import Roll

printBoard :: [Int] -> IO()
printBoard arr = do
    let len = lengthCheck arr
    if len /= 0 then do
        printLineByLine len arr
    else do
        putStrLn "Error: unable to print board due to invalid board size"

-- 
printLineByLine :: Int -> [Int] -> IO()
printLineByLine len arr = do
    if length arr >= len then do
        putStr "# "
        printLine (take len arr)
        printLineByLine len (drop len arr)
    else
        return ()

printLine :: [Int] -> IO()
printLine i = do
    if (length i) > 0 then do
        putStr (show (head i))
        putChar ' '
        printLine (tail i)
    else
        putStr "\n"