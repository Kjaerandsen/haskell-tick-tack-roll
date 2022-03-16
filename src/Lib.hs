module Lib
    ( printBoard
    ) where

import Roll -- For checking if the board size is valid

-- | printBoard takes a board and prints it to the screen, error message if the board is invalid
-- uses the helper function printLineByLine to print each line of the array
printBoard :: [Int] -> IO()
printBoard arr = do
    let len = lengthCheck arr
    if len /= 0 then do
        printLineByLine len arr
    else do
        putStrLn "Error: unable to print board due to invalid board size"

-- | printLineByLine takes a line length and a line list and prints the list line by line
-- uses the printLine helper function to print each line
printLineByLine :: Int -> [Int] -> IO()
printLineByLine len arr = do
    if length arr >= len then do
        putStr "# "
        printLine (take len arr)
        printLineByLine len (drop len arr)
    else
        return ()

-- | printLine takes an array of integers and prints them one by one separated by a single space
-- when done also print a line break
printLine :: [Int] -> IO()
printLine i = do
    if (length i) > 0 then do
        putStr (show (head i))
        putChar ' '
        printLine (tail i)
    else
        putStr "\n"