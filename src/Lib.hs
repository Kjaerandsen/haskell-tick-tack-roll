module Lib
    ( printBoard,
      winCheck
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

-- | winCheck takes a grid, determines the grid size and player pieces, returns the winner
-- or blank if no winner in the current board state
winCheck :: [Char] -> Char
winCheck a = do
    -- get the line length
    let len = lengthCheck a
    if len /= 0 then do
        -- make a list of the left diagonal and check the item count of X and O
        
        -- make a list of the right diagonal and check the item count of X and O

        -- check line by line
        let result = winCheckHorizontal len a -- Check horizontal lines
        if result /= '_' then
            result 
        else do -- Check vertical lines
            let result = winCheckHorizontal len (rollHelper len True a)
            result
    else -- Default to blank output if invalid input
        '_'

-- >>> winCheckHorizontal 3 ['X','X','X','O','X','O','X','_','X']
-- 'X'
--
-- >>> winCheckHorizontal 3 ['O','O','O','X','O','X','X','_','X']
-- 'O'
--

-- | winCheckHorizontal takes a grid size and a grid, returns the winner or '_', for horizontal wins
-- recurses until empty, if no winner before empty then return '_' 
winCheckHorizontal :: Int -> [Char] -> Char
winCheckHorizontal x a = do
    -- Take a line
    let line = take x a
    let winner = membersInListHelper x line
    if winner /= '_' || length a == x  then
        winner
    else
        winCheckHorizontal x (drop x a)

--winCheckDiagonal :: Bool -> [Char] -> Char


-- | membersInListhelper, helper function for membersInList, 
-- calls it with both characters and returns a victor if there is one
membersInListHelper :: Int -> [Char] -> Char
membersInListHelper x a = do
    if membersInList 'X' a == x then
        'X'
    else if membersInList 'O' a == x then
        'O'
    else
        '_'
    

-- | membersInList, uses list comprehension to go through each element of the input list, comparing it to
-- the signature and returning the number of occurrences
-- same as mySignature function from haskell-01
membersInList :: Eq a => a -> [a] -> Int
membersInList z xs = sum [if z == x then 1 else 0 | x <- xs]