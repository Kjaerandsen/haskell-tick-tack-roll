module Lib
    ( printBoard,
      winCheck,
      membersInList
    ) where

import Roll -- For checking if the board size is valid

-- | printBoard takes a board and prints it to the screen, error message if the board is invalid
-- uses the helper function printLineByLine to print each line of the array
printBoard :: [a] -> IO()
printBoard arr = do
    let len = lengthCheck arr
    if len /= 0 then do
        printLineByLine len arr
    else do
        putStrLn "Error: unable to print board due to invalid board size"

-- | printWinner takes a winner and prints a win-message to the screen
printWinner :: Char -> IO()
printWinner winner = do
    if winner /= '_' then do
        let string = "GAME OVER " ++ [winner] ++ " WON"
        putStrLn string
    else
        putStrLn "GAME OVER DRAW"

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
winCheck arr = do
    -- get the line length
    let len = lengthCheck arr
    if len /= 0 then do
        -- check diagonal
        let result = winCheckHorizontal len (createDiagonalLines len arr)
        if result /= '_' then
            result
        else do
            -- check line by line
            let result = winCheckHorizontal len arr -- Check horizontal lines
            if result /= '_' then
                result 
            else do -- Check vertical lines
                let result = winCheckHorizontal len (rollHelper len True arr)
                result
    else -- Default to blank output if invalid input
        '_'

-- | createDiagonalLines takes a board length and a board, returns the two diagonal lines' pieces as a list
createDiagonalLines :: Int -> [Char] -> [Char]
createDiagonalLines len board = do
    createDiagonalFromCoords ((createDiagonalLineHelper True len 0) ++ (createDiagonalLineHelper False len 0)) board

-- | createDiagonalLineHelper line takes an array length and returns the left or right diagonal line
-- according to the right bool
createDiagonalLineHelper :: Bool -> Int -> Int -> [Int]
createDiagonalLineHelper right len recCount = do
    if len == recCount then
        []
    else
        if right then 
            [0+(recCount*(len+1))] ++ (createDiagonalLineHelper right len (recCount+1))
        else
            [(len-1)+(recCount*(len-1))] ++ (createDiagonalLineHelper right len (recCount+1))

-- | createDiagonalLineHelper tests for left and right with grid sizes 3 and 5
-- >>> createDiagonalLineHelper True 3 0
-- [0,4,8]
--
-- >>> createDiagonalLineHelper False 3 0
-- [2,4,6]
--
-- >>> createDiagonalLineHelper True 5 0
-- [0,6,12,18,24]
--
-- >>> createDiagonalLineHelper False 5 0
-- [4,8,12,16,20]
--


-- | createDiagonalFromCoords takes a list of coords and a board and returns the board slots corresponding to the coords
createDiagonalFromCoords :: [Int] -> [Char] -> [Char]
createDiagonalFromCoords coordinates board = [board!!x | x <- coordinates]

-- >>> createDiagonalFromCoords [0,4,8,2,4,6] ['X','X','X','O','X','O','X','_','X']
-- "XXXXXX"
--

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