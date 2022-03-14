module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | roll left tests, should only allow grid sizes of 3x3 and larger odd numbers, else return empty arrays
-- >>> rollLeft [1,2,3,4,5,6,7,8,9]
-- >>> rollLeft [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
-- >>> rollLeft [3,2,1]
-- >>> rollLeft [3,2,1,4]
-- >>> rollLeft []
-- [1,4,7,2,5,8,3,6,9,4,7,10]
-- [1,6,11,16,21,2,7,12,17,22,3,8,13,18,23,4,9,14,19,24,5,10,15,20,25,6,11,16,21,26]
-- []
-- []
-- []
--

-- | rollLeft takes a grid and returns the grid rolled left
rollLeft :: [Int] -> [Int]
rollLeft x = do
    let len = lengthCheck x
    if len /= 0 then do
        -- Rotate the array and return it
        -- calculate the offset
        let offset = [0..len-1]
        -- rotate the rows
        rollLeftHelper x offset len
    else
        -- Return an empty array
        []   

-- | rollLeftHelper takes a grid and a offset list, returns the grid rotatet rollLeft
rollLeftHelper :: [Int] -> [Int] -> Int -> [Int]
rollLeftHelper arr offset rowLen = do
    if length offset /= 0 then
        (rollRowLeft rowLen (head  offset) 0 arr) ++ (rollLeftHelper arr (tail offset) rowLen)
    else
        []

-- >>> rollLeftHelper [1,2,3,4,5,6,7,8,9] [0,1,2] 3
-- [1,4,7,2,5,8,3,6,9]
--
-- >>> rollLeftHelper [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25] [3,2,1,0,-1] 5

-- >>> rollRowLeft 3 1 0 [1,2,3,4,5,6,7,8,9]
-- >>> rollRowLeft 3 0 0 [1,2,3,4,5,6,7,8,9]
-- >>> rollRowLeft 3 (-1) 0 [1,2,3,4,5,6,7,8,9]
-- [2,5,8]
-- [1,4,7]
-- [0,3,6]
--

-- | rollRowLeft recursive function that rolls a single row left
-- takes the row length and the whole grid, a deviation and a recursion counter as variables
rollRowLeft :: Int -> Int -> Int -> [Int] -> [Int]
rollRowLeft rowLen offSet recCount arr = do
    if rowLen == recCount then
        []
    else
        [arr!!(rowLen*recCount)+offSet] ++ rollRowLeft rowLen offSet (recCount+1) arr

-- Need to check if the length sqrt is an integer or not.

-- | lengthCheck tests, works only with valid grid sizes (3x3 + 2x) where x is a whole number
-- if the length is less than 9, not squarable as a whole number, or not even 0 is returned.
-- >>> lengthCheck [1,2,3,4,5,6,7,8,9]
-- >>> lengthCheck[1,2,3,4,5,6,7,8,9,10,11]
-- >>> lengthCheck[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
-- >>> lengthCheck []
-- >>> lengthCheck [0,1,2]
-- 3
-- 0
-- 0
-- 0
-- 0
--

-- | lengthCheck checks if the length is valid, returns the length if valid, 0 if not
-- if the length is less than 9, not squarable as a whole number, or not even 0 is returned.
-- else the length squared is returned
lengthCheck :: [Int] -> Int
lengthCheck x = do
    let len = length x
    if len < 9 then
        0
    else if (len `mod` 2) /= 1 then
        0
    else do
        -- Check if the length squared is a whole number
        let rowLen = squaredInteger len
        -- If not a whole number return 0
        if (rowLen * rowLen) /= len then
            0
        -- Else return the row length
        else
            rowLen

-- | Squared integer tests, works with positive, negative and zero values.
-- >>> squaredInteger 0
-- 0
--
-- >>> squaredInteger 2
-- 1
--
-- >>> squaredInteger (-10)
-- 0
--

-- | squaredInteger takes an integer and returns the integer squared rounded to an integer
squaredInteger :: Int -> Int
squaredInteger x = round . sqrt . fromIntegral $ x

-- inspired by the first comment to the solution from kqr at stackoverflow:
-- The comment: "How does it compare to intSqrt = floor . sqrt . fromInteger ? – kqr Nov 14, 2013 at 7:41"
-- Link: https://stackoverflow.com/questions/19965149/integer-square-root-function-in-haskell