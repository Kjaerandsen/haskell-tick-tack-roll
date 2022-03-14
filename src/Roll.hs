module Roll
    ( swap,
      roll,
      lengthCheck
    ) where

-- | swap tests, swaps the first and last element of the first line in valid grids
-- >>> swap [1,2,3,4,5,6,7,8,9]
-- >>> swap [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
-- >>> swap [1,2,3]
-- [3,2,1,4,5,6,7,8,9]
-- [5,2,3,4,1,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
-- []
--

-- | swap swaps the first and last mark off the first row
swap :: [Int] -> [Int]
swap x = do
    let len = lengthCheck x
    if len /= 0 then do
        -- swap item 0 and len in the list
        -- Get the first line, remove the head and tail (the items to swap)
        let middleFirstLine = init (tail (take len x))
        -- Get the rest of the list (drop the first line)
        let rest = drop len x
        -- Concattenate the lists
        [x!!len-1] ++ middleFirstLine ++ [x!!0] ++ rest
    else
        -- Return an empty array
        []   

-- | roll tests, takes a direction and a grid, rolls the grid the direction
-- >>> roll "left" [1,2,3,4,5,6,7,8,9]
-- >>> roll "left" [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
-- >>> roll "right" [1,2,3,4,5,6,7,8,9]
-- >>> roll "right" [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
-- >>> roll "" []
-- [3,6,9,2,5,8,1,4,7]
-- [5,10,15,20,25,4,9,14,19,24,3,8,13,18,23,2,7,12,17,22,1,6,11,16,21]
-- [9,6,3,8,5,2,7,4,1]
-- [25,20,15,10,5,24,19,14,9,4,23,18,13,8,3,22,17,12,7,2,21,16,11,6,1]
-- []
--

-- roll function that rolls the grid the specified direction, returns an empty array on failure
roll :: String -> [Int] -> [Int]
roll s arr = do
    if s == "left" then
        rollHelper True arr
    else if s == "right" then
        rollHelper False arr
    else
        []

-- rollHelper helper function that rolls the grid the provided direction
rollHelper :: Bool -> [Int] -> [Int]
rollHelper dir arr = do
    let len = lengthCheck arr
    if len /= 0 then do
        -- Rotate the array and return it
        if dir then do
            -- calculate the offset
            let offset = reverse [0..len-1]
            -- rotate the rows
            rollRowsHelper arr dir offset len
        else do
            -- calculate the offset
            let offset = reverse [0..len-1]
            -- rotate the rows
            rollRowsHelper arr dir offset len
    else
        -- Return an empty array
        []   

-- | rollRowsHelper rolls all rows of an array left if true, right if false
-- >>> rollRowsHelper [1,2,3,4,5,6,7,8,9] True [0,1,2] 3
-- >>> rollRowsHelper [1,2,3,4,5,6,7,8,9] False [0,1,2] 3
-- >>> rollRowsHelper [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25] True [3,2,1,0,-1] 5
-- >>> rollRowsHelper [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25] False [3,2,1,0,-1] 5
-- [1,4,7,2,5,8,3,6,9]
-- [7,4,1,8,5,2,9,6,3]
-- [4,9,14,19,24,3,8,13,18,23,2,7,12,17,22,1,6,11,16,21,0,5,10,15,20]
-- [24,19,14,9,4,23,18,13,8,3,22,17,12,7,2,21,16,11,6,1,20,15,10,5,0]
--

-- | rollRowsHelper takes a grid and a offset list, returns the grid rotated, left if dir, else right
rollRowsHelper :: [Int] -> Bool -> [Int] -> Int -> [Int]
rollRowsHelper arr dir offset rowLen = do
    if length offset /= 0 then
        if dir then
            (rollRowLeft rowLen (head  offset) 0 arr) ++ (rollRowsHelper arr dir (tail offset) rowLen)
        else
            reverse (rollRowLeft rowLen (head  offset) 0 arr) ++ (rollRowsHelper arr dir (tail offset) rowLen)
    else
        []

-- | rollRowLeft tests, rolls a single roll left
-- >>> rollRowLeft 3 0 0 [1,2,3,4,5,6,7,8,9]
-- >>> rollRowLeft 3 1 0 [1,2,3,4,5,6,7,8,9]
-- >>> rollRowLeft 3 2 0 [1,2,3,4,5,6,7,8,9]
-- [1,4,7]
-- [2,5,8]
-- [3,6,9]
--

-- | rollRowLeft recursive function that rolls a single row left
-- takes the row length and the whole grid, a deviation and a recursion counter as variables
rollRowLeft :: Int -> Int -> Int -> [Int] -> [Int]
rollRowLeft rowLen offSet recCount arr = do
    if rowLen == recCount then
        []
    else
        [arr!!(rowLen*recCount)+offSet] ++ rollRowLeft rowLen offSet (recCount+1) arr


-- | lengthCheck tests, works only with valid grid sizes (3x3 + 2x) where x is a whole number
-- if the length is less than 9, not squarable as a whole number, or not even 0 is returned.
-- >>> lengthCheck [1..9]
-- >>> lengthCheck [1..25]
-- >>> lengthCheck [1..11]
-- >>> lengthCheck [1..16]
-- >>> lengthCheck []
-- >>> lengthCheck [1..3]
-- 3
-- 5
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

-- | squared integer tests, works with positive, negative and zero values.
-- >>> squaredInteger 0
-- >>> squaredInteger 2
-- >>> squaredInteger (-10)
-- 0
-- 1
-- 0
--

-- | squaredInteger takes an integer and returns the integer squared rounded to an integer
squaredInteger :: Int -> Int
squaredInteger x = round . sqrt . fromIntegral $ x

-- inspired by the first comment to the solution from kqr at stackoverflow:
-- The comment: "How does it compare to intSqrt = floor . sqrt . fromInteger ? – kqr Nov 14, 2013 at 7:41"
-- Link: https://stackoverflow.com/questions/19965149/integer-square-root-function-in-haskell