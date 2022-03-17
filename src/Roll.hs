module Roll
    ( swap,
      roll,
      lengthCheck,
      rollHelper,
      rollRowsHelper,
      rollRowLeft,
      squaredInteger
    ) where

-- | swap tests, swaps the first and last element of the first line in valid grids
-- >>> swap [1..9]
-- [3,2,1,4,5,6,7,8,9]
--
-- >>> swap [1..25]
-- [5,2,3,4,1,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]
--
-- >>> swap [1..3]
-- []
--

-- | swap swaps the first and last mark off the first row
swap :: [a] -> [a]
swap x = do
    let len = lengthCheck x
    if len /= 0 then do
        -- swap item 0 and len in the list
        -- Get the first line, remove the head and tail (the items to swap)
        let middleFirstLine = init (tail (take len x))
        -- Get the rest of the list (drop the first line)
        let rest = drop len x
        -- Concattenate the lists
        [x!!(len-1)] ++ middleFirstLine ++ [x!!0] ++ rest
    else
        -- Return an empty array
        []   

-- | roll tests, takes a direction and a grid, rolls the grid the direction
-- >>> roll "left" [1..9]
-- [1,6,9,2,5,8,3,4,7]
--
-- >>> roll "left" [1..25]
-- [1,10,15,20,25,4,9,14,19,24,3,8,13,18,23,2,7,12,17,22,5,6,11,16,21]
--
-- >>> roll "right" [1..9]
-- [7,4,3,8,5,2,9,6,1]
--
-- >>> roll "right" [1..25]
-- [21,16,11,6,5,22,17,12,7,2,23,18,13,8,3,24,19,14,9,4,25,20,15,10,1]
--
-- >>> roll "" [1..25]
-- []
--
-- >>> roll "right" []
-- []
--
-- >>> roll "" []
-- []
--

-- roll function that rolls the grid the specified direction, returns an empty array on failure
roll :: String -> [a] -> [a]
roll s arr = do
    if s == "left" then
        rollHelper True (swap arr)
    else if s == "right" then
        rollHelper False (swap arr)
    else
        []

-- | rollHelper tests, takes a direction and a grid, returns the grid rotated that direction
-- >>> rollHelper True [1..9]
-- [3,6,9,2,5,8,1,4,7]
--
-- >>> rollHelper False [1..25]
-- [21,16,11,6,1,22,17,12,7,2,23,18,13,8,3,24,19,14,9,4,25,20,15,10,5]
--
-- >>> rollHelper False []
-- []
--
-- >>> rollHelper True [1..16]
-- []
--

-- rollHelper helper function that rolls the grid the provided direction
rollHelper :: Bool -> [a] -> [a]
rollHelper dir arr = do
    let len = lengthCheck arr
    if len /= 0 then do
        -- Rotate the array and return it
        if dir then do
            -- calculate the offset
            let offset = reverse [0..(len-1)]
            -- rotate the rows
            rollRowsHelper arr dir offset len
        else do
            -- calculate the offset
            let offset = reverse [0..(len-1)]
            -- rotate the rows
            rollRowsHelper arr dir offset len
    else
        -- Return an empty array
        []   

-- | rollRowsHelper rolls all rows of an array left if true, right if false
-- >>> rollRowsHelper [1..9] True [0..2] 3
-- [1,4,7,2,5,8,3,6,9]
--
-- >>> rollRowsHelper [1..9] False (reverse [0..2]) 3
-- [7,4,1,8,5,2,9,6,3]
--
-- >>> rollRowsHelper [1..25] True [0..4] 5
-- [1,6,11,16,21,2,7,12,17,22,3,8,13,18,23,4,9,14,19,24,5,10,15,20,25]
--
-- >>> rollRowsHelper [1..25] False (reverse [0..4]) 5
-- [21,16,11,6,1,22,17,12,7,2,23,18,13,8,3,24,19,14,9,4,25,20,15,10,5]
--


-- | rollRowsHelper takes a grid and a offset list, returns the grid rotated, left if dir, else right
rollRowsHelper :: [a] -> Bool -> [Int] -> Int -> [a]
rollRowsHelper arr dir offset rowLen = do
    if length offset /= 0 then
        if dir then
            (rollRowLeft rowLen (head offset) 0 arr) ++ (rollRowsHelper arr dir (tail offset) rowLen)
        else
            (rollRowsHelper arr dir (tail offset) rowLen) ++ reverse (rollRowLeft rowLen (head  offset) 0 arr) 
    else
        []

-- | rollRowLeft tests, rolls a single roll left
-- >>> rollRowLeft 3 0 0 [1..9]
-- [1,4,7]
--
-- >>> rollRowLeft 3 1 0 [1..9]
-- [2,5,8]
--
-- >>> rollRowLeft 3 2 0 [1..9]
-- [3,6,9]
--


-- | rollRowLeft recursive function that rolls a single row left
-- takes the row length and the whole grid, a deviation and a recursion counter as variables
rollRowLeft :: Int -> Int -> Int -> [a] -> [a]
rollRowLeft rowLen offSet recCount arr = do
    if rowLen == recCount then
        []
    else
        [arr!!((rowLen*recCount)+offSet)] ++ rollRowLeft rowLen offSet (recCount+1) arr


-- | lengthCheck tests, works only with valid grid sizes (3x3 + 2x) where x is a whole number
-- if the length is less than 9, not squarable as a whole number, or not even 0 is returned.
-- >>> lengthCheck [1..9]
-- 3
--
-- >>> lengthCheck [1..25]
-- 5
--
-- >>> lengthCheck [1..11]
-- 0
--
-- >>> lengthCheck [1..16]
-- 0
--
-- >>> lengthCheck []
-- 0
--
-- >>> lengthCheck [1..3]
-- 0
--


-- | lengthCheck checks if the length is valid, returns the length if valid, 0 if not
-- if the length is less than 9, not squarable as a whole number, or not even 0 is returned.
-- else the length squared is returned
lengthCheck :: [a] -> Int
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
-- 0
--
-- >>> squaredInteger 2
-- 1
--
-- >>> squaredInteger 9
-- 3
--
-- >>> squaredInteger 16
-- 4
--
-- >>> squaredInteger (-10)
-- 0
--


-- | squaredInteger takes an integer and returns the integer squared rounded to an integer
squaredInteger :: Int -> Int
squaredInteger x = do
    if x > 0 then
        round . sqrt . fromIntegral $ x
    else
        0

-- inspired by the first comment to the solution from kqr at stackoverflow:
-- The comment: "How does it compare to intSqrt = floor . sqrt . fromInteger ? – kqr Nov 14, 2013 at 7:41"
-- Link: https://stackoverflow.com/questions/19965149/integer-square-root-function-in-haskell