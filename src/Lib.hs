module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | roll left tests, should only allow grid sizes of 3x3 and larger odd numbers, else return empty arrays
-- >>> rollLeft [3,2,1]
-- [3,2,1]
--
-- >>> rollLeft [3,2,1,4]
-- []
--
-- >>> rollLeft []
-- []
--

-- >>> sqrt 10
-- 3.1622776601683795
--


-- | rollLeft function that handles rolling a board of 3 + 2x size to the left according to the game rules
rollLeft :: [Int] -> [Int]
rollLeft x = do
    let len = lengthCheck x
    if len /= 0 then
        -- Rotate the array and return it
        x
    else
        -- Return an empty array
        []   

-- Need to check if the length sqrt is an integer or not.

-- | lengthCheck checks if the length is valid, returns the length if valid, 0 if not
lengthCheck :: [Int] -> Int
lengthCheck x = do
    let len = length x
    if len > 3 then
        0
    else if (len `mod` 2) /= 1 then
        0
    else
        -- Check if the length squared is a whole number
        -- Return that number if true, else return 0
        len