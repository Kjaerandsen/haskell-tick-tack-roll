module Bot
    ( testMoves,
      findPossibleMoves,
      testMovesRoll
    ) where

import Lib
import Roll


-- | findPossibleMoves takes a board, and a counter. Returns a list of possible move coordinates.
findPossibleMoves :: [Char] -> Int -> [Int] -> [Int]
findPossibleMoves board counter moves = do
    if (length board) > 0 then
        if head board == '_' then
            findPossibleMoves (tail board) (counter+1) (moves++[counter])
        else
            findPossibleMoves (tail board) (counter+1) moves
    else
        moves
        
-- >>> findPossibleMoves "O__XX___O" 1 []
-- [2,3,6,7,8]
--


-- | testMoves, goes through a list of moves against a board and checks if any move
-- results in a victory, returns the winning move if there is one, else 0
testMoves :: [Int] -> Char -> [Char] -> Int
testMoves moves piece board = do
    if (length moves) > 0 then do
        let updatedBoard = doMove piece (head moves) board
        let winner = winCheck updatedBoard
        if winner == '_' then
            testMoves (tail moves) piece board
        else
            -- Return the winning move if there is one
            head moves
    else
        0

-- >>> testMoves [2,3,6,7,8] 'X' "O__XX___O"
-- 6
--


-- | testMovesRoll, same as testMoves, but with rolling.
testMovesRoll :: [Int] -> Char -> [Char] -> Int
testMovesRoll moves piece board = do
    if (length moves) > 0 then do
        -- Calculate the board after the move
        let updatedBoard = doMove piece (head moves) board
        -- Roll the board to the left and test
        let rolledBoard = roll "left" updatedBoard
        let winner = winCheck rolledBoard
        if winner /= piece then do
            let rolledBoard = roll "left" updatedBoard
            let winner = winCheck rolledBoard
            if winner /= piece then
                testMoves (tail moves) piece board
            else
                -- Returns the winning move if there is one
                head moves
        else
            -- Return the winning move if there is one
            head moves
    else
        0

-- >>> testMovesRoll 