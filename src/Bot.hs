module Bot
    ( botMove,
    testMoves,
    findPossibleMoves
    ) where

-- | botMove does a move for the bot, returns the board if no winner, else returns the winning party
botMove :: Char -> [Char] -> [Char]
botMove piece board = do
    -- Create a list of possible moves
    --let moves = findPossibleMoves board 1 []
    -- Check if a move results in a victory
    -- Perform the move

    -- Else check with rotation
    
    -- Else do random random


    -- Return the winner when there is one, if no winner return the board
    "____X____"

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