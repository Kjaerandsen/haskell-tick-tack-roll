module Main where

import Roll -- Rolling functions
import Lib

main :: IO ()
main = do
    mainInit 1 3

-- Function for setting up the game, decide board size, mode (vs bot or player),
-- if bot then which piece the bot plays as. Possibly also bot difficulty.

mainInit :: Int -> Int -> IO()
mainInit mode boardSize = do
    let board = take (boardSize*boardSize) (repeat '_')
    putStrLn board
    -- Do the first bot move if the bot should start (mode == 1)
    if mode == 1 then do
        let updatedBoard = botMove 'X' board
        mainLoopPvB 'O' 'X' updatedBoard
    -- Else start the game as usual
    else if mode == 0 then
        -- add a check for mode, 0 = pvb, 2 = pvp
        mainLoopPvB 'X' 'O' board
    else
        mainLoopPvP 'X' 'O' board

-- | mainLoop for player vs bot, recurses until a winner is printed
mainLoopPvB :: Char -> Char -> [Char] -> IO()
mainLoopPvB playerPiece botPiece board = do
    printBoard board

    -- Take player input
    inputLine <- getLine
    -- Parse the player input
    let inputData = words inputLine
    
    -- If the move is invalid
    if (inputData!!0) == " " then do
        printWinner botPiece
    else do
        -- Error handling?
        let move = read (inputData!!0)
        -- parse the input
        let updatedBoard = doMove playerPiece move board
        
        if updatedBoard /= [] then do
            if (length inputData > 1) then do
                let updatedBoard = roll (inputData!!1) updatedBoard
                -- Check for victory
                let winner = winCheck updatedBoard
                if winner == '_' then do
                    -- The bot makes his move
                    let updatedBoard = botMove botPiece updatedBoard
                    if (length updatedBoard) > 1 then
                        mainLoopPvB playerPiece botPiece updatedBoard
                    else
                        printWinner (updatedBoard!!0)
                else
                    printWinner winner
            else do
                let winner = winCheck updatedBoard
                if winner == '_' then do
                    -- The bot makes his move
                    let updatedBoard = botMove botPiece updatedBoard
                    if (length updatedBoard) > 1 then
                        mainLoopPvB playerPiece botPiece updatedBoard
                    else
                        printWinner (updatedBoard!!0)
                else
                    printWinner winner
        -- If the player wins
        else do
            printWinner botPiece

-- | Main loop for pvp gameplay (recurses with alternating player pieces), recurses until a winner is printed
mainLoopPvP :: Char -> Char -> [Char] -> IO()
mainLoopPvP playerPiece nextPlayerPiece board = do
    printBoard board

    -- Take player input
    inputLine <- getLine
    -- Parse the player input
    let inputData = words inputLine
    -- If the move is invalid
    if (inputData!!0) == " " then do
        printWinner nextPlayerPiece
    else do
        -- Error handling?
        let move = read (inputData!!0)
        -- parse the input
        let updatedBoard = doMove playerPiece move board
        
        if updatedBoard /= [] then do
            if (length inputData > 1) then do
                let updatedBoardRoll = roll (inputData!!1) updatedBoard
                -- Check for victory
                let winner = winCheck updatedBoardRoll
                if winner == '_' then do
                    -- Recurse if no winner
                    mainLoopPvP nextPlayerPiece playerPiece updatedBoardRoll
                else
                    printWinner winner
            else do
                let winner = winCheck updatedBoard
                if winner == '_' then
                    -- Recurse if no winner
                    mainLoopPvP nextPlayerPiece playerPiece updatedBoard
                else do
                    printWinner winner
        -- If the player wins
        else do
            printWinner nextPlayerPiece


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


-- | doMove takes a move and a board, returns the board if the move is successfull
-- else if the spot is occupied returns an empty array
doMove :: Char -> Int -> [Char] -> [Char]
doMove piece move board = do
    -- Check if the move is within the valid range for a move
    if ((length board) < move) || (move < 1) then
        []
    -- Check if the tile is occupied already
    else if board!!(move-1) /= '_' then
        []
    else
        (take (move-1) board) ++ [piece] ++ drop move board

-- | doMove tests:
-- >>> doMove 'X' 10 ['X','O','X','O','X','O','_','_','_']
-- ""
--
-- >>> doMove 'X' 9 ['X','O','X','O','X','O','_','_','_']
-- "XOXOXO__X"
--
-- >>> doMove 'X' 1 ['X','O','X','O','X','O','_','_','_']
-- ""
--
-- >>> doMove 'X' 1 ['_','O','X','O','X','O','_','_','_']
-- "XOXOXO___"
--