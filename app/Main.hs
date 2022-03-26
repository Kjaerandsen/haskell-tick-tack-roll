module Main where

import Roll -- Rolling functions
import Lib

main :: IO ()
main = do
    mainInit 0 3

-- Function for setting up the game, decide board size, mode (vs bot or player),
-- if bot then which piece the bot plays as. Possibly also bot difficulty.

mainInit :: Int -> Int -> IO()
mainInit mode boardSize = do
    let board = take (boardSize*boardSize) (repeat '_')
    putStrLn board
    -- Do the first bot move if the bot should start (mode == 1)
    if mode == 1 then do
        -- botMove
        mainLoopPVB 'O' 'X' board
    -- Else start the game as usual
    else
        -- add a check for mode, 0 = pvb, 2 = pvp
        mainLoopPVB 'X' 'O' board

-- | mainLoop for player vs bot
mainLoopPVB :: Char -> Char -> [Char] -> IO()
mainLoopPVB playerPiece botPiece board = do
    printBoard board

    -- Take player input
    inputLine <- getLine
    -- Parse the player input
    let inputData = words inputLine
    
    -- If the move is invalid
    if (inputData!!0) == " " then do
        if playerPiece == 'O' then
            printWinner 'X'
        else
            printWinner 'O'
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
                        mainLoopPVB playerPiece botPiece updatedBoard
                    else
                        printWinner (updatedBoard!!0)
                else
                    printWinner winner
            else do
                let winner = winCheck updatedBoard
                if winner == '_' then
                    printWinner winner
                else do
                    -- The bot makes his move
                    let updatedBoard = botMove botPiece updatedBoard
                    if (length updatedBoard) > 1 then
                        mainLoopPVB playerPiece botPiece updatedBoard
                    else
                        printWinner (updatedBoard!!0)
        -- If the player wins
        else do
            printWinner botPiece
    

-- | playerMove takes a player move, performs it and checks if a win condition occurrs
-- Char which decides which piece the player plays
-- Char array containing the game board

-- needs to simply return the board instead of winner, have another winner check in the calling function instead
playerMove :: Char -> Int -> [Char] -> [Char] -> [Char]
playerMove playerPiece move direction board = do
    -- Do the move here
    -- TODO: check if a winner is found at the move first, then the roll
    roll direction (doMove playerPiece move board)

-- WinCheck
-- if boardState == [] then
--        '_'
--    else do
        -- Check for a winner
--        let winner = winCheck board
--        if winner /= '_' then
--            -- Return the winner if there is one
--            winner
--        else if (membersInList '_' board) == 0 then
--            -- If there is no winner and all tiles are used up return a tie
--            '_'
--        else
--            ' '

-- | botMove does a move for the bot, returns the board if no winner, else returns the winning party
botMove :: Char -> [Char] -> [Char]
botMove piece board = do
    -- Create a list of possible moves
    -- Pick one at random
    -- Perform the move
    -- Rotate if wanted
    -- Return the winner when there is one, if no winner return the board
    ['X']

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