module Main where

import Roll -- Rolling functions
import Lib

main :: IO ()
main = do
    printBoard [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5]
    let test = playerMove 'X' "_________"
    putChar test

-- Function for setting up the game, decide board size, mode (vs bot or player),
-- if bot then which piece the bot plays as. Possibly also bot difficulty.

-- | playerMove takes a player move, performs it and checks if a win condition occurrs
-- Char which decides which piece the player plays
-- Char array containing the game board
playerMove :: Char -> [Char] -> Char
playerMove playerPiece board = do
    -- take user input
    let move = 4
    let rotate = "left"
    -- parse it into the move let

    -- Do the move here
    let boardState = roll rotate (doMove playerPiece move board)

    if boardState == [] then
        '_'
    else do
        -- Check for a winner
        let winner = winCheck board
        if winner /= '_' then
            -- Return the winner if there is one
            winner
        else if (membersInList '_' board) == 0 then
            -- If there is no winner and all tiles are used up return a tie
            '_'
        else
            ' '

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