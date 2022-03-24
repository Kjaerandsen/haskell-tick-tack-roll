module Main where

import Roll -- Rolling functions
import Lib

main :: IO ()
main = printBoard [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5]

-- Function for setting up the game, decide board size, mode (vs bot or player),
-- if bot then which piece the bot plays as. Possibly also bot difficulty.

{-
-- | mainLoop takes a Bool which decides if the game is over or not
-- a Char which decides which piece the bot plays ['X','O','_'] if bot == '_' then the game is pvp
-- a bool which decides which players turn it is, True == 'X', False == 'O'
-- a Char array containing the game board
-- First bool might be redundant
mainLoop :: Bool -> Char -> Char -> [Char] -> IO () -> Char
mainLoop finished bot turn board = do
    if finished then
        '_'
    else do
        if turn == bot then // Let the bot do its turn
            move = botTurn bot board
        else do
            // ask user for input
            putStr "Player " + turn + "'s turn.\n Player input:"

            move = the input

        // Do the move here,
        // check if it conflicts with a location on the board, then return a winner? do this within the function
        let boardState = doMove move board
        if boardState == [] then
            '_'
        else do
            let winner = winCheck board
            
            if winner /= '_' then
                winner
            else do
                if membersInList '_' board == 0 then
                    '_'
                else
                    mainLoop finished bot turn board
-}

{-
-- | doMove takes a move and a board, returns the board if the move is successfull
-- else if the spot is occupied returns an empty array
doMove :: Char -> Int -> [Char] -> [Char]
doMove piece move board = do
    if board!!move /= '_' then
        []
    else
        
-}