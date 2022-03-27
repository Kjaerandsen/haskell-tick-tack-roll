module Main where

import Roll -- Rolling functions
import Lib  -- Other game logic
import Bot  -- Bot ai / functions

main :: IO ()
main = do
    mainInit 1 3

-- | mainInit function for setting up the game, decide board size, mode (vs bot or player),
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