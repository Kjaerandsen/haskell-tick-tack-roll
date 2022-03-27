module Main where

import Roll               -- Rolling functions
import Lib                -- Other game logic
import Bot                -- Bot ai / functions
import System.Environment -- For arguments

main :: IO ()
main = do
    -- Take arguments, first arg for mode, second for board size?
    args <- getArgs
    if args /= [] then do
        -- Check the length of the args, if bigger than one read the second as board size
        if length args > 1 then do
            mainInit 1 3
        else do
        -- else just start with the first as mode
            let test = head(args) -- Take only the first argument
            let test2 = read ((words test) !! 0) -- Read it as an integer
            if test2 > 2 || test2 < 0 then
                mainInit 1 3 -- If invalid input arg use the default
            else
                mainInit test2 3
    else
        mainInit 1 3

-- | mainInit function for setting up the game, decide board size, mode (vs bot or player),
-- if bot then which piece the bot plays as. Possibly also bot difficulty.
mainInit :: Int -> Int -> IO()
mainInit mode boardSize = do
    let board = take (boardSize*boardSize) (repeat '_')
    -- Do the first bot move if the bot should start (mode == 1)
    if mode == 1 then do
        let moveOfBot = botMove 'X' board
        mainLoopPvB 'O' 'X' (doMove 'X' (read (moveOfBot)) board)
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
                let updatedBoard2 = roll (inputData!!1) updatedBoard
                -- Check for victory
                let winner = winCheck updatedBoard2
                if winner == '_' then do
                    -- The bot makes his move
                    let moveOfBot = botMove botPiece updatedBoard2
                    if (last moveOfBot) == 'W' then do
                        putStrLn (init moveOfBot)
                        printWinner botPiece
                    else do
                        -- Do the bot move and recurse
                        putStrLn moveOfBot
                        mainLoopPvB playerPiece botPiece (doMove botPiece (read moveOfBot) updatedBoard2)
                else
                    printWinner winner
            else do
                let winner = winCheck updatedBoard
                if winner == '_' then do
                    -- The bot makes his move
                    let moveOfBot = botMove botPiece updatedBoard
                    if (last moveOfBot) == 'W' then do
                        putStrLn (init moveOfBot)
                        printWinner botPiece
                    else do
                        -- Do the bot move and recurse
                        putStrLn moveOfBot
                        mainLoopPvB playerPiece botPiece (doMove botPiece (read moveOfBot) updatedBoard)
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

-- | botMove does a move for the bot, returns the move and a win flag if it is a winning move
-- else returns just the move
botMove :: Char -> [Char] -> [Char]
botMove piece board = do
    -- Create a list of possible moves
    let moves = findPossibleMoves board 1 []
    -- Check if centre is available, use it if it is
    let centerTile = ((((length board)-1) `div` 2) + 1)
    -- No win checking here as it will always be the first or second move in the game
    -- and therefore it is impossible for it to be a winning or loosing move
    if board!!centerTile == '_' then
        show (centerTile)
    else do
        -- Check if a move results in a victory
        let winningMove = testMoves moves piece board
        if winningMove /= 0 then do
            -- Return the winning move
            (show winningMove ++ " W")
        else do
            let winningMove = testMovesRoll moves piece board
            if winningMove /= 0 then do
                -- Return the winning move
                ((show winningMove) ++ " W")
            else do
                -- Random move if there is no directly winning move
                show (moves!!0)