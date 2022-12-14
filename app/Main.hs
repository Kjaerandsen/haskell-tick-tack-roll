module Main where

import Roll               -- Rolling functions
import Lib                -- Other game logic
import Bot                -- Bot ai / functions
import System.Environment -- For arguments
import System.Random      -- For bot random move

main :: IO ()
main = do
    -- Take arguments, first arg for mode, second for board size?
    args <- getArgs
    if args /= [] then do
        let gameMode = read ((words (args!!0)) !! 0) -- Read it as an integer
        if gameMode > 2 || gameMode < 0 then
            mainInit 1 3 -- If invalid input arg use the default
        else
            if length args > 1 then do
                let boardSize = read ((words (args!!1)) !! 0)
                if boardSize >2 && (boardSize) `mod` 2 /= 1 then
                    -- Use default boardSize if input arg is invalid
                    mainInit gameMode 3
                else
                    -- Else use parameter boardSize
                    mainInit gameMode boardSize
            else
                mainInit gameMode 3
        -- Check the length of the args, if bigger than one read the second as board size
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
                        putStrLn (take ((length moveOfBot)-2) moveOfBot)
                        printWinner botPiece
                    else if (last moveOfBot) == '_' then do
                        putStrLn (take ((length moveOfBot)-2) moveOfBot)
                        printWinner '_'
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
                        putStrLn (take ((length moveOfBot)-2) moveOfBot)
                        printWinner botPiece
                    else if (last moveOfBot) == '_' then do
                        putStrLn (take ((length moveOfBot)-2) moveOfBot)
                        printWinner '_'
                    else do
                        -- Do the bot move and recurse
                        -- Chose a random bot move, perform it and recurse
                        let moves = findPossibleMoves updatedBoard 1 []
                        let numberOfMoves = length moves
                        moveNumber <- randomRIO (1::Int, numberOfMoves::Int)
                        putStrLn (show (moves!!(moveNumber-1)))
                        mainLoopPvB playerPiece botPiece (doMove botPiece (moves!!(moveNumber-1)) updatedBoard)
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
    if (board!!(centerTile-1)) == '_' then
        show (centerTile)
    else do
        -- Check if a move results in a victory
        let winningMove = testMoves moves piece board
        if winningMove /= 0 then do
            -- Return the winning move
            (show winningMove ++ " W")
        else do
            let winningMove = testMovesRoll moves piece board
            if winningMove /= "" && winningMove /= "_" then do
                -- Return the winning move
                (show winningMove ++ " W")
            else if winningMove /= "" then do
                -- Return the winning and tie sign
                (show winningMove ++ " _")
            else do
                -- Random move handled in the mainLoopPvB function instead
                "D"