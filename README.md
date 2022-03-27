# haskell-tick-tack-roll

Tick tack roll game in haskell

## Running the program
The program can be run without any parameters, it then defaults to playing against a bot, the bot having the first move and playing as x on a three by three grid.

Available parameters: 
gamemode and gridsize

- gamemode: a number deciding the gamemode. 0 is against the bot with you playing as x (starting), 1 the bot starts and 2 is player vs player, defaults to one.
- gridsize: a number deciding the size of the grid, minimum value of three, only valid as odd numbers (3 + 2x sizes), defaults to three.

Examples of running the program with and without parameters:
- ./haskell-tick-tack-roll.exe 0
Playing against the bot, with the player starting as X.

./haskell-tick-tack-roll.exe 1
Playing against the bot, with the bot starting as X.

./haskell-tick-tack-roll.exe 2
Playing against another player.

./haskell-tick-tack-roll.exe 0 5
Playing against the bot, with the player starting as X, on a five by five grid.

