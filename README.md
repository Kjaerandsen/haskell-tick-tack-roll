# haskell-tick-tack-roll

Tick tack roll game in haskell

## The game
Tick tack roll is a digital version of tick tack toe which adds the option to roll the board to the right or the left on your turn.
This allows for more interesting gameplay decisions. Rolling simply works by first swapping the first and last pieces on the first row of the board,
then rotating the board either to the left or to the right.

Win conditions:
The game is won for a player if the player has the grid size amount of pieces in a row.
For example if the grid is three by three squares, a line of three pieces in any direction is a win.
If a player tries to perform an illegal move the enemy player wins.
If there are no available tiles left in the grid the game is a draw.

## Running the program
The program can be run without any parameters, it then defaults to playing against a bot, the bot having the first move and playing as x on a three by three grid.

Available parameters: 
gamemode and gridsize

- gamemode: a number deciding the gamemode. 0 is against the bot with you playing as x (starting), 1 the bot starts and 2 is player vs player, defaults to one.
- gridsize: a number deciding the size of the grid, minimum value of three, only valid as odd numbers (3 + 2x sizes), defaults to three.

Input:
After the board is printed the user can input a number and a direction to roll if wanted. The number corresponds to the place in the grid the user wants to place their piece. This is a number which is counted up by reading line by line left to right. First tile is 1, second on the same row is 2 etc.
Example inputs:
- 5 left
- 2
- 3 right

## Building and testing
The program uses stack to build and test.
Simply run `stack build` to build the excecutable.
For testing run `stack test`.

Examples of running the program with and without parameters:
- ./haskell-tick-tack-roll.exe 0

Playing against the bot, with the player starting as X.

- ./haskell-tick-tack-roll.exe 1

Playing against the bot, with the bot starting as X.

- ./haskell-tick-tack-roll.exe 2

Playing against another player.

- ./haskell-tick-tack-roll.exe 0 5

- Playing against the bot, with the player starting as X, on a five by five grid.

## Notes on own implementation and work on the assignment:
- The whole program should handle arbitrary grid sizes as long as the size is (3+2x) by (3+2x).
- The parameters to run the program decide gamemode and grid size as explained above.
- The bot currently defaults to the first open slot unless it sees a winning move in the grid.
- There is not proper error handling in parsing the input.
- Functions are split into three different libraries, bot functions in the bot library, roll functions in the roll library and helper functions for main in the lib library.
- The win messages are simplified to only display the piece of the winner (GAME OVER X WON/GAME OVER O WON/GAME OVER DRAW).
- When i started implementing the main gameplay loop a lot of challenges appeared, as i am not very familiar with haskell i struggled estimating time and planning how to divide the tasks into smaller pieces and functions. So the work got less organized and a little messy. Writing pseudo-code helped, but i probably should have spent more time thinking/planning and divided the work into smaller chunks which could be tested as individually. As i ended up with a lot of problems resulting from bloated functions which relied on a lot of small parts, leading to difficulty testing and fixing the functions.
- Better error handling would also be something i would focus on if i were to do this again.
- Implementing arbitrary grid sizes was a fun challenge which took up a lot of my time spent on the project.

Time tracking:

- tests: 
145m coding

- other timetracking:
20m thinking
67m coding

- Args:
10m research
10m coding

- Bot ai:
30m thinking
138m coding

- main gameplay loop:
25m planning
321m coding

- input parsing:
10m researching
17m coding

- Convert datatypes in the program:
25m coding.

- Board Printing function:
34m coding.

- Roll right function:
49m coding

- Roll left function:
30m thinking
23m researching.
90m coding

- Total:
15h 06m coding (906m)
43m researching
1h 20m thinking

- Total time spent:
17h 9m

Sidenote: The line between coding and researching / thinking is a little blurred. I tend to do theese actions at the same time so the coding part is skewed larger than the others.
