Tower Defence Project

Members:
Turekulov Elaman, Naumov Dmitrii, Hokimiyon Muhammadjon

This game is Plants vs Zombies-like tower defence.

To run the project:
1) Open the "main" file
2) Run the stack and run the main file
3) You can now play the game

ATTENTION:
The game might not function properly if "CodeWorld.Sketches" won't import
In that case you can play the game on CodeWorld
Here is the link: https://code.world/haskell#PA9uwcz5A3utwGsvSbDP4Ng

Rules
   - Every 10 seconds player is gained 100 or more coins which player can spend to buy towers
   - There are 4 types of towers: fire magician, ice magician, elf and haskell
   - Towers can be put in squares
   - Every 5-15 seconds the wave of enemies appear
   - Player can see game timer and amount of coins in the top
   - The game ends on a ~180 second

How to play
   1) Pick a tower on the top 
   2) Pick a square to put a tower 
   3) If you have enough coins, then the tower will be placed


## How to use

### Using Cabal

To build and run a project (without tests), use

```sh
cabal v2-run --disable-tests
```

from the project directory. This will download all the necessary dependencies, compile the project and start CodeWorld canvas server at http://localhost:3000

To simply build the project, run

```sh
cabal v2-build
```

To run tests:

```sh
cabal v2-test
```

#### Freezing dependencies

When you add/change dependencies of the project it is a good idea to freeze the used versions, to ensure the project will be successfully built on another machine at another time. To freeze dependencies, run

```sh
cabal v2-freeze
```

### Using Stack

To build and run a project (without tests), use

```sh
stack run
```

from the project directory. This will download all the necessary dependencies, compile the project and start CodeWorld canvas server at http://localhost:3000

To simply build the project, run

```sh
stack build
```

To run tests:

```sh
stack test
```

### With Visual Studio Code

One way to use VS Code is to open a browser tab, pointing to [http://localhost:3000](http://localhost:3000).

![Sample VS Code setup.](images/vs-code-setup.png)
