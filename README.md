# onitama-haskell
## Custom types
The program uses some custom type synonyms.

```Haskell
type Game = (State, Moves)

type State = (Cards, Pieces, Pieces)

type Pieces = [Piece]

type Moves = [Move]

type Move = (Position, Position, Card)

type Piece = (Int, Int)

type Position = (Int, Int)

type PlayerOneTurn = Bool

type Card = String

type Cards = [Card]
```

## generateRandom



## isValid
First a game is being parsed using the given file. The string input will be turned into a `Game`, where the first line of a file is the `State` and the rest of the lines are a list of moves to be made, `Moves`.

It will start playing the game, by taking the first move in the list and try it. If there are no moves, it results in a `ParsingError`.

When a move is being made, it will give back the new state of the board. A move can thus give a `Maybe State`. It returns `Nothing` if an invalid move was made. The program uses `Either` to determine if the move was a winning move or just a normal move. `Left` is a normal move, and `Right` is a winning move. This way it can check if there are more moves after a winning move.

When a game ends it will return the current state of the board as a string.

Before each move is made, they are being run through a `validMove` function that look like this.

```Haskell
validMove :: Cards -> Move -> Pieces -> Bool
validMove cs m@(start, end, card) ps = isPlayerCard cs card && isCardMove m && isWithinBoard end && takeOwnPiece m ps && isPlayerPiece start ps
```

After each move, the program checks if some of the pieces in player B are overlapping with player A, if so remove from player B's pieces.
The pieces will be mirrored and switched, and the cards will be switched around as well.


## Tests
Sometimes I had some code that would not be covered by tests, because it would never reach that piece of code.
I often had catchalls or an `otherwise` at the end of guards, but I made checks that made it so it would never reach that.

Some of the boolean expressions are always `true`. But that is because the code makes some checks before reaching the code with the boolean checks, such that if they reach the last boolean check, it will always evaluate to `true`. Some of the are `otherwise` which is obviously always `true`. There is one if statement in `RandomGame.hs`, that always evaluates to `false`. That is because there are no tests for random, where it reaches a point in the game where there is no move that can be made. I still have the check, since it could happen, but it is hard to make a test for it since it is random.

The reason for 96% alternatives used, is the same as above. It is because it never runs the code, where no move can be made.