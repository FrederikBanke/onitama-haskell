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

After each move, the program checks if some of the pieces in player B are overlapping with player A, if so remove from player B's peices.
The pieces will be mirrored and switched, and the cards will be switched around as well.

