module Moving
    ( Moves
    , Move
    , Position
    , Pieces
    , State
    , move
    , isWithinBoard
    , debugState -- FIXME: Remove
    )
where

import           System.IO
import           Control.Monad
import           System.Random
import           Data.List
import           Cards
import           Helper

type State = (Cards, Pieces, Pieces)

type Pieces = [Position]

type Moves = [Move]

type Move = (Position, Position, Card)

type Position = (Int, Int)

debugState = (["Not a card"], [(0, 0)], [(0, 0)]) :: State


-- Make a move. Takes a state and a move to be applied to the state. Returns the new state.
-- 
-- Check if a piece should be removed after the move is taken
move :: State -> Move -> Maybe (Either State State)
move curState@(cards, pA, pB) m@(start, end, c)
    | validMove m = case () of -- Nested if statements
        ()
            | isWinningMove (head pA) (head pB) start end
            -> Just $ Right $ flipBoard $ checkRemove
                (switchCards cards c, movePiece pA m, pB)
            | otherwise
            -> Just $ Left $ flipBoard $ checkRemove
                (switchCards cards c, movePiece pA m, pB)
    | otherwise = Nothing

-- Remove overlapping pieces
checkRemove :: State -> State
checkRemove (cards, pA, pB) = (cards, pA, [ x | x <- pB, x `notElem` pA ]) -- Filter elements

movePiece :: Pieces -> Move -> Pieces
movePiece p (start, end, _) = map (\x -> if x == start then end else x) p -- Move element if it is equal to the start position. TODO: Maybe sort here

-- Compare a move with every moveset for a card
validMove :: Move -> Bool
validMove move@(start, end, card) = isCardMove move && isWithinBoard end

-- Compare a move with a card's moveset
isCardMove :: Move -> Bool
isCardMove ((x, y), (z, w), card) =
    any (\(a, b) -> (z == x + a) && (w == y + b)) $ getMoveSet card

-- Check if piece is still on the board after move.
isWithinBoard :: Position -> Bool
isWithinBoard (x, y) = all (\a -> (a < 5) && (a >= 0)) [x, y]

-- Checks if a move is the winning move
isWinningMove :: Position -> Position -> Position -> Position -> Bool
isWinningMove mA mB start end@(x, _) | mB == end             = True-- Won by Way of the Stone
                                     | start == mA && x == 4 = True-- Won by Way of the Stream
                                     | otherwise = False 

flipBoard :: State -> State
flipBoard (cards, pA, pB) =
    (cards, head pBf : sort (tail pBf), head pAf : sort (tail pAf))
  where
    pAf = flipPieces pA
    pBf = flipPieces pB


flipPieces :: Pieces -> Pieces
flipPieces = map (\(x, y) -> (4 - x, 4 - y))
