module Moving
    ( Moves
    , Move
    , Position
    , Pieces
    , State
    , move
    , debugState -- FIXME: Remove
    )
where

import           System.IO
import           Control.Monad
import           System.Random
import           Data.List
import           Cards

type State = (Cards, Pieces, Pieces)

type Pieces = [Position]

type Moves = [Move]

type Move = (Position, Position, Card)

type Position = (Int, Int)

debugState = (["Not a card"], [(0, 0)], [(0, 0)]) :: State


-- Make a move. Takes a state and a move to be applied to the state. Returns the new state.
-- 
move :: State -> Move -> Maybe State
move curState@(cards, pA, pB) move
    |
        -- Check if a piece should be removed after the move is taken
      validMove move = Just $ flipBoard $ checkRemove (cards, movePiece pA move, pB)
    | otherwise      = Nothing

-- Remove overlapping pieces
checkRemove :: State -> State
checkRemove (cards, pA, pB) = (cards, pA, [ x | x <- pB, x `notElem` pA ]) -- Filter elements

movePiece :: Pieces -> Move -> Pieces
movePiece p (start, end, _) = map (\x -> if x==start then end else x) p -- Move element if it is equal to the start position. TODO: Maybe sort here

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
-- TODO: Mangler at lave
isWinningMove :: Bool
isWinningMove = False

flipBoard :: State -> State
flipBoard (cards, pA, pB) = (cards, flipPieces pB, flipPieces pA)

flipPieces :: Pieces -> Pieces
flipPieces = map (\(x, y) -> (4 - x, 4 - y))
