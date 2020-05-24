module Moving
    ( Moves
    , Move
    , Position
    , Pieces
    , State
    , PlayerOneTurn
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

type PlayerOneTurn = Bool

debugState = (["Not a card"], [(0, 0)], [(0, 0)]) :: State


-- Make a move. Takes a state and a move to be applied to the state. Returns the new state.
-- Check if a piece should be removed after the move is taken
move :: State -> Move -> PlayerOneTurn -> Maybe (Either State State)
move curState@(cards@[c1, c2, _,_,_], pA, pB) m@(start, end, c) pot@True
    | validMove [c1, c2] m = case () of -- Nested if statements
        ()
            | isWinningMove (head pA) (head pB) start end
            -> Just $ Right $ flipBoard (switchCards cards c, movePiece pA m, [])
            | otherwise
            -> Just $ Left $ flipBoard $ checkRemove
                (switchCards cards c, movePiece pA m, pB) pot
    | otherwise = Nothing

move curState@(cards@[_, _, c3,c4,_], pA, pB) m@(start, end, c) pot@False
    | validMove [c3,c4] m = case () of -- Nested if statements
        ()
            | isWinningMove (head pB) (head pA) start end
            -> Just $ Right $ flipBoard (switchCards cards c, [], movePiece pB m)
            | otherwise
            -> Just $ Left $ flipBoard $ checkRemove
                (switchCards cards c, pA, movePiece pB m) pot
    | otherwise = Nothing

-- Remove overlapping pieces
checkRemove :: State -> PlayerOneTurn -> State
checkRemove (cards, pA, pB) True = (cards, pA, [ x | x <- pB, x `notElem` pA ]) -- Filter elements
checkRemove (cards, pA, pB) False  = (cards, [ x | x <- pA, x `notElem` pB ], pB) -- Filter elements

movePiece :: Pieces -> Move -> Pieces
movePiece p (start, end, _) = map (\x -> if x == start then end else x) p -- Move element if it is equal to the start position. TODO: Maybe sort here

-- Compare a move with every moveset for a card
-- Makes sure that the player has the card being used
-- Is the move within the board
validMove :: Cards -> Move -> Bool
validMove cs move@(start, end, card) = isPlayerCard cs card && isCardMove move && isWithinBoard end

-- TODO: Lav den her
isPlayerCard :: Cards -> Card -> Bool
isPlayerCard cs c = c `elem` cs

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

-- Flip the pieces of the board.
-- Pattern matches with empty, so we do not take the head of an empty list.
flipBoard :: State -> State
flipBoard (cards, pA@(_:_), pB@(_:_)) =
    (cards, head pAf : sort (tail pAf), head pBf : sort (tail pBf))
  where
    pAf = flipPieces pA
    pBf = flipPieces pB
flipBoard (cards, [], pB) = (cards, [], head pBf : sort (tail pBf))
  where
    pBf = flipPieces pB
flipBoard (cards, pA, []) = (cards, head pAf : sort (tail pAf), [])
  where
    pAf = flipPieces pA


flipPieces :: Pieces -> Pieces
flipPieces = map (\(x, y) -> (4 - x, 4 - y))
