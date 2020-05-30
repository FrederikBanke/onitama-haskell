module Moving
    ( Moves
    , Move
    , Position
    , Pieces
    , Piece
    , State
    , PlayerOneTurn
    , move
    , isWithinBoard
    , validMove
    )
where

import           System.IO
import           Control.Monad
import           System.Random
import           Data.List
import           Cards
import           Helper

type State = (Cards, Pieces, Pieces)

type Pieces = [Piece]

type Moves = [Move]

type Move = (Position, Position, Card)

type Piece = (Int, Int)

type Position = (Int, Int)

type PlayerOneTurn = Bool

-- Make a move. Takes a state and a move to be applied to the state. Returns the new state.
-- Check if a piece should be removed after the move is taken
move :: State -> Move -> Maybe (Either State State)
move curState@(cards@[c1,c2,_,_,_], pA, pB) m@(start, end, c)
    | validMove [c1, c2] m pA = case () of -- Nested if statements
        ()
            | isWinningMove (head pA) (head pB) start end
            -> Just $ Right $ flipBoard (sortCards (switchCards cards c), movePiece pA m, [])
            | otherwise
            -> Just $ Left $ flipBoard $ checkRemove
                (sortCards (switchCards cards c), movePiece pA m, pB)
    | otherwise = Nothing

-- Remove overlapping pieces
checkRemove :: State -> State
checkRemove (cards, pA, pB) = (cards, pA, [ x | x <- pB, x `notElem` pA ]) -- Filter elements


movePiece :: Pieces -> Move -> Pieces
movePiece p (start, end, _) = map (\x -> if x == start then end else x) p -- Move element if it is equal to the start position.

-- Compare a move with every moveset for a card
-- Makes sure that the player has the card being used
-- Is the move within the board
validMove :: Cards -> Move -> Pieces -> Bool
validMove cs m@(start, end, card) ps = isPlayerCard cs card && isCardMove m && isWithinBoard end && takeOwnPiece m ps && isPlayerPiece start ps

isPlayerPiece :: Piece -> Pieces -> Bool
isPlayerPiece p ps = p `elem` ps

takeOwnPiece :: Move -> Pieces -> Bool
takeOwnPiece (_, end, _) p = end `notElem` p

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
isWinningMove :: Piece -> Piece -> Position -> Position -> Bool
isWinningMove mA mB start end@(x, y) | mB == end             = True-- Won by Way of the Stone
                                     | start == mA && x == 4 && y == 2 = True-- Won by Way of the Stream
                                     | otherwise = False 

-- Flip the pieces of the board.
-- Pattern matches with empty, so we do not take the head of an empty list.
flipBoard :: State -> State
flipBoard (cards, [a], []) = (cards, [], af)
    where af = flipPieces [a]

flipBoard (cards, pA@(x:xs), []) = (cards, [], pAs)
  where
    pAf = flipPieces pA
    pAs = head pAf : sort (tail pAf)

flipBoard (cards, [a], [b]) = (cards, bf, af)
    where 
        af = flipPieces [a]
        bf = flipPieces [b]

flipBoard (cards, pA@(x:xs), [b]) = (cards, bf, pAs)
  where
    pAf = flipPieces pA
    pAs = head pAf : sort (tail pAf)
    bf = flipPieces [b]

flipBoard (cards, [a], pB@(x:xs)) = (cards, pBs, af)
  where
    pBf = flipPieces pB
    pBs = head pBf : sort (tail pBf)
    af = flipPieces [a]


flipBoard (cards, pA@(_:_), pB@(_:_)) =
    (cards, pBs, pAs)
  where
    pAf = flipPieces pA
    pBf = flipPieces pB
    pAs = head pAf : sort (tail pAf)
    pBs = head pBf : sort (tail pBf)


flipPieces :: Pieces -> Pieces
flipPieces = map (\(x, y) -> (4 - x, 4 - y))
