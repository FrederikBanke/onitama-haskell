module WinningStrategy
    ( makeWinningMove
    )
where


import           System.IO
import           Text.Read
import           Control.Monad
import           System.Random
import           Data.Maybe
import           Data.List
import           Cards
import           Moving
import           Helper
import qualified RandomGame                    as RG


-- Return which player won or "None".
makeWinningMove :: State -> Int -> PlayerOneTurn -> Maybe String
makeWinningMove _ 0 pot = Just "None" -- Check who won, or none 

-- makeWinningMove s@(cs@[c1, c2, _, _, _], pA, pB) n pot = do
    -- Choose move set
    -- let posToTry  = getMoveSet $ head cs
    -- let moveToTry = tryMove [c1, c2] pA [c1, c2] posToTry pA
    -- -- Check move is valid, if not try again.
    -- if isNothing moveToTry
    --     then makeWinningMove s n pot  -- No move could be made
    --     else do
    --         let m = unPack moveToTry
            -- checkRandomMove (move s m) (gameString ++ "\n" ++ show m)

-- Recursively makes the moves.
-- Left: A normal move was made.
-- Right: A winning move was made.
checkRandomMove :: Maybe (Either State State) -> String -> IO String
-- checkRandomMove (Just (Left  s)) gs = makeWinningMove s gs
checkRandomMove (Just (Right _)) gs = return gs
checkRandomMove Nothing          gs = return gs


-- Tries to make a move with a given card.
-- Cards in hand -> Player pieces-> Cards to play -> Movesets for card -> Pieces to try
tryMove :: Cards -> Pieces -> Cards -> [Position] -> Pieces -> Maybe Move
tryMove _    _  _          _  []       = Nothing -- No pieces left to try
    -- No movesets left to try
-- tryMove hand pA c@[ca, cb] [] (p : ps) = tryMove hand pA [cb] () (p : ps)
    -- No cards left to try
-- tryMove hand pA [_]        [] (p : ps) = tryMove hand pA hand () ps
    -- Try all moveset
tryMove hand pA c@(ca : _) (e : es) (p : ps)
    | validMove hand (p, addTup p e, ca) pA = Just (p, addTup p e, ca)
    | otherwise                             = tryMove hand pA c es (p : ps)
