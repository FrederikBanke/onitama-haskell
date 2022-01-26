module RandomGame
    ( initialState
    , makeRandomMove
    , shufflePieces
    , shuffleCards
    , makeMoveSet
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

-- A default start for a game. A random deck will be assigned.
initialState :: StdGen -> IO State
initialState rGen = do
    cards <- shuffleDeck rGen
    return
        ( sortCards cards
        , [(0, 2), (0, 0), (0, 1), (0, 3), (0, 4)]
        , [(4, 2), (4, 0), (4, 1), (4, 3), (4, 4)]
        )

-- Make a random move by shuffling pieces and cards on hand.
makeRandomMove :: StdGen -> State -> Int -> String -> IO String
makeRandomMove rGen s@(cs@[c1, c2, _, _, _], pA, pB) n gameString = do
    let ps        = shufflePieces rGen pA
    let pc        = shuffleCards rGen [c1, c2]
    -- Choose move set
    let posToTry  = makeMoveSet rGen (head pc)
    let moveToTry = tryMove rGen pc pA pc posToTry ps
    -- Check move is valid, if not try again.
    let m         = unPack moveToTry
    -- Make move and update state
    checkRandomMove (move s m) rGen (n - 1) (gameString ++ "\n" ++ show m)

-- Recursively makes the moves.
-- Left: A normal move was made.
-- Right: A winning move was made.
checkRandomMove
    :: Maybe (Either State State) -> StdGen -> Int -> String -> IO String
checkRandomMove (Just (Left _)) _ 0 gs = return gs -- n has reached 0
checkRandomMove (Just (Left s)) rGen n gs =
    makeRandomMove (snd (random rGen :: (Int, StdGen))) s n gs
checkRandomMove (Just (Right _)) _ _ gs = return gs

-- Tries to make a move with a given card.
-- Cards in hand -> Player pieces -> Cards to play -> Movesets for card -> Pieces to try
tryMove
    :: StdGen -> Cards -> Pieces -> Cards -> [Position] -> Pieces -> Maybe Move
    -- No movesets left to try
tryMove rGen hand pA c@[ca, cb] [] (p : ps) =
    tryMove rGen hand pA [cb] (makeMoveSet rGen cb) (p : ps)
    -- No cards left to try
tryMove rGen hand pA [_] [] (p : ps) =
    tryMove rGen hand pA hand (makeMoveSet rGen (head hand)) ps
    -- Try all moveset
tryMove rGen hand pA c@(ca : _) (e : es) (p : ps)
    | validMove hand (p, addTup p e, ca) pA = Just (p, addTup p e, ca)
    | otherwise                             = tryMove rGen hand pA c es (p : ps)


-- Shuffle player pieces
shufflePieces :: StdGen -> Pieces -> Pieces
shufflePieces rGen ps = map (ps !!) indexes
  where
    indexes = randomRange rGen (length ps) n
    n       = length ps - 1

-- Shuffle player cards
shuffleCards :: StdGen -> Cards -> Cards
shuffleCards rGen cs = map (cs !!) indexes
  where
    indexes = randomRange rGen 2 n
    n       = length cs - 1

-- Make a list of possible end positions with a given card.
-- It takes the moveset of a card, and shuffles it.
makeMoveSet :: StdGen -> Card -> [Position]
makeMoveSet rGen c = map (getMoveSet c !!) (randomRange rGen n max)
  where
    n   = length (getMoveSet c)
    max = length (getMoveSet c) - 1
