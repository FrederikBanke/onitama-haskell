module RandomGame
    ( initialState
    , makeRandomMove
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

initialState :: StdGen -> IO State
initialState rGen = do
    cards <- shuffleDeck rGen
    return
        ( sortCards cards
        , [(0, 2), (0, 0), (0, 1), (0, 3), (0, 4)]
        , [(4, 2), (4, 0), (4, 1), (4, 3), (4, 4)]
        )

makeRandomMove :: StdGen -> State -> Int -> String -> IO String
makeRandomMove rGen s@(cs@[c1, c2, _, _, _], pA, pB) n gameString = do
    let ps       = shufflePieces rGen pA
    let pc       = shuffleCards rGen [c1, c2]
    -- Choose move set
    let posToTry = makeMoveSet rGen (head pc)
    let moveToTry = tryMove pc pA pc posToTry ps
    -- Check move is valid, if not try again.
    if isNothing moveToTry
        then return gameString -- No move could be made
        else do
            let m = unPack moveToTry
            checkRandomMove (move s m)
                            rGen
                            (n - 1)
                            (gameString ++ "\n" ++ show m)
    -- Make move and update state

-- Recursively makes the moves.
-- Left: A normal move was made.
-- Right: A winning move was made.
checkRandomMove
    :: Maybe (Either State State) -> StdGen -> Int -> String -> IO String
checkRandomMove (Just (Left  _)) _    0 gs = return gs -- n has reached 0
checkRandomMove (Just (Left  s)) rGen n gs = makeRandomMove rGen s n gs
checkRandomMove (Just (Right _)) _    _ gs = return gs
checkRandomMove Nothing          _    _ gs = return gs

-- Tries to make a move with a given card.
-- Cards in hand -> Player pieces-> Cards to play -> Movesets for card -> Pieces to try
tryMove :: Cards -> Pieces -> Cards -> [Position] -> Pieces -> Maybe Move
tryMove _ _ _ _ [] = Nothing -- No pieces left to try
tryMove hand pA c@[ca, cb] [] (p : ps) = -- No movesets left to try
    tryMove hand pA [cb] (getMoveSet cb) (p : ps)
tryMove hand pA [_] [] (p : ps) = -- No cards left to try
    tryMove hand pA hand (getMoveSet (head hand)) ps
tryMove hand pA c@(ca : cb) (e : es) (p : ps)
    | -- Try all moveset
      validMove hand (p, e, ca) pA = Just (p, e, ca)
    | otherwise                    = tryMove hand pA c es (p : ps)



shufflePieces :: StdGen -> Pieces -> Pieces
shufflePieces rGen ps = map (ps !!) indexes
  where
    indexes = randomRange rGen (length ps) n
    n       = length ps - 1


shuffleCards :: StdGen -> Cards -> Cards
shuffleCards rGen cs = map (cs !!) indexes
  where
    indexes = randomRange rGen 2 n
    n       = length cs - 1
-- Make a list of possible end positions with a given card.
-- It takes the moveset of a card, and shuffles it.
makeMoveSet :: StdGen -> Card -> [Position]
makeMoveSet rGen c = map (getMoveSet c !!) (randomRange rGen n n)
    where n = length (getMoveSet c) - 1
