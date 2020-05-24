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

makeRandomMove :: StdGen -> State -> PlayerOneTurn -> Int -> String -> IO String
makeRandomMove rGen s@(cs@[c1, c2, c3, c4, _], pA, pB) pot n gameString = do
    let playerPieces = if pot then pA else pB
    let playerCards  = if pot then [c1, c2] else [c3, c4]
    let ps           = shufflePieces rGen playerPieces
    let pc           = shuffleCards rGen playerCards
    -- Choose move set
    let ends         = makeMoveSet rGen (head pc)
    -- Check move is valid, if not try again.
    if isNothing (tryMove pc pc ends ps)
        then return gameString -- No move could be made
        else do
            let m = unPack (tryMove pc pc ends ps)
            checkRandomMove (move s m pot)
                            rGen
                            (n - 1)
                            pot
                            (gameString ++ "\n" ++ show m)
    -- Make move and update state

-- Recursively makes the moves.
-- Left: A normal move was made.
-- Right: A winning move was made.
checkRandomMove
    :: Maybe (Either State State)
    -> StdGen
    -> Int
    -> PlayerOneTurn
    -> String
    -> IO String
checkRandomMove (Just (Left _)) _ 0 _ gs = return gs -- n has reached 0
checkRandomMove (Just (Left s)) rGen n pot gs =
    makeRandomMove rGen s (not pot) n gs
checkRandomMove (Just (Right s)) _ _ _ gs = return gs
checkRandomMove Nothing          _ _ _ gs = return gs

-- Tries to make a move with a given card.
-- Cards in hand -> Cards to play -> Movesets for card -> Player pieces
tryMove :: Cards -> Cards -> [Position] -> Pieces -> Maybe Move
tryMove _ _ _ [] = Nothing -- No pieces left to try
tryMove hand c@[ca, cb] [] (p : ps) = -- No movesets left to try
    tryMove hand [cb] (getMoveSet cb) (p : ps)
tryMove hand [_] [] (p : ps) = -- No cards left to try
    tryMove hand hand (getMoveSet (head hand)) ps
tryMove hand c@(ca : cb) (e : es) (p : ps)
    | -- Try all moveset
      validMove hand (p, e, ca) = Just (p, e, ca)
    | otherwise                 = tryMove hand c es (p : ps)



-- TODO: Lav den her
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
