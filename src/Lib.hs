module Lib
    ( generateRandom
    , isValid
    , hasWinningStrategy
    )
where

import           System.IO
import           Control.Monad
import           System.Random
import           Data.List
import           Cards
import           Moving
import           Helper

type Game = (State, Moves)

initialState :: IO State
initialState = do
    cards <- shuffleCards
    return
        ( cards
        , [(0, 2), (0, 0), (0, 1), (0, 3), (0, 4)]
        , [(4, 2), (4, 0), (4, 1), (4, 3), (4, 4)]
        )


-- Takes an integer seed, an integer n, and outputs a string representing a game with n moves computed randomly.
generateRandom :: Int -> Int -> IO (String)
-- generateRandom rGen _ = do
-- initialize random generator with seed
-- Make random move with generator
-- generateRandom seed n = randoms (Cobra,Monkey) (mkStdGen seed) -- 

-- Should ouput format:
-- (Cards, PiecesA, PiecesB)    // The intial state of the board
-- (Start, End, Card)           // A move, the rest of the lines will also be moves
-- Output string representing a game with n moves computed randomly
generateRandom _ _ = return "Not yet implemented"


-- Is the game valid
isValid :: FilePath -> IO (String)
-- Valid: Output string of the final state: (Cards, PiecesA, PiecesB)
isValid path = do
    handle   <- openFile path ReadMode
    contents <- hGetContents handle
    -- mapM_ putStrLn $ lines contents -- #FIXME: Debugging
    if isInitStateValid $ read $ head $ lines contents -- Check if initial state is valid
        then return $ playGame $ parseGame $ lines contents
        -- ParsingError if not valid format
        else return "ParsingError"

-- Can there be a winning strategy withing n moves.
hasWinningStrategy :: Int -> FilePath -> IO (String)
-- Filepath is a the game state.
-- Should return FirstPlayer if the first player has a winning strategy with less than n (Int) moves. None if none of the players has a winning move.
hasWinningStrategy _ _ = return "Not yet implemented"

-- Takes a list of strings.
-- The first element is the intial state.
-- The tail is the moves taken in the game.
parseGame :: [String] -> Game
parseGame (x : xs) = (read x, map read xs)

-- Play a game. Will return the final state of the game as a string
-- Will return the move that was not valid as a string if there is one.
playGame :: Game -> String
playGame (initState, []   ) = show debugState -- FIXME: Skift ud med initState når jeg er færdig
-- Go through and make each move.
playGame (initState, moves) = unPack $ checkMoves (Just initState) moves 0 -- Returns the final state

-- Recursively makes the moves.
checkMoves :: Maybe State -> Moves -> Int -> Maybe String
checkMoves (Just s) []       _ = Just $ show s
checkMoves (Just s) (m : ms) n = checkMoves (move s m) ms (n + 1)
checkMoves Nothing  _        n = Just $ "NonValid " ++ show n


isInitStateValid :: State -> Bool
isInitStateValid (cards, piecesA, piecesB) =
    isValidCards cards && noOverlap (piecesA ++ piecesB)
isInitStateValid _ = False

-- Makes a new list with no duplicates.
-- Then checks if the length is the same.
noOverlap :: Pieces -> Bool
noOverlap pieces = (== length pieces) $ length $ nub pieces

-- Checks if given cards are part of the valid cards for the game
isValidCards :: [Card] -> Bool
isValidCards = foldl (\acc x -> (x `elem` validCards) && acc) True
