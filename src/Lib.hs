module Lib
    ( generateRandom
    , isValid
    , hasWinningStrategy
    )
where

import           System.IO
import           Text.Read
import           Control.Monad
import           System.Random
import           Data.List
import           Cards
import           Moving
import           Helper
import           RandomGame


-- Checklist:
-- TODO: Check om det første move er et winning move. Giver altid Left, måske det skal være Right?
-- TODO: I random, så skal movesets genereres random hver gang.
-- TODO: Lav tests

type Game = (State, Moves)

-- Takes an integer seed, an integer n, and outputs a string representing a game with n moves computed randomly.
generateRandom :: Int -> Int -> IO (String)
generateRandom seed n = do
    -- Make a new state with a random deck of cards
    initState <- initialState . mkStdGen $ seed
    -- Make random move with generator
    makeRandomMove (mkStdGen seed) initState n (show initState)


-- Is the game valid
isValid :: FilePath -> IO (String)
-- Valid: Output string of the final state: (Cards, PiecesA, PiecesB)
isValid path = do
    handle   <- openFile path ReadMode
    contents <- hGetContents handle
    if isInitStateValid $ readMaybe $ head $ lines contents -- Check if initial state is valid
        then return $ playGame $ parseGame $ lines contents
        -- ParsingError if not valid format
        else return "ParsingError"

-- Can there be a winning strategy withing n moves.
hasWinningStrategy :: Int -> FilePath -> IO (String)
-- Filepath is a the game state.
-- Should return FirstPlayer if the first player has a winning strategy with less than n (Int) moves. None if none of the players has a winning move.
hasWinningStrategy n path = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle 
    let initState = read $ head $ lines contents -- get first line
    stdGen <- getStdGen
    makeRandomMove stdGen initState n (show initState)

-- Takes a list of strings.
-- The first element is the intial state.
-- The tail is the moves taken in the game.
parseGame :: [String] -> Game
parseGame (x : xs) = (read x, map read xs)

-- Play a game. Will return the final state of the game as a string
-- Will return the move that was not valid as a string if there is one.
playGame :: Game -> String
playGame (initState, []) = "ParsingError" -- If no moves, not a valid game.
-- Go through and make each move.
playGame (initState, moves) =
    unPack $ checkMoves (Just $ Left initState) moves (head moves) -- Returns the final state

-- Recursively makes the moves.
-- Nothing: A nonvalid move was made.
-- Left: A normal move was made.
-- Right: A winning move was made.
checkMoves :: Maybe (Either State State) -> Moves -> Move -> Maybe String
checkMoves (Just (Left  s)) []       _  = Just $ show s -- Non winning move
checkMoves (Just (Right s)) []       _  = Just $ show s -- Winning move
checkMoves (Just (Left  s)) (m : ms) _  = checkMoves (move s m) ms m -- Normal move
checkMoves (Just (Right s)) (m : _ ) _  = Just $ "NonValid " ++ show m -- Moves after win
checkMoves Nothing          _        lm = Just $ "NonValid " ++ show lm -- Invalid move

-- Check if initial game state is valid.
isInitStateValid :: Maybe State -> Bool
isInitStateValid (Just (cards, piecesA, piecesB)) =
    isValidCards cards && noOverlap (piecesA ++ piecesB) && all
        isWithinBoard
        (piecesA ++ piecesB)
isInitStateValid Nothing = False

-- Makes a new list with no duplicates.
-- Then checks if the length is the same.
noOverlap :: Pieces -> Bool
noOverlap pieces = (== length pieces) $ length $ nub pieces

-- Checks if given cards are part of the valid cards for the game
isValidCards :: [Card] -> Bool
isValidCards = foldl (\acc x -> (x `elem` validCards) && acc) True
