module Lib (
 generateRandom,
 isValid,
 hasWinningStrategy
 ) where

import System.IO
import Control.Monad
import System.Random
import Data.List
import Cards

data Board = Board State

data State = State PlayerState PlayerState

data PlayerState = PlayerState Position Position Position Position Position

data Position = Position Int Int deriving (Eq, Ord)

data Moves = Moves [Position] deriving (Eq, Ord, Enum)


generateRandom :: Int -> Int -> IO (String)
-- Takes an integer seed, an integer n, and outputs a string representing a game with n moves computed randomly.
-- generateRandom _ _ = do
    -- Get random card
    -- deck <- shuffleCards
    -- Make n moves using some cards
    -- putStrLn $ take 1 deck

-- 
-- generateRandom seed n = randoms (Cobra,Monkey) (mkStdGen seed) -- 
generateRandom _ _ = return "Not yet implemente"
-- Output string representing a game with n moves computed randomly


isValid :: FilePath -> IO (String)
-- 
isValid _ = return "Not yet implemented"

hasWinningStrategy :: Int -> FilePath -> IO (String)
-- Filepath is a the game state.
-- Should return FirstPlayer if the first player has a winning strategy with less than n (Int) moves. None if none of the players has a winning move.
hasWinningStrategy _ _ = return "Not yet implemented"


makeMove :: Position -> Position -> Card -> Position
makeMove _ _ _ = Position 1 1
