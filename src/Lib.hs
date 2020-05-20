module Lib (
 generateRandom,
 isValid,
 hasWinningStrategy
 ) where

import System.IO
import Control.Monad
import System.Random
import Data.List

data Board = Board State

data State = State PlayerState PlayerState

data PlayerState = PlayerState Position Position Position Position Position

data Position = Position Int Int

-- Define all cards. They are bounded, and can be ordered and compared.
data Card = Boar | Cobra | Crab | Crane | Dragon | Eel | Elephant | Frog | Goose | Horse | Mantis | Monkey | Ox | Rabbit | Rooster | Tiger deriving (Eq, Ord, Bounded, Enum)
-- Make it possible to pick a random Card.
instance Random Card where
    random g = case randomR (fromEnum (minBound :: Card), fromEnum (maxBound :: Card)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')
-- Make it possible to show a Card
instance Show Card where
    show Boar = "Boar"
    show Cobra = "Cobra"
    show Crab = "Crab"
    show Crane = "Crane"
    show Dragon = "Dragon"
    show Eel = "Eel"
    show Elephant = "Elephant"
    show Frog = "Frog"
    show Goose = "Goose"
    show Horse = "Horse"
    show Mantis = "Mantis"
    show Monkey = "Monkey"
    show Ox = "Ox"
    show Rabbit = "Rabbit"
    show Rooster = "Rooster"
    show Tiger = "Tiger"

generateRandom :: Int -> Int -> IO (String)
-- Takes an integer seed, an integer n, and outputs a string representing a game with n moves computed randomly.

-- Make n moves using some cards

-- Get random card

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


-- Shuffle a deck of cards
shuffleCards :: IO [Card]
shuffleCards = do
    g <- newStdGen -- Generate a new random generator
    return $ take 5 . nub $ (randomRs (Boar, Monkey) g) -- Makes a random range and filters duplicates and takes 5 elements from the resulting list