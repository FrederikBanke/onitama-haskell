module Cards
    (
--  cards
    )
where

import           System.IO
import           Control.Monad
import           System.Random
import           Data.List

-- Define all cards. They are bounded, and can be ordered and compared.
newtype Card = Card String deriving (Eq, Ord, Show)

cards :: [Card]
cards =
    [ Card "Boar"
    , Card "Cobra"
    , Card "Crab"
    , Card "Crane"
    , Card "Dragon"
    , Card "Eel"
    , Card "Elephant"
    , Card "Frog"
    , Card "Goose"
    , Card "Horse"
    , Card "Mantis"
    , Card "Monkey"
    , Card "Ox"
    , Card "Rabbit"
    , Card "Rooster"
    , Card "Tiger"
    ]

getMoveSet :: Card -> [(Int, Int)]
getMoveSet (Card name) | name == "Boar"     = [(-1, 0), (0, 1), (1, 0)]
                       | name == "Cobra"    = [(-1, 0), (1, 1), (1, -1)]
                       | name == "Crab"     = [(-2, 0), (0, 1), (2, 0)]
                       | name == "Crane"    = [(-1, -1), (0, 1), (1, -1)]
                       | name == "Dragon" = [(-2, 1), (-1, -1), (1, -1), (2, 1)]
                       | name == "Eel"      = [(-1, 1), (-1, -1), (1, 0)]
                       | name == "Elephant" = [(-1, 1), (-1, 0), (1, 0), (1, 1)]
                       | name == "Frog"     = [(2, 0), (1, 1), (-1, -1)]
                       | name == "Goose" = [(-1, 1), (-1, 0), (1, 0), (1, -1)]
                       | name == "Horse"    = [(-1, 0), (0, 1), (0, -1)]
                       | name == "Mantis"   = [(-1, 1), (0, -1), (1, 1)]
                       | name == "Monkey" = [(-1, 1), (-1, -1), (1, 1), (1, -1)]
                       | name == "Ox"       = [(0, 1), (1, 0), (0, -1)]
                       | name == "Rabbit"   = [(-1, -1), (1, 1), (2, 0)]
                       | name == "Rooster" = [(-1, -1), (-1, 0), (1, 0), (1, 1)]
                       | name == "Tiger"    = [(0, 2), (0, -1)]

-- Shuffle a deck of cards
shuffleCards :: IO [Card]
shuffleCards = do
    g <- newStdGen -- Generate a new random generator
    let indexes = take 5 . nub $ randomRs (0, 15) g -- Makes a random range and filters duplicates and takes 5 elements from the resulting list
    return (map (cards !!) indexes)
