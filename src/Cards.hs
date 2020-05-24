module Cards
    ( Card
    , Cards
    , validCards
    , getMoveSet
    , shuffleCards
    , switchCards
    , sortCards
    )
where

import           System.IO
import           Control.Monad
import           System.Random
import           Data.List
import           Helper

-- Make a type synonym for a card.
type Card = String
type Cards = [Card]

validCards :: [Card]
validCards =
    [ "Boar"
    , "Cobra"
    , "Crab"
    , "Crane"
    , "Dragon"
    , "Eel"
    , "Elephant"
    , "Frog"
    , "Goose"
    , "Horse"
    , "Mantis"
    , "Monkey"
    , "Ox"
    , "Rabbit"
    , "Rooster"
    , "Tiger"
    ]

getMoveSet :: Card -> [(Int, Int)]
getMoveSet name | name == "Boar"     = [(0, -1), (1, 0), (0, 1)]
                | name == "Cobra"    = [(0, -1), (1, 1), (-1, 1)]
                | name == "Crab"     = [(0, -2), (1, 0), (0, 2)]
                | name == "Crane"    = [(-1, -1), (1, 0), (-1, 1)]
                | name == "Dragon"   = [(1, -2), (-1, -1), (-1, 1), (1, 2)]
                | name == "Eel"      = [(1, -1), (-1, -1), (0, 1)]
                | name == "Elephant" = [(1, -1), (0, -1), (0, 1), (1, 1)]
                | name == "Frog"     = [(0, 2), (1, 1), (-1, -1)]
                | name == "Goose"    = [(1, -1), (0, -1), (0, 1), (-1, 1)]
                | name == "Horse"    = [(0, -1), (1, 0), (-1, 0)]
                | name == "Mantis"   = [(1, -1), (-1, 0), (1, 1)]
                | name == "Monkey"   = [(1, -1), (-1, -1), (1, 1), (-1, 1)]
                | name == "Ox"       = [(1, 0), (0, 1), (-1, 0)]
                | name == "Rabbit"   = [(-1, -1), (1, 1), (0, 2)]
                | name == "Rooster"  = [(-1, -1), (0, -1), (0, 1), (1, 1)]
                | name == "Tiger"    = [(2, 0), (-1, 0)]
                | otherwise          = []

-- Shuffle a deck of cards
shuffleCards :: IO [Card]
shuffleCards = do
    g <- newStdGen -- Generate a new random generator
    let indexes = take 5 . nub $ randomRs (0, 15) g -- Makes a random range and filters duplicates and takes 5 elements from the resulting list
    return (map (validCards !!) indexes)

-- Switch cards when used
switchCards :: Cards -> Card -> Cards
switchCards cs@[c1, c2, c3, c4, c5] c
    | c == c1   = [c5 , c2 , c3 , c4 , c1]
    | c == c2   = [c1 , c5 , c3 , c4 , c2]
    | c == c3   = [c1 , c2 , c5 , c4 , c3]
    | c == c4   = [c1 , c2 , c3 , c5 , c4]

sortCards :: Cards -> Cards
sortCards [c1, c2, c3, c4, c5] = concat [sortPlayerCards c1 c2, sortPlayerCards c3 c4, [c5]]
    where sortPlayerCards ca cb = if ca < cb then [ca, cb] else [cb, ca]