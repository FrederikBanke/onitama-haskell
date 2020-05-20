module Cards (
--  cards
 ) where

import System.IO
import Control.Monad
import System.Random
import Data.List

-- Define all cards. They are bounded, and can be ordered and compared.
data Card = Card String deriving (Eq, Ord)

-- cards :: [Card]

getMoveSet :: (Num a) => String -> [(a,a)]
getMoveSet "Boar" = [(-1,0),(0,1),(1,0)]
getMoveSet "Cobra" = [(-1,0),(1,1),(1,-1)]
getMoveSet "Crab" = [(-2,0),(0,1),(2,0)]
getMoveSet "Crane" = [(-1,-1),(0,1),(1,-1)]
getMoveSet "Dragon" = [(-2,1),(-1,-1),(1,-1),(2,1)]
getMoveSet "Eel" = [(-1,1),(-1,-1),(1,0)]
getMoveSet "Elephant" = [(-1,1),(-1,0),(1,0),(1,1)]
getMoveSet "Frog" = [(2,0),(1,1),(-1,-1)]
getMoveSet "Goose" = [(-1,1),(-1,0),(1,0),(1,-1)]
getMoveSet "Horse" = [(-1,0),(0,1),(0,-1)]
getMoveSet "Mantis" = [(-1,1),(0,-1),(1,1)]
getMoveSet "Monkey" = [(-1,1),(-1,-1),(1,1),(1,-1)]
getMoveSet "Ox" = [(0,1),(1,0),(0,-1)]
getMoveSet "Rabbit" = [(-1,-1),(1,1),(2,0)]
getMoveSet "Rooster" = [(-1,-1),(-1,0),(1,0),(1,1)]
getMoveSet "Tiger" = [(0,2),(0,-1)]
