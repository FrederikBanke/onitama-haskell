module Lib (
 generateRandom,
 isValid,
 hasWinningStrategy
 ) where

import System.IO
import Control.Monad
import System.Random

generateRandom :: Int -> Int -> IO (String)
generateRandom _ _ = return "Not yet implemented"

isValid :: FilePath -> IO (String)
isValid _ = return "Not yet implemented"

hasWinningStrategy :: Int -> FilePath -> IO (String)
hasWinningStrategy _ _ = return "Not yet implemented"

