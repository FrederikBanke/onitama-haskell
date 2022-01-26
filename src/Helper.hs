module Helper
    ( unPack
    , randomRange
    , addTup
    )
where

import           System.Random
import           Data.List

-- Take value out of Maybe.
unPack :: Maybe a -> a
unPack (Just a) = a

randomRange :: StdGen -> Int -> Int -> [Int]
randomRange g n max = take n . nub $ randomRs (0, max) g

addTup :: (Num a) => (a, a) -> (a, a) -> (a, a)
addTup (a, b) (x, y) = (a + x, b + y)
