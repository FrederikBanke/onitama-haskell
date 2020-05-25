module Helper
    ( unPack
    , isRight
    , randomRange
    , trd
    , addTup
    )
where

import           System.Random
import           Data.List

-- Take value out of Maybe.
unPack :: Maybe a -> a
unpack Nothing = 0
unPack (Just a) = a

isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True

randomRange :: StdGen -> Int -> Int -> [Int]
randomRange g n max = take n . nub $ randomRs (0, max) g

trd :: (a, b, c) -> b
trd (_, b, _) = b

addTup :: (Num a) => (a, a) -> (a, a) -> (a, a)
addTup (a, b) (x, y) = (a + x, b + y)
