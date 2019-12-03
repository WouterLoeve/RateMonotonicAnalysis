module Main where
import Data.List
import Data.Char
import Data.Tuple
import Data.Function
import Test.QuickCheck
import System.Random

main :: IO ()
main = print $ isSched setB

-- (Period, Deadline, (Worst case) execution time)
setB :: (Integral a) => [(a, a, a)]
setB = [(2, 2, 1), (5, 5, 1), (7, 7, 2)]

calcRi :: (Integral a) => (a, a, a) -> [(a, a, a)] -> a -> a
calcRi (t, d, c) xs r1 = c + sum 
    [ceiling $ fromIntegral r1 / fromIntegral t1 | (t1, d1, c1) <- xs, t1 < t]

isSched :: Integral a => [(a, a, a)] -> Bool
isSched taskSet = and [fixedPoint (t, d, c) taskSet 0 <= d | (t, d, c) <- taskSet]

fixedPoint :: Integral t => (t, t, t) -> [(t, t, t)] -> t -> t
fixedPoint task xs r | result == r = result
                     | result /= r = fixedPoint task xs result
    where 
        result = calcRi task xs r