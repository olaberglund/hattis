module Main where

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    words >>> drop 1 >>> map read >>> solve

solve :: [Int] -> String
solve xs =
  let (deg, next) = nextDegVal (0, 0) xs
   in unwords $ map show [deg, next]
  where
    nextDegVal :: (Int, Int) -> [Int] -> (Int, Int)
    nextDegVal (deg, lst) xs | and $ zipWith (==) xs (tail xs) = (deg, lst + last xs)
    nextDegVal (deg, lst) xs = nextDegVal (deg + 1, lst + last xs) (zipWith subtract xs (tail xs))
