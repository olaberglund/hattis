module Main where

import Control.Arrow ((>>>))
import Data.List (unfoldr)

main :: IO ()
main =
  interact $
    words >>> drop 1 >>> map read >>> solve

solve :: [Int] -> String
solve xs =
  let ds = diffs xs
   in unwords $ map show [length ds - 1, sum (map last ds)]
  where
    diffs xs | and $ zipWith (==) xs (tail xs) = [xs]
    diffs xs = xs : diffs (zipWith subtract xs (tail xs))
