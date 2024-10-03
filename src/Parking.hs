-- https://open.kattis.com/problems/parking2
module Parking where

import Control.Arrow ((>>>))
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.List (sort)
import Data.List.Split (chunksOf)

main :: IO ()
main =
  interact $
    lines
      >>> drop 1
      >>> map (map read . words)
      >>> chunksOf 2
      >>> map (show . solve)
      >>> unlines

solve :: [[Int]] -> Int
solve [_, stores] = 2 * (maximum stores - minimum stores)
