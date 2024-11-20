module PlantingTrees where

import Data.List (sortOn)
import Data.Ord (Down (Down))

main :: IO ()
main =
  interact $
    show
      . solve
      . map read
      . words
      . last
      . lines

solve :: [Int] -> Int
solve =
  (+ 1)
    . maximum
    . zipWith (+) [1 ..]
    . sortOn Down
