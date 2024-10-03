module Password where

import Control.Arrow ((>>>))
import Data.List (sort, sortBy)

main :: IO ()
main =
  interact $
    lines
      >>> drop 1
      >>> map (read . last . words)
      >>> sortBy (flip compare)
      >>> zip [1 ..]
      >>> foldl solve 0
      >>> show

solve :: Double -> (Double, Double) -> Double
solve acc = (+ acc) . uncurry (*)
