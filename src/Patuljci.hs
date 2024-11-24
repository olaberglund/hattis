module Patuljci where

import Control.Arrow ((>>>))
import Data.List ((\\))

main :: IO ()
main =
  interact $
    lines >>> map read >>> solve >>> map show >>> unlines

solve :: [Int] -> [Int]
solve xs = xs \\ concat (filter ((diff ==) . sum) pairs)
 where
  diff = sum xs - 100
  pairs = [[a, b] | a <- xs, b <- xs, a /= b]
