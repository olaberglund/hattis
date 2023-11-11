{-# LANGUAGE TypeApplications #-}

module Reseto where

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

main :: IO ()
main = interact (show . solve . map read . words)

solve :: [Int] -> Int
solve [n, k] = cross (V.enumFromTo @Int 2 n) (k - 1)

cross :: Vector Int -> Int -> Int
cross xs k =
  let (yes, no) = V.partition ((== 0) . flip mod (V.head xs)) xs
      len = V.length yes
   in if k > len - 1
        then cross no (k - len)
        else yes ! k
