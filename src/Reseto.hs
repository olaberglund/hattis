{-# LANGUAGE TypeApplications #-}

module Reseto where

import Data.List (unfoldr)
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

-- an alternative solution, from: https://discourse.haskell.org/t/how-lazy-is-this-kattis-solution-reseto/8090/5
-- TODO: grok unfoldr

reseto' :: Int -> Int -> Int
reseto' n k = V.concat (unfoldr step (V.enumFromTo 2 n)) V.! (k - 1)
  where
    step xs
      | V.null xs = Nothing
      | otherwise = Just $ V.partition ((== 0) . (`mod` V.head xs)) xs
