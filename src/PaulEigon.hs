module PaulEigon where

import Control.Arrow ((>>>))

data Player = Paul | Opponent

main :: IO ()
main =
  interact $
    lines
      >>> map (map read . words)
      >>> (\[[n, p, q]] -> paul n p q)
      >>> \p -> if p then "paul" else "opponent"

paul :: Int -> Int -> Int -> Bool
paul n p q = even ((p + q) `div` n)
