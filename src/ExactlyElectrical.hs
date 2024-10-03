module ExactlyElectrical where

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines
      >>> map (map read . words)
      >>> solve

solve :: [[Int]] -> String
solve [[x1, y1], [x2, y2], [c]]
  | c < dx + dy || odd (c - dx - dy) = "N"
  | otherwise = "Y"
  where
    dx = abs (x1 - x2)
    dy = abs (y1 - y2)
