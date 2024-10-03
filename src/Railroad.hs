module Railroad where

import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    lines >>> map (solve . map read . words) >>> unlines

solve :: [Int] -> String
solve [x, y] = if even (x * 4 + y * 3) then "possible" else "impossible"
