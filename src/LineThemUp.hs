module LineThemUp where

import Control.Arrow ((>>>))

main :: IO ()
main = interact $ lines >>> tail >>> solve

solve :: [String] -> String
solve names
  | isOrderedBy (<) = "INCREASING"
  | isOrderedBy (>) = "DECREASING"
  | otherwise = "NEITHER"
 where
  isOrderedBy p = and $ zipWith p names (tail names)
