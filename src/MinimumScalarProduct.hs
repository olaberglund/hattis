module MinimumScalarProduct where

import Control.Arrow ((>>>))
import Data.List (sort, sortBy, unfoldr)
import Data.Ord (Down (Down), comparing)

main :: IO ()
main =
  interact $
    lines
      >>> tail
      >>> map (map (read :: String -> Int) . words)
      >>> unfoldr step
      >>> zip [1 ..]
      >>> map showCase
      >>> unlines

showCase :: (Int, Int) -> String
showCase (i, n) = "Case #" <> show i <> ": " <> show n

step :: [[Int]] -> Maybe (Int, [[Int]])
step [] = Nothing
step (_ : xs : ys : rest) = Just (sum $ zipWith (*) (sort xs) (sortBy (comparing Down) ys), rest)
