module ARationalSequenceTake3 where

data Dir = L | R

main :: IO ()
main =
  interact $
    unlines
      . map ((\[i, n] -> i <> " " <> pp (solve (read n))) . words)
      . tail
      . lines
 where
  pp :: (Int, Int) -> String
  pp (p, q) = show p <> "/" <> show q

path :: Int -> [Dir]
path = map ((\p -> if p then L else R) . even) . takeWhile (/= 1) . iterate (`div` 2)

fraction :: Dir -> (Int, Int) -> (Int, Int)
fraction R (p, q) = (p + q, q)
fraction L (p, q) = (p, p + q)

solve :: Int -> (Int, Int)
solve n = foldr fraction (1, 1) (path n)
