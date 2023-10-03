module FizzBuzz where

main :: IO ()
main = interact (solve . map read . words)

solve :: [Int] -> String
solve [x, y, n] = unlines $ map (fizzle x y) [1 .. n]

fizzle :: Int -> Int -> Int -> String
fizzle x y n
  | all ((== 0) . mod n) [x, y] = "FizzBuzz"
  | n `mod` x == 0 = "Fizz"
  | n `mod` y == 0 = "Buzz"
  | otherwise = show n
