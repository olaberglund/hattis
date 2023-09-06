module Triarea where

main :: IO ()
main = interact (show . area . lengths)

lengths :: (Read a) => String -> (a, a)
lengths = tuple . map read . words
  where
    tuple [a,b] = (a,b)

area :: (Double, Double) -> Double
area = (/ 2) . uncurry (*)
