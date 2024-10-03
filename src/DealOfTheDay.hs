module DealOfTheDay where

import Control.Monad (replicateM)
import Data.List ((\\))

data Setup = Setup
  { cards :: [Integer],
    dealt :: Int
  }
  deriving (Show)

main :: IO ()
main = interact (show . solve . mkSetup . lines)

mkSetup :: [String] -> Setup
mkSetup [cs, nbr] =
  let cs' = read <$> words cs
   in Setup (filter (/= 0) cs') (read nbr)

solve :: Setup -> Integer
solve (Setup cs d) = sum $ map product (subsequencesOfSize d cs)

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs =
  let l = length xs
   in if n > l then [] else subsequencesBySize xs !! (l - n)
  where
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x : xs) =
      let next = subsequencesBySize xs
       in zipWith (++) ([] : next) (map (map (x :)) next ++ [[]])
