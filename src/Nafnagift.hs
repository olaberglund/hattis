module Nafnagift where

import Control.Arrow ((>>>))
import Data.Either (isLeft)
import Data.Foldable (maximumBy)
import Data.Ord (comparing)

main :: IO ()
main =
  interact $
    lines >>> (\[m, n] -> scs m n) >>> combine

scs :: (Eq a) => [a] -> [a] -> [Either a a]
scs [] ys = Right <$> ys
scs xs [] = Right <$> xs
scs (x : xs) (y : ys)
  | x == y = Left x : scs xs ys
  | otherwise =
      maximumBy
        (comparing (length . filter isLeft))
        [Right y : scs (x : xs) ys, Right x : scs xs (y : ys)]

combine :: [Either a a] -> [a]
combine = map (either id id)
