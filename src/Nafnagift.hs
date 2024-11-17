module Nafnagift where

import Control.Arrow ((>>>))
import Data.Array (Array, Ix (..), listArray, (!))
import Data.Foldable (Foldable (toList), minimumBy)
import Data.Ord (comparing)
import Data.Sequence ((<|))
import qualified Data.Sequence as Seq

main :: IO ()
main = interact $ lines >>> (\[m, n] -> solve m n)

solve :: (Eq a) => [a] -> [a] -> [a]
solve m n = toList $ lcs (0, 0)
 where
  rng = ((0, 0), (length m, length n))
  m' = listArray (0, length m) m
  n' = listArray (0, length n) n

  lcs = memo rng $ \(i, j) ->
    let x = m' ! i
        y = n' ! j
        i' = i + 1
        j' = j + 1
     in if inRange rng (i', j')
          then
            if x == y
              then x <| lcs (i', j')
              else minimumBy (comparing length) [y <| lcs (i, j'), x <| lcs (i', j)]
          else Seq.fromList $ drop i m <> drop j n

tabulate :: (Ix i) => (i, i) -> (i -> e) -> Array i e
tabulate rng f = listArray rng (map f $ range rng)

memo :: (Ix i) => (i, i) -> (i -> e) -> i -> e
memo rng = (!) . tabulate rng
