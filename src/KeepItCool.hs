module KeepItCool where

import           Control.Arrow ((>>>))
import           Control.Monad (guard)
import           Data.Foldable (toList)
import           Data.List     (foldl', sortOn, unfoldr)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq

main :: IO ()
main =
    interact $
        lines
            >>> map (map read . words)
            >>> (\[[n, m, s, d], slots] -> optimal n m s d (sortOn snd $ zip [0 ..] slots))
            >>> either id (unwords . map show . toList)

optimal :: Int -> Int -> Int -> Int -> [(Int, Int)] -> Either String (Seq Int)
optimal n m s d slots =
    if sum (drop (length placements) (snd <$> slots)) < m
        then Left "impossible"
        else Right refills
  where
    placements :: [(Int, Int)]
    placements =
        unfoldr
            ( \(left, slots') -> do
                ((i, colds) : rest) <- Just slots'
                guard (left /= 0)
                let placed = min left (d - colds)
                pure ((i, placed), (left - placed, rest))
            )
            (n, slots)

    refills =
        foldl'
            (\plcs (i, num) -> Seq.adjust (const num) i plcs)
            (Seq.replicate s 0)
            placements
