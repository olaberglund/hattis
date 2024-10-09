module Kolone where

import           Control.Arrow ((>>>))
import           Data.List     (foldl')

type Ant = Char

main :: IO ()
main =
    interact $
        lines
            >>> tail
            >>> ( \[ants1, ants2, t] ->
                    solve
                        ants1
                        ants2
                        (read t)
                )

solve :: [Ant] -> [Ant] -> Int -> [Ant]
solve as1 as2 t = merge $ zip as1' as2'
  where
    merge :: [(Maybe Ant, Maybe Ant)] -> [Ant]
    merge = foldl' merge' []

    merge' :: [Ant] -> (Maybe Ant, Maybe Ant) -> [Ant]
    merge' ants pair =
        ants <> case pair of
            (Just a1, Just a2) -> [a2, a1]
            (Nothing, Just a)  -> [a]
            (Just a, Nothing)  -> [a]
            _                  -> []

    nothings = replicate (length as2) Nothing
    as1' = nothings <> map pure (reverse as1) <> nothings
    as2' = replicate (length as1 + length as2 - t) Nothing <> map pure as2 <> replicate t Nothing
