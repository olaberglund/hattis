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
                    simulate
                        ants1
                        ants2
                        (read t)
                )

simulate :: [Ant] -> [Ant] -> Int -> [Ant]
simulate left right t = foldl' (\ants _ -> go ants) (reverse left ++ right) [1 .. t]
  where
    go [] = []
    go [x] = [x]
    go (x : y : xs)
        | x `elem` left && y `elem` right = y : x : go xs
        | otherwise = x : go (y : xs)

-- solve :: [Ant] -> [Ant] -> Int -> [Ant]
-- solve as1 as2 t = toList $ merge $ zip as1' as2'
--   where
--     merge :: [(Maybe Ant, Maybe Ant)] -> Seq Ant
--     merge = foldl' merge' empty
--
--     merge' :: Seq Ant -> (Maybe Ant, Maybe Ant) -> Seq Ant
--     merge' ants pair =
--         case pair of
--             (Just a1, Just a2) -> ants |> a2 |> a1
--             (Nothing, Just a)  -> ants |> a
--             (Just a, Nothing)  -> ants |> a
--             _                  -> ants
--
--     nothings = replicate (length as2) Nothing
--     as1' = nothings <> map pure (reverse as1) <> nothings
--     as2' = replicate (length as1 + length as2 - t) Nothing <> map pure as2 <> replicate t Nothing
