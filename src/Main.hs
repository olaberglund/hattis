module Main where

import           Control.Arrow (second, (>>>))
import           Data.Map      (Map)
import qualified Data.Map      as Map

type Lemonade = String
type Litres = Float

data Classmate = Classmate
    { id      :: Int
    , offered :: Lemonade
    , wanted  :: Lemonade
    , rate    :: Litres
    }
    deriving (Eq, Ord)

-- At this point/classmate, it's possible
-- to have X litres of lemonade Y.
type Inventory = Map Classmate (Map Lemonade Litres)

main :: IO ()
main = putStrLn ""

test :: String -> c
test =
    lines
        >>> tail
        >>> zip [1 ..]
        >>> map ((\(i, [o, w, r]) -> Classmate i o w (read r)) . second words)
        >>> undefined

calcTrades :: [Classmate] -> Inventory
calcTrades cs = Map.empty
  where
    recordTrade :: Classmate -> Inventory -> Inventory
    recordTrade cm = Map.adjust (trade cm) cm

    trade :: Classmate -> Map Lemonade Litres -> Map Lemonade Litres
    trade cm = Map.adjust (\litres -> litres * cm.wan) cm.offered
