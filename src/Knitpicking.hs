{-# LANGUAGE LambdaCase #-}

module Knitpicking where

import           Data.Function (on)
import           Data.List     (groupBy, sort)
import           Prelude       hiding (Left, Right, even, odd)

data SockGroup = SockGroup {variant :: String, fit :: String, numOf :: Int}
    deriving (Show, Eq, Ord)

main :: IO ()
main = interact $ solve . (\(_ : groups) -> mkGroups groups) . lines

mkGroups :: [String] -> [SockGroup]
mkGroups = map ((\[v, f, n] -> SockGroup v f (read n)) . words)

solve :: [SockGroup] -> String
solve socks =
    if sum (map numOf socks) < guaranteed
        then "impossible"
        else show guaranteed
  where
    guaranteed = sum picked + 1
    picked = map (maximum . map pick) $ groupBy ((==) `on` variant) (sort socks)

pick :: SockGroup -> Int
pick = \case
    SockGroup _ "any" _ -> 1
    SockGroup _ _ n -> n
