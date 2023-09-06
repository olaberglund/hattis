module Abracadabra where

main :: IO ()
main = interact (unlines . abracadabras . read)

abracadabras :: Int -> [String]
abracadabras = map ((<> " Abracadabra") . show) . enumFromTo 1
