{-# LANGUAGE LambdaCase #-}

module TakeTwoStones where

data Parity = Even | Odd

main :: IO ()
main = interact (showName . parity . read)

parity :: Int -> Parity
parity n = if even n then Even else Odd

showName :: Parity -> String
showName = \case
  Odd -> "Alice"
  Even -> "Bob"
