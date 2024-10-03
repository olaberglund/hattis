{-# LANGUAGE TupleSections #-}

module Hardware where

import Control.Arrow
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String

type Counter = M.Map Char Int

put :: Counter -> Char -> Counter
put m c = M.insertWith (+) c 1 m

base :: Counter
base = M.fromList $ map (,0) ['0' .. '9']

newtype Street = Street String deriving (Show)

newtype Tally = Tally String deriving (Read, Show)

type Addresses = [String]

data Order = Order Street Tally Addresses deriving (Show)

main :: IO ()
main =
  interact $
    lines >>> drop 1 >>> unlines >>> solve
  where
    solve os = case parse (many order) "" os of
      Left err -> show err
      Right ords -> concatMap output ords

output :: Order -> String
output (Order (Street street) (Tally tally) addresses) =
  unlines $
    [ street,
      tally
    ]
      ++ map (\(d, c) -> "Make " <> show c <> " digit " <> [d]) (M.assocs counts)
      ++ ["In total " <> if nbrDigits == 1 then "1 digit" else show nbrDigits <> " digits"]
  where
    counts = foldl put base (concat addresses)
    nbrDigits = M.foldr (+) 0 counts

order :: Parser Order
order = Order <$> street <*> tally <*> addresses

street :: Parser Street
street = Street <$> manyTill (letter <|> char ' ') newline

addresses :: Parser [String]
addresses = concat <$> many ((single <|> range) <* newline)

tally :: Parser Tally
tally = Tally <$> manyTill anyChar newline

range :: Parser [String]
range = do
  char '+'
  [from, to, step] <- count 3 (space *> (read <$> number))
  return $ map show $ enumFromThenTo from (from + step) to

number :: Parser String
number = many1 digit

single :: Parser [String]
single = pure <$> number
