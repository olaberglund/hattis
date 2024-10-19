{-# LANGUAGE LambdaCase #-}

module MusicalScales where

import Control.Arrow ((&&&), (>>>))
import Data.List (mapAccumL)
import Data.Set (Set)
import qualified Data.Set as Set

data Note = A | ASharp | B | C | CSharp | D | DSharp | E | F | FSharp | G | GSharp
  deriving (Show, Eq, Ord, Enum, Read)

nextNote :: Note -> Note
nextNote GSharp = A
nextNote n = succ n

showNote :: Note -> String
showNote n = case n of
  ASharp -> "A#"
  CSharp -> "C#"
  DSharp -> "D#"
  FSharp -> "F#"
  GSharp -> "G#"
  _ -> show n

parseNote :: String -> Note
parseNote n = case n of
  "A#" -> ASharp
  "C#" -> CSharp
  "D#" -> DSharp
  "F#" -> FSharp
  "G#" -> GSharp
  _ -> read n

mkScale :: Note -> [Note]
mkScale n = snd $ mapAccumL (\cn s -> (s cn, s cn)) n scale
 where
  scale = [id, tone, tone, semi, tone, tone, tone, semi]
  tone = nextNote . nextNote
  semi = nextNote

notes :: [String] -> Set Note
notes = Set.fromList . map parseNote

allScales :: [(Note, Set Note)]
allScales = map (id &&& Set.fromList . mkScale) [A ..]

scales :: Set Note -> [Note]
scales ns = map fst $ filter (Set.isSubsetOf ns . snd) allScales

main :: IO ()
main =
  interact $
    lines
      >>> drop 1
      >>> head
      >>> words
      >>> notes
      >>> scales
      >>> ( \case
              [] -> ["none"]
              ns -> map showNote ns
          )
      >>> unwords
