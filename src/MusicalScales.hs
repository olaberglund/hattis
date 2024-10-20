{-# LANGUAGE LambdaCase #-}

module MusicalScales where

import           Control.Arrow ((>>>))
import           Data.Foldable (toList)
import           Data.List     (scanl')
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set      (Set)
import qualified Data.Set      as Set

notes :: Seq String
notes = Seq.fromList ["A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"]

scales :: Map String (Set String)
scales =
    Map.fromList
        . toList
        . Seq.mapWithIndex
            ( \i n ->
                (n, Set.fromList $ Seq.index notes <$> indices i)
            )
        $ notes
  where
    indices i = map (`mod` len) (scanl' (flip ($)) i [id, tone, tone, semi, tone, tone, tone, semi])
    len = Seq.length notes
    semi = (+ 1)
    tone = semi . semi

main :: IO ()
main =
    interact $
        lines
            >>> drop 1
            >>> head
            >>> words
            >>> Set.fromList
            >>> (\s -> Map.filter (s `Set.isSubsetOf`) scales)
            >>> Map.keys
            >>> \case
                [] -> "none"
                ss -> unwords ss
