module DNA
  ( nucleotideCounts
  , Nucleotide(..)
  )
where

import           Data.Map                       ( Map
                                                , fromList
                                                )
import           Data.List

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

charToNucleotide :: Char -> Either Char Nucleotide
charToNucleotide a = case a of
  'A' -> Right A
  'C' -> Right C
  'G' -> Right G
  'T' -> Right T
  _   -> Left a

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = case mapM charToNucleotide xs of
  Left invalidNucleotide -> Left [invalidNucleotide]
  Right nucleotides ->
    Right
      . fromList
      . map (\a -> (head a, length a))
      . group
      . sort
      $ nucleotides
