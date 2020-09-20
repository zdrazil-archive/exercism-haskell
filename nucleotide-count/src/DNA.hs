{-# LANGUAGE TupleSections #-}

module DNA
  ( nucleotideCounts
  , Nucleotide(..)
  )
where

import           Data.Map                       ( Map
                                                , fromListWith
                                                )

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

charToNucleotide :: Char -> Either Char Nucleotide
charToNucleotide a = case a of
  'A' -> Right A
  'C' -> Right C
  'G' -> Right G
  'T' -> Right T
  _   -> Left a

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = either (Left . (: []))
                             (Right . fromListWith (+) . map (, 1))
                             (mapM charToNucleotide xs)

