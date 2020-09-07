{-# LANGUAGE OverloadedStrings #-}

module DNA
  ( toRNA
  )
where

dna = "acgt"
rna = "acgu"

flippedElem :: String -> Char -> Bool
flippedElem = flip elem

isValid :: String -> String -> Bool
isValid strand a | null a    = False
                 | otherwise = all (flippedElem strand) a

isValidDna :: String -> Bool
isValidDna = isValid dna

dnaToRnaNucleotide :: Char -> Char
dnaToRnaNucleotide a = case a of
  'g' -> 'c'
  'c' -> 'g'
  't' -> 'a'
  'a' -> 'u'

dnaToRna :: String -> String
dnaToRna = map dnaToRnaNucleotide


toRNA :: String -> Either Char String
toRNA xs = error "You need to implement this function."
