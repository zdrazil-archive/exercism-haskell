{-# LANGUAGE OverloadedStrings #-}

module DNA
  ( toRNA
  )
where

newtype DNA = DNA String

isDnaNucleotide :: Char -> Bool
isDnaNucleotide a = a `elem` ("GCTA" :: String)

getInvalidNucleotides :: String -> String
getInvalidNucleotides = filter (not . isDnaNucleotide)

dnaFromString :: String -> Either Char DNA
dnaFromString xs =
  let invalids = getInvalidNucleotides xs
  in  case invalids of
        [] -> Right $ DNA xs
        _  -> Left . head $ invalids


stringFromDna :: DNA -> String
stringFromDna (DNA s) = s

rnaFromDna :: DNA -> String
rnaFromDna = map dnaToRnaNucleotide . stringFromDna

dnaToRnaNucleotide :: Char -> Char
dnaToRnaNucleotide a = case a of
  'G' -> 'C'
  'C' -> 'G'
  'T' -> 'A'
  'A' -> 'U'
  _   -> a


toRNA :: String -> Either Char String
toRNA xs = case dnaFromString xs of
  Left  symbol -> Left symbol
  Right dna    -> Right $ rnaFromDna dna
