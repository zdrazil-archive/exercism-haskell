{-# LANGUAGE OverloadedStrings #-}

module DNA
  ( toRNA
  )
where

isDnaNucleotide :: Char -> Bool
isDnaNucleotide a = elem a ("GCTA" :: String)

getInvalidNucleotides :: String -> String
getInvalidNucleotides a = filter (not . isDnaNucleotide) a

dnaToRnaNucleotide :: Char -> Char
dnaToRnaNucleotide a = case a of
  'G' -> 'C'
  'C' -> 'G'
  'T' -> 'A'
  'A' -> 'U'
  _   -> a

dnaToRna :: String -> String
dnaToRna = map dnaToRnaNucleotide

toRNA :: String -> Either Char String
toRNA xs =
  let invalids = getInvalidNucleotides $ xs
  in  case invalids of
        [] -> Right $ dnaToRna xs
        _  -> Left . head $ invalids
