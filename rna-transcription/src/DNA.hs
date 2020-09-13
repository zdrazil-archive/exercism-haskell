{-# LANGUAGE OverloadedStrings #-}

module DNA
  ( toRNA
  )
where

newtype DNANucleotide = DNANucleotide Char deriving (Show)
type DNA = [DNANucleotide]

newtype RNANucleotide = RNANucleotide Char

isDnaNucleotide :: Char -> Bool
isDnaNucleotide a = a `elem` ("GCTA" :: String)

dnaNucleotideFromChar :: Char -> Either Char DNANucleotide
dnaNucleotideFromChar n | isDnaNucleotide n = Right $ DNANucleotide n
                        | otherwise         = Left n

dnaNucleotideToChar :: DNANucleotide -> Char
dnaNucleotideToChar a = a

dnaFromString :: String -> Either Char DNA
dnaFromString = mapM dnaNucleotideFromChar

rnaFromDna :: DNA -> RNA
rnaFromDna = map dnaToRnaNucleotide

dnaToRnaN :: DNANucleotide -> Char -> RNANucleotide
dnaToRnaN a = (a : Char)

dnaToRnaNucleotide :: DNANucleotide -> RNANucleotide
dnaToRnaNucleotide a = case a of
  'G' -> 'C'
  'C' -> 'G'
  'T' -> 'A'
  'A' -> 'U'


toRNA :: String -> Either Char String
toRNA xs = case dnaFromString xs of
  Left  symbol -> Left symbol
  Right dna    -> Right $ rnaFromDna dna
