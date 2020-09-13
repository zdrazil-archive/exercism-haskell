{-# LANGUAGE OverloadedStrings #-}

module DNA
  ( toRNA
  )
where

newtype DNANucleotide = DNANucleotide Char deriving (Show)
type DNA = [DNANucleotide]

newtype RNANucleotide = RNANucleotide Char deriving (Show)

isDnaNucleotide :: Char -> Bool
isDnaNucleotide a = a `elem` ("GCTA" :: String)

dnaNucleotideFromChar :: Char -> Either Char DNANucleotide
dnaNucleotideFromChar n | isDnaNucleotide n = Right $ DNANucleotide n
                        | otherwise         = Left n

dnaFromString :: String -> Either Char DNA
dnaFromString = mapM dnaNucleotideFromChar

dnaToRnaChar :: Char -> Char
dnaToRnaChar x = case x of
  'G' -> 'C'
  'C' -> 'G'
  'T' -> 'A'
  'A' -> 'U'


dnaToRnaNucleotide :: DNANucleotide -> RNANucleotide
dnaToRnaNucleotide = RNANucleotide . dnaToRnaChar . head . show


toRNA :: String -> Either Char String
-- toRNA xs = undefined
toRNA xs = case dnaFromString xs of
  Left  symbol -> Left symbol
  Right dna    -> Right . show . map dnaToRnaNucleotide $ dna
