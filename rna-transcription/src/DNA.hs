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

isValidRna :: String -> Bool
isValidRna = isValid rna


toRNA :: String -> Either Char String
toRNA xs = error "You need to implement this function."
