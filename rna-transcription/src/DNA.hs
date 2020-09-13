module DNA
  ( toRNA
  )
where

dnaToRnaChar :: Char -> Either Char Char
dnaToRnaChar a = case a of
  'G' -> Right 'C'
  'C' -> Right 'G'
  'T' -> Right 'A'
  'A' -> Right 'U'
  _   -> Left a

dnaToRnaString :: String -> Either Char [Char]
dnaToRnaString = mapM dnaToRnaChar

toRNA :: String -> Either Char String
toRNA xs = case dnaToRnaString xs of
  Left  symbol -> Left symbol
  Right dna    -> Right dna
