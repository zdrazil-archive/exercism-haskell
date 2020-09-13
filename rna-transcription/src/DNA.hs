module DNA
  ( toRNA
  )
where

toRNA' :: Char -> Either Char Char
toRNA' a = case a of
  'G' -> Right 'C'
  'C' -> Right 'G'
  'T' -> Right 'A'
  'A' -> Right 'U'
  _   -> Left a

toRNA :: String -> Either Char String
toRNA = mapM toRNA'
