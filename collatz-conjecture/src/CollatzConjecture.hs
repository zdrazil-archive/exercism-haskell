module CollatzConjecture
  ( collatz
  )
where

method :: Integer -> Integer -> (Integer, Integer)
method n count | n == 1    = (n, count)
               | even n    = method (n `div` 2) (count + 1)
               | odd n     = method ((3 * n) + 1) (count + 1)
               | otherwise = (n, count)

collatz :: Integer -> Maybe Integer
collatz n | n >= 1    = Just (snd $ method n 0)
          | otherwise = Nothing
