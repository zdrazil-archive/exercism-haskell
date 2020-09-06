module CollatzConjecture
  ( collatz
  )
where

next :: Integer -> Integer
next n | n == 1    = n
       | even n    = n `div` 2
       | odd n     = (3 * n) + 1
       | otherwise = n

calc :: Integer -> [Integer]
calc n = takeWhile (/= 1) . iterate next $ n

collatz :: Integer -> Maybe Integer
collatz n | n >= 1    = Just . fromIntegral . length . calc $ n
          | otherwise = Nothing
