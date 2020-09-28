module SumOfMultiples
  ( sumOfMultiples
  )
where

import           Data.List                      ( nub )

getMultiples :: Integer -> Integer -> [Integer]
getMultiples limit factor = case factor of
  0 -> []
  _ -> [ x | x <- [1 .. limit - 1], x `mod` factor == 0 ]

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum . nub . concatMap (getMultiples limit) $ factors


-- takeWhile (\num -> num < b`div` a) [1, 2 ..]
