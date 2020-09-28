module SumOfMultiples
  ( sumOfMultiples
  )
where

import           Data.List                      ( nub )

getMultiples :: Integer -> Integer -> [Integer]
getMultiples limit factor =
  [ x * factor | x <- [1 .. limit], x * factor < limit ]

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum . nub . concatMap (getMultiples limit) $ factors


-- takeWhile (\num -> num < b`div` a) [1, 2 ..]
