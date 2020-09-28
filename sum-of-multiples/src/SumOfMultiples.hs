module SumOfMultiples
  ( sumOfMultiples
  )
where

isMultiple :: Integer -> Integer -> Bool
isMultiple x factor = (factor /= 0) && (x `mod` factor == 0)

getMultiples :: Integer -> [Integer] -> [Integer]
getMultiples limit factors =
  [ x | x <- [1 .. limit - 1], any (isMultiple x) factors ]

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ getMultiples limit factors

