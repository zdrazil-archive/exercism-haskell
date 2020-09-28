module SumOfMultiples
  ( sumOfMultiples
  )
where

isMultiple :: Integer -> Integer -> Bool
isMultiple x factor = case factor of
  0 -> False
  _ -> x `mod` factor == 0

getMultiples :: Integer -> [Integer] -> [Integer]
getMultiples limit factors =
  [ x | x <- [1 .. limit - 1], any (isMultiple x) factors ]

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ getMultiples limit factors

