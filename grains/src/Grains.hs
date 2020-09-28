module Grains
  ( square
  , total
  )
where

getGrains :: [Integer]
getGrains = 1 : take 63 [ 2 ^ x | x <- [1 :: Integer ..] ]

square :: Integer -> Maybe Integer
square n | n > 64 || n < 1 = Nothing
         | otherwise       = Just $ getGrains !! fromIntegral (n - 1)

total :: Integer
total = sum getGrains
