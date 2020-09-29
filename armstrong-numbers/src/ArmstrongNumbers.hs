module ArmstrongNumbers
  ( armstrong
  )
where

import           Data.Char                      ( digitToInt )

armstrong :: Integral a => a -> Bool
armstrong a =
  let digits      = show . toInteger $ a
      sumOfDigits = sum . map ((^ length digits) . digitToInt) $ digits
  in  fromIntegral a == sumOfDigits
