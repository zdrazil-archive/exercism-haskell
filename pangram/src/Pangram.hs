module Pangram
  ( isPangram
  )
where

import           Data.List                      ( sort
                                                , nub
                                                )
import           Data.Char                      ( isAscii
                                                , toLower
                                                , isLetter
                                                )

isPangram :: String -> Bool
isPangram text =
  let alpha = ['a' .. 'z']
      newText =
          nub . sort . map toLower . filter isLetter . filter isAscii $ text
  in  alpha == newText

