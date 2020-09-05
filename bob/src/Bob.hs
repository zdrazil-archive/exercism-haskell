{-# LANGUAGE OverloadedStrings #-}
module Bob
  ( responseFor
  )
where

import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Data.Char                     as Char

isUpper :: Text -> Bool
isUpper a | alphaChars == "" = False
          | otherwise        = T.all Char.isUpper alphaChars
  where alphaChars = T.filter Char.isAlpha a
-- responseFor a = T.init a

responseFor :: Text -> Text
responseFor a | T.null withoutSpaces = "Fine. Be that way!"
              | isU && isQuestion    = "Calm down, I know what I'm doing!"
              | isQuestion           = "Sure."
              | isU                  = "Whoa, chill out!"
              | otherwise            = "Whatever."
 where
  withoutSpaces = T.filter (not . Char.isSpace) a
  isQuestion    = T.isSuffixOf "?" withoutSpaces
  isU           = isUpper withoutSpaces


