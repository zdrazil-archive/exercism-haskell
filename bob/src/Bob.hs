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

data Category = ShoutQuestion | Silence | Question | Shout | Other

getResponse :: Category -> Text
getResponse Silence       = "Fine. Be that way!"
getResponse ShoutQuestion = "Calm down, I know what I'm doing!"
getResponse Question      = "Sure."
getResponse Shout         = "Whoa, chill out!"
getResponse Other         = "Whatever."

categorizeStatement :: Text -> Category
categorizeStatement a | isSilence             = Silence
                      | isShout && isQuestion = ShoutQuestion
                      | isQuestion            = Question
                      | isShout               = Shout
                      | otherwise             = Other
 where
  withoutSpaces = T.filter (not . Char.isSpace) a
  isQuestion    = T.isSuffixOf "?" withoutSpaces
  isShout       = isUpper withoutSpaces
  isSilence     = T.null withoutSpaces

responseFor :: Text -> Text
responseFor = getResponse . categorizeStatement
