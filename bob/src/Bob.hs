{-# LANGUAGE OverloadedStrings #-}
module Bob
  ( responseFor
  )
where

import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Data.Char                     as Char

data Category = ShoutQuestion | Silence | Question | Shout | Other

getResponse :: Category -> Text
getResponse Silence       = "Fine. Be that way!"
getResponse ShoutQuestion = "Calm down, I know what I'm doing!"
getResponse Question      = "Sure."
getResponse Shout         = "Whoa, chill out!"
getResponse Other         = "Whatever."

isUpper :: Text -> Bool
isUpper a | T.null alpha = False
          | otherwise    = T.all Char.isUpper alpha
  where alpha = T.filter Char.isAlpha a

categorizeStatement :: Text -> Category
categorizeStatement statement | isSilence             = Silence
                              | isShout && isQuestion = ShoutQuestion
                              | isQuestion            = Question
                              | isShout               = Shout
                              | otherwise             = Other
 where
  isQuestion = T.isSuffixOf "?" . T.filter (not . Char.isSpace) $ statement
  isShout    = isUpper statement
  isSilence  = T.all Char.isSpace statement

responseFor :: Text -> Text
responseFor = getResponse . categorizeStatement
