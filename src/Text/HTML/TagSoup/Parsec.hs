module Text.HTML.TagSoup.Parsec where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.StringLike


type TagParser s = Parsec [Tag s] ()

tagText :: (Show s, StringLike s) => TagParser s (Tag s)
tagText = tokenPrim showTag nextTag testTag
  where
    showTag = show
    nextTag = undefined
    testTag = undefined
