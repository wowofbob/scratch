{-# LANGUAGE FlexibleContexts #-}
module Text.HTML.TagSoup.Parsec where

import Data.Functor.Identity
import Data.Text.Lazy
import Data.Text.Lazy.Encoding

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.Parsec
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec
import Text.StringLike

import Debug.Trace


type TagParser s = Parsec [Tag s] ()


tagText :: (Show s, StringLike s) => TagParser s (Tag s)
tagText = do
  pos <- getPosition
  token showTag (const pos) testTag
  where
    showTag     = show
    testTag tag =
      case tag of
        TagText _ -> Just tag
        _         -> Nothing

tagSpace
  :: (Show s, StringLike s, Stream s Identity Char)
  => TagParser s (Tag s)
tagSpace = do
  
  -- Parse TagText
  t <- tagText
  
  let tagContents = fromTagText t
      isSpaces    = parse
                      -- Parse spaces until the end of the tag contents.
                      (spaces <* eof)
                      -- Internal error, Left part is not used later.
                      ""
                      tagContents
  
  case isSpaces of
    Left  _ -> fail msg
    Right _ -> pure t
  
  where
    msg = "expected TagText containing spaces"
