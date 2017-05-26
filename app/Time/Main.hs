{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Text.HTML.TagSoup (Tag(TagOpen, TagText), parseTags)

import HTML


parseTime :: Text -> Maybe Text
parseTime htmlText =
  case getTimeTag htmlText of
    TagText time -> Just time
    _            -> Nothing
  where

    -- Is tag a division containing time?
    isTimeDiv (TagOpen name [(attr, val)]) =
      name == "div" && attr == "id" && val == "twd"
    isTimeDiv _ = False
    
    -- Extract time tag.
    getTimeTag =
      head . tail . dropWhile (not . isTimeDiv) . parseTags


main :: IO ()
main = retrieve parseTime decodeUtf8 "https://time.is" >>= print
