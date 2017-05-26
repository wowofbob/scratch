{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding
import Network.HTTP.Conduit
import Text.HTML.TagSoup


type URL = String


getTime
  -- Url to html with time.
  :: URL
  -- How to decode html.
  -> (ByteString -> Text)
  -- How to extract time.
  -> ([Tag Text] -> Maybe Text)
  -- Maybe return something.
  -> IO (Maybe Text)
getTime url decodeHtml parseTime =
  parseTime . parseTags . decodeHtml <$> simpleHttp url

parseTime :: [Tag Text] -> Maybe Text
parseTime tags =
  case getTimeTag tags of
    TagText time -> Just time
    _            -> Nothing
  where

    -- Is tag a division containing time?
    isTimeDiv (TagOpen name [(attr, val)]) =
      name == "div" && attr == "id" && val == "twd"
    isTimeDiv _ = False
    
    -- Extract time tag.
    getTimeTag =
      head . tail . dropWhile (not . isTimeDiv)


main :: IO ()
main = getTime "https://time.is" decodeUtf8 parseTime >>= print
