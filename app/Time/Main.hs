module Main where

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Conduit
import Text.HTML.TagSoup


get :: String -> IO String
get url = simpleHTTP

-- https://time.is/
url :: String
url = "https://www.google.ru/search?q=current+time"

getHTML :: IO ByteString
getHTML = get url

main :: IO ()
main = getHTML >>= print
  --tags <- parseTags <$> getHTML
  --print tags 
  {-
    dropWhile (\ tag ->
      case tag of
        TagOpen name _ -> name /= "body"
        _              -> True) tags-}
