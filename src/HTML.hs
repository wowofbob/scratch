module HTML where

import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Network.HTTP.Conduit (simpleHttp)

type URL = String

retrieve
  :: MonadIO m
  -- How to extract value from html text.
  => (Text -> Maybe a)
  -- How to decode html.
  -> (ByteString -> Text)
  -- Url to html.
  -> URL
  -- Value retrieving may fail.
  -> m (Maybe a)
retrieve parse decodeHtml url =
  parse . decodeHtml <$> simpleHttp url

