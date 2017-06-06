{-# LANGUAGE OverloadedStrings #-}
module PHP.BB where

import Data.Text.Internal (Text)
import Data.Attoparsec.Text hiding (skipSpace, skip)
import Text.HTML.Parser
import Text.HTML.Parser.Tag

import qualified Data.Map.Lazy as M

import HTML


data Topic
  = Topic
  { topicName    :: Text
  , topicAuthor  :: Text
  , topicDate    :: Text
  , topicReplies :: Word
  , topicViews   :: Word
  , topicUrl     :: URL
  }

topic :: Parser Text
topic =
  do skip $ tagOpen' "li"
     skip $ tagOpen' "dl"
     skip $ tagOpen' "dt"
     href <- attr "href" <$> tagOpen "a"
     name <- contentText
     skip $ tagClose' "a"
     skip $ tagOpen' "br"
     
     skip $ tagOpen' "strong"
     _ <- manyTill anyToken (tagClose "strong")
     
     skip $ contentText
     skip $ tagOpen' "a"
     author <- contentText
     skip $ tagClose' "a"
     date <- contentText
     
     _ <- manyTill anyToken (tagClose "dt")
     _ <- manyTill anyToken (tagOpen "dd")
     replies <- contentText
     
     _ <- manyTill anyToken (tagOpen "dd")
     views <- contentText
     
     pure date
     




data ForumPage
  = ForumPage
  { pageName   :: Text
  , pageTopics :: [Topic]
  , pageNumber :: Word
  , pageMap    :: M.Map Word URL
  }
