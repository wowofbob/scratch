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

topic :: Parser [Attr]
topic =
  do skip $ tagOpen' "li"
     skip $ tagOpen' "dl"
     skip $ tagOpen' "dt"
     tagOpen' "a"
     




data ForumPage
  = ForumPage
  { pageName   :: Text
  , pageTopics :: [Topic]
  , pageNumber :: Word
  , pageMap    :: M.Map Word URL
  }
