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

topic :: Parser Token
topic =
  do skipSpace
     skip $ tagOpen "li"
     skipSpace
     skip $ tagOpen "dl"
     skipSpace
     skip $ tagOpen "dt"
     skipSpace
     tagOpen "a"
     




data ForumPage
  = ForumPage
  { pageName   :: Text
  , pageTopics :: [Topic]
  , pageNumber :: Word
  , pageMap    :: M.Map Word URL
  }
