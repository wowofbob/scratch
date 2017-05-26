module PHP.BB where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Word
import Text.HTML.TagSoup

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

data ForumPage
  = ForumPage
  { pageName   :: Text
  , pageTopics :: [Topic]
  , pageNumber :: Word
  , pageMap    :: M.Map Word URL
  }
