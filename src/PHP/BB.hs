{-# LANGUAGE OverloadedStrings #-}
module PHP.BB where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Word
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.ParserCombinators.Parsec

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
  

isTopic :: TagTree Text -> Bool
isTopic (TagBranch "li" [("class", "row bg1")] _) = True
isTopic (TagBranch "li" [("class", "row bg2")] _) = True
isTopic _                                         = False


