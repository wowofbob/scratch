{-# LANGUAGE OverloadedStrings #-}
module PHP.BB where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Data.Word
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.ParserCombinators.Parsec
import Text.StringLike

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
  


data WebPageParams s
  = WebPageParams
  { webPageDocType :: [Attribute s]
  , webPageHtml    :: [Attribute s]
  }
  

data WebPage s
  = WebPage
  { webPageParams  :: WebPageParams s
  , webPageHead    :: TagTree s
  , webPageBody    :: TagTree s
  }

webPage :: StringLike s => s -> WebPage s
webPage input = let
  tree = parseTree input
  in undefined

isTopic :: TagTree Text -> Bool
isTopic (TagBranch "li" [("class", "row bg1")] _) = True
isTopic (TagBranch "li" [("class", "row bg2")] _) = True
isTopic _                                         = False


