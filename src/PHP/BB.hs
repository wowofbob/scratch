{-# LANGUAGE OverloadedStrings #-}
module PHP.BB where

import Prelude hiding (words, unwords, tail, filter)
import Data.Text hiding (Text, drop, init)
import Data.Text.Internal (Text)
import Data.Attoparsec.Text hiding (skipSpace, skip)
import Text.HTML.Parser
import Text.HTML.Parser.Tag

import qualified Data.Map.Lazy as M

import HTML


data Topic
  = Topic
  { topicTitle   :: Text
  , topicAuthor  :: Text
  , topicDate    :: Text
  , topicReplies :: Word
  , topicViews   :: Word
  , topicRef     :: Maybe Text
  } deriving Show

topic :: Parser Topic
topic = do 
  
  -- Gather information.
  mref    <- attr "href" <$> tagOpen "a" `after` tagOpen "dt"
  title   <- contentText
  author  <- contentText `after` tagOpen "a" `after` tagClose "strong"
  date    <- contentText `after` tagClose "a"
  replies <- contentText `after` tagOpen "dd"
  views   <- contentText `after` tagOpen "dd"
  
  -- Skip tokens until the end of topic section.
  skipUntil (tagClose "li")
  
  -- Format data to look pretty.
  let
            -- Drop '.' (it's not enouth yet)
    fmref = tail <$> mref
    
            -- Remove ','
    fdate = filter ((/=) ',') . 
              -- Assemble everything back
              unwords .
                -- Drop "&raquo;" and "Mon"
                drop 2 .
                  -- Drop "am"
                  init .
                    words $ date
    
    rcnt  = read . unpack $ replies
    vcnt  = read . unpack $ views
    
  
  pure $ Topic title author fdate rcnt vcnt fmref
       




data ForumPage
  = ForumPage
  { pageName   :: Text
  , pageTopics :: [Topic]
  , pageNumber :: Word
  , pageMap    :: M.Map Word URL
  }
