module Text.HTML.Parser.Tag where

import Prelude hiding (all)
import Control.Monad
import Data.Attoparsec.Text hiding (space, skipSpace)
import Data.Char
import Data.Text (unpack, all)
import Data.Text.Internal (Text)
import Data.Text.Lazy.Builder
import Text.HTML.Parser

import qualified Data.Map.Lazy as M


-- * Utility

type Attrs = M.Map Text Text

fromAttrs :: [Attr] -> Attrs
fromAttrs = foldl (\ m (Attr k v) -> M.insert k v m) M.empty 

attr :: Text -> Attrs -> Text
attr k = flip (M.!) k


-- * Matching functions.

isTagOpen :: Text -> Token -> Bool
isTagOpen n (TagOpen n' _) = n == n'
isTagOpen _ _              = False

isTagClose :: Text -> Token -> Bool
isTagClose n (TagClose n') = n == n'
isTagClose _ _             = False

isContentText :: Token -> Bool
isContentText (ContentText _) = True
isContentText _               = False

isContentChar :: Token -> Bool
isContentChar (ContentChar _) = True
isContentChar _               = False

isComment :: Token -> Bool
isComment (Comment _) = True
isComment _           = False

isDoctype :: Token -> Bool
isDoctype (Doctype _) = True
isDoctype _           = False

-- Token is a space token if it is a:
--   * ContentText containing spaces
--   * ContentChar containing space
isSpaceToken :: Token -> Bool
isSpaceToken t =
  case t of
    ContentText text -> all isSpace text
    ContentChar c    -> isSpace c
    _                -> False


-- * Tag parsers

-- Alias for 'token'.
anyToken :: Parser Token
anyToken = token

tagOpen :: Text -> Parser Attrs
tagOpen n =
  withToken
    (\ t -> case t of
      TagOpen n' attrs -> if n == n'
                            then Just (fromAttrs attrs)
                            else Nothing
      _                -> Nothing) <?> msg
  where
    msg = unwords ["tagOpen", unpack n]
    
tagClose :: Text -> Parser ()
tagClose n =
  satisfyToken (isTagClose n) >> pure () <?> msg
  where
    msg = unwords ["tagClose", unpack n]

contentText :: Parser Text
contentText =
  withToken
    (\ t -> case t of
      ContentText text -> Just text
      _                -> Nothing) <?> "contentText"

contentChar :: Parser Char
contentChar =
  withToken
    (\ t -> case t of
      ContentChar c -> Just c
      _             -> Nothing) <?> "contentChar"

comment :: Parser Builder
comment =
  withToken
    (\ t -> case t of
      Comment b -> Just b
      _         -> Nothing) <?> "comment"

doctype :: Parser Text
doctype =
  withToken
    (\ t -> case t of
      Doctype text -> Just text
      _            -> Nothing) <?> "doctype"


-- * Combinators.

-- Might be a problems with choices here:
-- This line 
--   t <- token
-- reads input from a stream. If first choice
-- fails, would the next one get the same input?
satisfyToken :: (Token -> Bool) -> Parser Token
satisfyToken p =
  do t <- token
     when (not (p t)) $ fail "satisfyToken"
     pure t

withToken :: (Token -> Maybe a) -> Parser a
withToken p =
  do t <- token
     case p t of
       Just a  -> pure a
       Nothing -> fail "withToken"

skip :: Parser a -> Parser ()
skip p = p >> pure ()


-- * Space processing parsers.

-- Read token not containing valuable information:
--   * ContentText containing spaces
--   * ContentChar containing space
space :: Parser Token
space =
  satisfyToken isSpaceToken <?> "space"

skipSpace :: Parser ()
skipSpace = skipMany space

ignoreSpace :: Parser a -> Parser a
ignoreSpace p = skipSpace >> p


-- * Parsers ignoring spaces.

tagOpen' :: Text -> Parser Attrs
tagOpen' = ignoreSpace . tagOpen

tagClose' :: Text -> Parser ()
tagClose' = ignoreSpace . tagClose

comment' :: Parser Builder
comment' = ignoreSpace comment

doctype' :: Parser Text
doctype' = ignoreSpace doctype
