module Text.HTML.Parser.Tag where

import Prelude hiding (all)
import Control.Monad
import Data.Attoparsec.Text hiding (space, skipSpace)
import Data.Char
import Data.Text (unpack, all)
import Data.Text.Internal (Text)
import Text.HTML.Parser


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
--   * Comment
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

tagOpen :: Text -> Parser Token
tagOpen n =
  satisfyToken (isTagOpen n) <?> msg
  where
    msg = unwords ["tagOpen", unpack n]
    
tagClose :: Text -> Parser Token
tagClose n =
  satisfyToken (isTagClose n) <?> msg
  where
    msg = unwords ["tagClose", unpack n]

contentText :: Parser Token
contentText =
  satisfyToken isContentText <?> "contentText"

contentChar :: Parser Token
contentChar =
  satisfyToken isContentChar <?> "contentChar"

comment :: Parser Token
comment =
  satisfyToken isComment <?> "comment"

doctype :: Parser Token
doctype =
  satisfyToken isDoctype <?> "doctype"


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

skip :: Parser a -> Parser ()
skip p = p >> pure ()


-- * Space processing parsers.

-- Read token not containing valuable information:
--   * ContentText containing spaces
--   * ContentChar containing space
--   * Comment
space :: Parser Token
space =
  satisfyToken isSpaceToken <?> "space"

skipSpace :: Parser ()
skipSpace = skipMany space

ignoreSpace :: Parser a -> Parser a
ignoreSpace p = skipSpace >> p


-- * Parsers ignoring spaces.

tagOpen' :: Text -> Parser Token
tagOpen' = ignoreSpace . tagOpen

tagClose' :: Text -> Parser Token
tagClose' = ignoreSpace . tagClose

comment' :: Parser Token
comment' = ignoreSpace comment

doctype' :: Parser Token
doctype' = ignoreSpace doctype
