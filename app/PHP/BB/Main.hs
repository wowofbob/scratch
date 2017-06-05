{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.FileEmbed

import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding

import qualified Data.ByteString.Lazy as B

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Text.Parsec

import HTML
import PHP.BB
import Text.HTML.TagSoup.Parsec


exampleHtml :: Text
exampleHtml = $(embedStringFile "page-example.html")

main :: IO ()
main = do
  let res = parse runExampleParser "EXAMPLE" exampleParserInput
  print res

exampleParserInput :: [Tag Text]
exampleParserInput =
  [TagText "  \n "]

runExampleParser :: TagParser Text (Tag Text)
runExampleParser = tagSpace
