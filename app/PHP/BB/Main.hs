{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.FileEmbed

import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding

import qualified Data.ByteString.Lazy as B

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

import HTML
import PHP.BB



exampleHtml :: Text
exampleHtml = $(embedStringFile "page-example.html")

main :: IO ()
main =  print . take 4 . parseTags $ exampleHtml
