module Main where

import Data.Text.Lazy
import Data.Text.Lazy.Encoding

import qualified Data.ByteString.Lazy as B

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

import HTML
import PHP.BB



readExampleHtml :: IO B.ByteString
readExampleHtml = B.readFile "page-example.html"

main :: IO ()
main = readExampleHtml >>= print
