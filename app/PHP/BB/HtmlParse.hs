{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.FileEmbed

import Data.Attoparsec.Text hiding (skipSpace, skip)
import Data.Text (unpack)
import Data.Text.Internal

import Text.HTML.Parser
import Text.HTML.Parser.Tag

import PHP.BB

exampleHtml :: Text
exampleHtml = $(embedStringFile "tree-example.html")

-- Parse replies count.
exampleParser :: Parser Text
exampleParser = topic

main :: IO ()
main = print $ parse exampleParser exampleHtml
