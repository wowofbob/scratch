{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.FileEmbed

import Data.Attoparsec.Text hiding (skipSpace, skip)
import Data.Text.Internal

import Text.HTML.Parser
import Text.HTML.Parser.Tag


exampleHtml :: Text
exampleHtml = $(embedStringFile "tree-example.html")

-- Parse replies count.
exampleParser :: Parser Int
exampleParser =
  do skipSpace
     skip $ tagOpen "li"
     skipSpace
     skip $ tagOpen "dl"
     skipSpace
     skip $ tagOpen "dt"
     pure 0

main :: IO ()
main = print $ parse exampleParser exampleHtml
