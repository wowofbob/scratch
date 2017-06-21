{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy
import Data.FileEmbed
import Data.Text

exampleJson :: ByteString
exampleJson = $(embedStringFile "app/SO/example.json")


data VM
  = VM
  { virtio :: Maybe Text
  , ide    :: Maybe Text
  , cores  :: Int
  , mem    :: Int
  } deriving Show

instance ToJSON VM where
  toJSON VM {..} = object
    [ "virtio" .= virtio
    , "ide"    .= ide
    , "cores"  .= cores
    , "mem"    .= mem
    ]

instance FromJSON VM where
  
  parseJSON (Object obj) =
    VM
      <$> obj .:? "virtio"
      <*> obj .:? "ide"
      <*> obj .:  "cores"
      <*> obj .:  "mem"
  
  parseJSON invalid = typeMismatch "VM" invalid


exampleVM :: VM
exampleVM = VM (Just "V") (Just "I") 1 0

main :: IO ()
main = print (decode (exampleJson) :: Maybe VM)
