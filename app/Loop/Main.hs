{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Prelude hiding (read)

import Control.Concurrent
import Data.Sequence
import Data.Int
import System.TimeIt


class Filter input state where
  filter_ :: input -> state -> input

data Input
  = Start
  | Pause
  | Stop
  | Restart

data State
  = Running
  | Waiting

data Status
  = Status
  { statusError :: Bool
  , statusState :: State
  }

instance Filter Input Status where
  filter_ Restart _ = Restart
  filter_ i       s =
    case statusError s of
      True  -> Pause
      False -> i

data Do   what
data Done what

data Cmd
  = Run
  | Wait

type Request  a = Do a 
type Response a = Done a

class Monad s => Server s where
  request :: Request a -> s (Response a)

main :: IO ()
main = pure ()
