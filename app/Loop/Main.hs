module Main where

import Prelude hiding (read)

import Control.Concurrent
import Data.Sequence
import Data.Int
import System.TimeIt


-- Hide variable type.
class IOVar v where  
  read  :: v a -> IO a
  write :: a -> v a -> IO ()


-- Commands.
data Command
  = Start | Pause | Stop


-- State.
data State
  = Running | Waiting

-- Possible responces for commands.
data Action
  = Run | Wait | Exit

action :: Command -> State -> Action
action Start Running = Run
action Start Waiting = Run
action Pause Running = Wait
action Pause Waiting = Wait
action Stop  _       = Exit


loop
  :: IOVar v
  => v (Maybe Command)
  -> IO ()
  -> v State
  -> IO ()
loop ctl f state =
  do mCmd <- read ctl
     case mCmd of
       Nothing  -> run
       Just cmd ->
         do s <- read state
            case action cmd s of
              Run  -> run
              Wait -> wait
              Exit -> pure ()
  where
    
    run  = f >> wait
         
    wait = threadDelay 1000


main :: IO ()
main = pure ()
