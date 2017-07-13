module Main where

import Control.Concurrent
import Control.Monad

import Data.ByteString (unpack)
import Data.ByteString.Internal (w2c)

import System.Environment
import System.Hardware.Serialport

import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    
    [p, b'] ->
      case sequence . map readMaybe $ [b'] of
        Nothing -> do
          printError
          printUsage
        Just [b]  -> do
          withSerial p defaultSerialSettings (go 0 b)
          
    _      -> do
      printError
      printUsage
      
  where

    printError =
      putStrLn "Error: serial port file path expected."
    
    printUsage =
      putStrLn "Usage: serial <serial/port/file/path> <buffer-size>"
  
    go n b sp =
      do input <- map w2c . unpack <$> recv sp b
         if not (null input)
           then do
             putStrLn $ show n ++ ":Data: " ++ input
             sleep
             go (n + 1) b sp
           else do
             sleep
             go n b sp
      where
        sleep = threadDelay 10000
