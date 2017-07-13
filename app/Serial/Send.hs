module Main where

import Data.ByteString (pack)
import Data.ByteString.Internal (c2w)

import System.Environment
import System.IO
import System.Hardware.Serialport


main :: IO ()
main = do
  args <- getArgs
  case args of
    
    [port, input'] ->
      let input = pack . map c2w $ input' in
        withSerial port defaultSerialSettings $ \ sp ->
          do flush sp
             sent <- send sp input
             putStrLn $ "Data: " ++ input'
             putStrLn $ "Sent: " ++ show sent
      
    _      -> do
      putStrLn "Error: serial port file path expected."
      putStrLn "Usage: serial <serial/port/file/path> <input>."
