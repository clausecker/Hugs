module System.Console where

import System.ObjectTy
import Char

data Console_ a
type Console a = Object (Console_ a)

foreign import dotnet
  "static System.Console.Read"
  readChar :: IO Int

foreign import dotnet
  "static System.Console.ReadLine"
  readLine :: IO String

foreign import dotnet
  "static System.Console.Write"
  writeChar :: Char -> IO ()

foreign import dotnet
  "static System.Console.WriteLine"
  writeLine :: String -> IO ()


