module System.IO.TextReader where

import DotNet
import qualified System.MarshalByRefObject
import System.Array
import System.Char

data TextReader_ a
type TextReader a = System.MarshalByRefObject.MarshalByRefObject (TextReader_ a)

foreign import dotnet
  "method System.IO.TextReader.ReadLine"
  readLine :: TextReader obj -> IO (String)

foreign import dotnet
  "method System.IO.TextReader.ReadBlock"
  readBlock :: System.Array.Array (System.Char.Char a0) -> Int -> Int -> TextReader obj -> IO (Int)

foreign import dotnet
  "method System.IO.TextReader.ReadToEnd"
  readToEnd :: TextReader obj -> IO (String)

foreign import dotnet
  "method System.IO.TextReader.Read"
  read :: System.Array.Array (System.Char.Char a0) -> Int -> Int -> TextReader obj -> IO (Int)

foreign import dotnet
  "method System.IO.TextReader.Read"
  read_1 :: TextReader obj -> IO (Int)

foreign import dotnet
  "method System.IO.TextReader.Peek"
  peek :: TextReader obj -> IO (Int)

foreign import dotnet
  "method System.IO.TextReader.Close"
  close :: TextReader obj -> IO (())

foreign import dotnet
  "static method System.IO.TextReader.Synchronized"
  synchronized :: System.IO.TextReader.TextReader a0 -> IO (System.IO.TextReader.TextReader a1)


