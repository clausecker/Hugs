module Dotnet.System.IO.TextReader where

import Dotnet
import qualified Dotnet.System.MarshalByRefObject
import Dotnet.System.Array
import Dotnet.System.Char

data TextReader_ a
type TextReader a = Dotnet.System.MarshalByRefObject.MarshalByRefObject (TextReader_ a)

foreign import dotnet
  "method Dotnet.System.IO.TextReader.ReadLine"
  readLine :: TextReader obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.IO.TextReader.ReadBlock"
  readBlock :: Dotnet.System.Array.Array (Dotnet.System.Char.Char a0) -> Int -> Int -> TextReader obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.IO.TextReader.ReadToEnd"
  readToEnd :: TextReader obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.IO.TextReader.Read"
  read :: Dotnet.System.Array.Array (Dotnet.System.Char.Char a0) -> Int -> Int -> TextReader obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.IO.TextReader.Read"
  read_1 :: TextReader obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.IO.TextReader.Peek"
  peek :: TextReader obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.IO.TextReader.Close"
  close :: TextReader obj -> IO (())

foreign import dotnet
  "static method Dotnet.System.IO.TextReader.Synchronized"
  synchronized :: Dotnet.System.IO.TextReader.TextReader a0 -> IO (Dotnet.System.IO.TextReader.TextReader a1)


