module Dotnet.System.IO.TextWriter where

import Dotnet
import qualified Dotnet.System.MarshalByRefObject
import qualified Dotnet.System.Array
import qualified Dotnet.System.Object
import qualified Dotnet.System.Decimal
import qualified Dotnet.System.Double
import qualified Dotnet.System.Single
import qualified Dotnet.System.UInt64
import qualified Dotnet.System.Int64
import qualified Dotnet.System.UInt32
import qualified Dotnet.System.Char
import qualified Dotnet.System.Text.Encoding
import qualified Dotnet.System.IFormatProvider

data TextWriter_ a
type TextWriter a = Dotnet.System.MarshalByRefObject.MarshalByRefObject (TextWriter_ a)

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine :: String -> Dotnet.System.Array.Array (Dotnet.System.Object.Object a1) -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine_1 :: String -> Dotnet.System.Object.Object a1 -> Dotnet.System.Object.Object a2 -> Dotnet.System.Object.Object a3 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine_2 :: String -> Dotnet.System.Object.Object a1 -> Dotnet.System.Object.Object a2 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine_3 :: String -> Dotnet.System.Object.Object a1 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine_4 :: Dotnet.System.Object.Object a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine_5 :: String -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine_6 :: Dotnet.System.Decimal.Decimal a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine_7 :: Dotnet.System.Double.Double a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine_8 :: Dotnet.System.Single.Single a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine_9 :: Dotnet.System.UInt64.UInt64 a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine_10 :: Dotnet.System.Int64.Int64 a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine_11 :: Dotnet.System.UInt32.UInt32 a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine_12 :: Int -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine_13 :: Bool -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine_14 :: Dotnet.System.Array.Array (Dotnet.System.Char.Char a0) -> Int -> Int -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine_15 :: Dotnet.System.Array.Array (Dotnet.System.Char.Char a0) -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine_16 :: Char -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.WriteLine"
  writeLine_17 :: TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Write"
  write :: String -> Dotnet.System.Array.Array (Dotnet.System.Object.Object a1) -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Write"
  write_1 :: String -> Dotnet.System.Object.Object a1 -> Dotnet.System.Object.Object a2 -> Dotnet.System.Object.Object a3 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Write"
  write_2 :: String -> Dotnet.System.Object.Object a1 -> Dotnet.System.Object.Object a2 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Write"
  write_3 :: String -> Dotnet.System.Object.Object a1 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Write"
  write_4 :: Dotnet.System.Object.Object a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Write"
  write_5 :: String -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Write"
  write_6 :: Dotnet.System.Decimal.Decimal a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Write"
  write_7 :: Dotnet.System.Double.Double a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Write"
  write_8 :: Dotnet.System.Single.Single a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Write"
  write_9 :: Dotnet.System.UInt64.UInt64 a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Write"
  write_10 :: Dotnet.System.Int64.Int64 a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Write"
  write_11 :: Dotnet.System.UInt32.UInt32 a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Write"
  write_12 :: Int -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Write"
  write_13 :: Bool -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Write"
  write_14 :: Dotnet.System.Array.Array (Dotnet.System.Char.Char a0) -> Int -> Int -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Write"
  write_15 :: Dotnet.System.Array.Array (Dotnet.System.Char.Char a0) -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Write"
  write_16 :: Char -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.set_NewLine"
  set_NewLine :: String -> TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.get_NewLine"
  get_NewLine :: TextWriter obj -> IO (String)

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.get_Encoding"
  get_Encoding :: TextWriter obj -> IO (Dotnet.System.Text.Encoding.Encoding a0)

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Flush"
  flush :: TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.Close"
  close :: TextWriter obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.TextWriter.get_FormatProvider"
  get_FormatProvider :: TextWriter obj -> IO (Dotnet.System.IFormatProvider.IFormatProvider a0)

foreign import dotnet
  "static method Dotnet.System.IO.TextWriter.Synchronized"
  synchronized :: Dotnet.System.IO.TextWriter.TextWriter a0 -> IO (Dotnet.System.IO.TextWriter.TextWriter a1)


