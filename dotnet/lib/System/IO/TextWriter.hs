module System.IO.TextWriter where

import DotNet
import qualified System.MarshalByRefObject
import qualified System.Array
import qualified System.Object
import qualified System.Decimal
import qualified System.Double
import qualified System.Single
import qualified System.UInt64
import qualified System.Int64
import qualified System.UInt32
import qualified System.Char
import qualified System.Text.Encoding
import qualified System.IFormatProvider

data TextWriter_ a
type TextWriter a = System.MarshalByRefObject.MarshalByRefObject (TextWriter_ a)

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine :: String -> System.Array.Array (System.Object.Object a1) -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_1 :: String -> System.Object.Object a1 -> System.Object.Object a2 -> System.Object.Object a3 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_2 :: String -> System.Object.Object a1 -> System.Object.Object a2 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_3 :: String -> System.Object.Object a1 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_4 :: System.Object.Object a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_5 :: String -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_6 :: System.Decimal.Decimal a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_7 :: System.Double.Double a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_8 :: System.Single.Single a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_9 :: System.UInt64.UInt64 a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_10 :: System.Int64.Int64 a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_11 :: System.UInt32.UInt32 a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_12 :: Int -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_13 :: Bool -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_14 :: System.Array.Array (System.Char.Char a0) -> Int -> Int -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_15 :: System.Array.Array (System.Char.Char a0) -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_16 :: Char -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.WriteLine"
  writeLine_17 :: TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write :: String -> System.Array.Array (System.Object.Object a1) -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_1 :: String -> System.Object.Object a1 -> System.Object.Object a2 -> System.Object.Object a3 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_2 :: String -> System.Object.Object a1 -> System.Object.Object a2 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_3 :: String -> System.Object.Object a1 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_4 :: System.Object.Object a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_5 :: String -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_6 :: System.Decimal.Decimal a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_7 :: System.Double.Double a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_8 :: System.Single.Single a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_9 :: System.UInt64.UInt64 a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_10 :: System.Int64.Int64 a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_11 :: System.UInt32.UInt32 a0 -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_12 :: Int -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_13 :: Bool -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_14 :: System.Array.Array (System.Char.Char a0) -> Int -> Int -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_15 :: System.Array.Array (System.Char.Char a0) -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Write"
  write_16 :: Char -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.set_NewLine"
  set_NewLine :: String -> TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.get_NewLine"
  get_NewLine :: TextWriter obj -> IO (String)

foreign import dotnet
  "method System.IO.TextWriter.get_Encoding"
  get_Encoding :: TextWriter obj -> IO (System.Text.Encoding.Encoding a0)

foreign import dotnet
  "method System.IO.TextWriter.Flush"
  flush :: TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.Close"
  close :: TextWriter obj -> IO (())

foreign import dotnet
  "method System.IO.TextWriter.get_FormatProvider"
  get_FormatProvider :: TextWriter obj -> IO (System.IFormatProvider.IFormatProvider a0)

foreign import dotnet
  "static method System.IO.TextWriter.Synchronized"
  synchronized :: System.IO.TextWriter.TextWriter a0 -> IO (System.IO.TextWriter.TextWriter a1)


