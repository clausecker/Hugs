module System.IO.Stream where

import DotNet
import qualified System.MarshalByRefObject
import System.Byte
import System.Array
import System.Int64
import System.IO.SeekOrigin
{-
import System.IAsyncResult
import System.AsyncCallback
-}

data Stream_ a
type Stream a = System.MarshalByRefObject.MarshalByRefObject (Stream_ a)

foreign import dotnet
  "method System.IO.Stream.WriteByte"
  writeByte :: System.Byte.Byte a0 -> Stream obj -> IO (())

foreign import dotnet
  "method System.IO.Stream.Write"
  write :: System.Array.Array (System.Byte.Byte a0) -> Int -> Int -> Stream obj -> IO (())

foreign import dotnet
  "method System.IO.Stream.ReadByte"
  readByte :: Stream obj -> IO (Int)

foreign import dotnet
  "method System.IO.Stream.Read"
  read :: System.Array.Array (System.Byte.Byte a0) -> Int -> Int -> Stream obj -> IO (Int)

foreign import dotnet
  "method System.IO.Stream.SetLength"
  setLength :: System.Int64.Int64 a0 -> Stream obj -> IO (())

foreign import dotnet
  "method System.IO.Stream.Seek"
  seek :: System.Int64.Int64 a0 -> System.IO.SeekOrigin.SeekOrigin a1 -> Stream obj -> IO (System.Int64.Int64 a2)

{-
foreign import dotnet
  "method System.IO.Stream.EndWrite"
  endWrite :: System.IAsyncResult.IAsyncResult a0 -> Stream obj -> IO (())

foreign import dotnet
  "method System.IO.Stream.BeginWrite"
  beginWrite :: System.Array.Array (System.Byte.Byte a0) -> Int -> Int -> System.AsyncCallback.AsyncCallback a3 -> System.Object.Object a4 -> Stream obj -> IO (System.IAsyncResult.IAsyncResult a5)

foreign import dotnet
  "method System.IO.Stream.EndRead"
  endRead :: System.IAsyncResult.IAsyncResult a0 -> Stream obj -> IO (Int)

foreign import dotnet
  "method System.IO.Stream.BeginRead"
  beginRead :: System.Array.Array (System.Byte.Byte a0) -> Int -> Int -> System.AsyncCallback.AsyncCallback a3 -> System.Object.Object a4 -> Stream obj -> IO (System.IAsyncResult.IAsyncResult a5)
-}

foreign import dotnet
  "method System.IO.Stream.Flush"
  flush :: Stream obj -> IO (())

foreign import dotnet
  "method System.IO.Stream.Close"
  close :: Stream obj -> IO (())

foreign import dotnet
  "method System.IO.Stream.set_Position"
  set_Position :: System.Int64.Int64 a0 -> Stream obj -> IO (())

foreign import dotnet
  "method System.IO.Stream.get_Position"
  get_Position :: Stream obj -> IO (System.Int64.Int64 a0)

foreign import dotnet
  "method System.IO.Stream.get_Length"
  get_Length :: Stream obj -> IO (System.Int64.Int64 a0)

foreign import dotnet
  "method System.IO.Stream.get_CanWrite"
  get_CanWrite :: Stream obj -> IO (Bool)

foreign import dotnet
  "method System.IO.Stream.get_CanSeek"
  get_CanSeek :: Stream obj -> IO (Bool)

foreign import dotnet
  "method System.IO.Stream.get_CanRead"
  get_CanRead :: Stream obj -> IO (Bool)


