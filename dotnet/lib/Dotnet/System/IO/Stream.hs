module Dotnet.System.IO.Stream where

import Dotnet
import qualified Dotnet.System.MarshalByRefObject
import Dotnet.System.Byte
import Dotnet.System.Array
import Dotnet.System.Int64
import Dotnet.System.IO.SeekOrigin
{-
import Dotnet.System.IAsyncResult
import Dotnet.System.AsyncCallback
-}

data Stream_ a
type Stream a = Dotnet.System.MarshalByRefObject.MarshalByRefObject (Stream_ a)

foreign import dotnet
  "method Dotnet.System.IO.Stream.WriteByte"
  writeByte :: Dotnet.System.Byte.Byte a0 -> Stream obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.Stream.Write"
  write :: Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a0) -> Int -> Int -> Stream obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.Stream.ReadByte"
  readByte :: Stream obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.IO.Stream.Read"
  read :: Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a0) -> Int -> Int -> Stream obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.IO.Stream.SetLength"
  setLength :: Dotnet.System.Int64.Int64 a0 -> Stream obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.Stream.Seek"
  seek :: Dotnet.System.Int64.Int64 a0 -> Dotnet.System.IO.SeekOrigin.SeekOrigin a1 -> Stream obj -> IO (Dotnet.System.Int64.Int64 a2)

{-
foreign import dotnet
  "method Dotnet.System.IO.Stream.EndWrite"
  endWrite :: Dotnet.System.IAsyncResult.IAsyncResult a0 -> Stream obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.Stream.BeginWrite"
  beginWrite :: Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a0) -> Int -> Int -> Dotnet.System.AsyncCallback.AsyncCallback a3 -> Dotnet.System.Object.Object a4 -> Stream obj -> IO (Dotnet.System.IAsyncResult.IAsyncResult a5)

foreign import dotnet
  "method Dotnet.System.IO.Stream.EndRead"
  endRead :: Dotnet.System.IAsyncResult.IAsyncResult a0 -> Stream obj -> IO (Int)

foreign import dotnet
  "method Dotnet.System.IO.Stream.BeginRead"
  beginRead :: Dotnet.System.Array.Array (Dotnet.System.Byte.Byte a0) -> Int -> Int -> Dotnet.System.AsyncCallback.AsyncCallback a3 -> Dotnet.System.Object.Object a4 -> Stream obj -> IO (Dotnet.System.IAsyncResult.IAsyncResult a5)
-}

foreign import dotnet
  "method Dotnet.System.IO.Stream.Flush"
  flush :: Stream obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.Stream.Close"
  close :: Stream obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.Stream.set_Position"
  set_Position :: Dotnet.System.Int64.Int64 a0 -> Stream obj -> IO (())

foreign import dotnet
  "method Dotnet.System.IO.Stream.get_Position"
  get_Position :: Stream obj -> IO (Dotnet.System.Int64.Int64 a0)

foreign import dotnet
  "method Dotnet.System.IO.Stream.get_Length"
  get_Length :: Stream obj -> IO (Dotnet.System.Int64.Int64 a0)

foreign import dotnet
  "method Dotnet.System.IO.Stream.get_CanWrite"
  get_CanWrite :: Stream obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.IO.Stream.get_CanSeek"
  get_CanSeek :: Stream obj -> IO (Bool)

foreign import dotnet
  "method Dotnet.System.IO.Stream.get_CanRead"
  get_CanRead :: Stream obj -> IO (Bool)


