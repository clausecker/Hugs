module System.Text.Encoding where

import DotNet
import qualified System.Object
import qualified System.Array
import System.Byte
import System.Text.Encoder
import System.Text.Decoder
import System.Char

data Encoding_ a
type Encoding a = System.Object.Object (Encoding_ a)

foreign import dotnet
  "method System.Text.Encoding.GetString"
  getString :: System.Array.Array (System.Byte.Byte a0) -> Int -> Int -> Encoding obj -> IO (String)

foreign import dotnet
  "method System.Text.Encoding.GetString"
  getString_1 :: System.Array.Array (System.Byte.Byte a0) -> Encoding obj -> IO (String)

foreign import dotnet
  "method System.Text.Encoding.GetMaxCharCount"
  getMaxCharCount :: Int -> Encoding obj -> IO (Int)

foreign import dotnet
  "method System.Text.Encoding.GetMaxByteCount"
  getMaxByteCount :: Int -> Encoding obj -> IO (Int)

foreign import dotnet
  "method System.Text.Encoding.GetEncoder"
  getEncoder :: Encoding obj -> IO (System.Text.Encoder.Encoder a0)

foreign import dotnet
  "method System.Text.Encoding.GetDecoder"
  getDecoder :: Encoding obj -> IO (System.Text.Decoder.Decoder a0)

foreign import dotnet
  "method System.Text.Encoding.get_CodePage"
  get_CodePage :: Encoding obj -> IO (Int)

foreign import dotnet
  "method System.Text.Encoding.GetChars"
  getChars :: System.Array.Array (System.Byte.Byte a0) -> Int -> Int -> System.Array.Array (System.Char.Char a3) -> Int -> Encoding obj -> IO (Int)

foreign import dotnet
  "method System.Text.Encoding.GetChars"
  getChars_1 :: System.Array.Array (System.Byte.Byte a0) -> Int -> Int -> Encoding obj -> IO (System.Array.Array (System.Char.Char a3))

foreign import dotnet
  "method System.Text.Encoding.GetChars"
  getChars_2 :: System.Array.Array (System.Byte.Byte a0) -> Encoding obj -> IO (System.Array.Array (System.Char.Char a1))

foreign import dotnet
  "method System.Text.Encoding.GetCharCount"
  getCharCount :: System.Array.Array (System.Byte.Byte a0) -> Int -> Int -> Encoding obj -> IO (Int)

foreign import dotnet
  "method System.Text.Encoding.GetCharCount"
  getCharCount_1 :: System.Array.Array (System.Byte.Byte a0) -> Encoding obj -> IO (Int)

foreign import dotnet
  "method System.Text.Encoding.GetBytes"
  getBytes :: String -> Int -> Int -> System.Array.Array (System.Byte.Byte a3) -> Int -> Encoding obj -> IO (Int)

foreign import dotnet
  "method System.Text.Encoding.GetBytes"
  getBytes_1 :: String -> Encoding obj -> IO (System.Array.Array (System.Byte.Byte a1))

foreign import dotnet
  "method System.Text.Encoding.GetBytes"
  getBytes_2 :: System.Array.Array (System.Char.Char a0) -> Int -> Int -> System.Array.Array (System.Byte.Byte a3) -> Int -> Encoding obj -> IO (Int)

foreign import dotnet
  "method System.Text.Encoding.GetBytes"
  getBytes_3 :: System.Array.Array (System.Char.Char a0) -> Int -> Int -> Encoding obj -> IO (System.Array.Array (System.Byte.Byte a3))

foreign import dotnet
  "method System.Text.Encoding.GetBytes"
  getBytes_4 :: System.Array.Array (System.Char.Char a0) -> Encoding obj -> IO (System.Array.Array (System.Byte.Byte a1))

foreign import dotnet
  "method System.Text.Encoding.GetByteCount"
  getByteCount :: System.Array.Array (System.Char.Char a0) -> Int -> Int -> Encoding obj -> IO (Int)

foreign import dotnet
  "method System.Text.Encoding.GetByteCount"
  getByteCount_1 :: String -> Encoding obj -> IO (Int)

foreign import dotnet
  "method System.Text.Encoding.GetByteCount"
  getByteCount_2 :: System.Array.Array (System.Char.Char a0) -> Encoding obj -> IO (Int)

foreign import dotnet
  "method System.Text.Encoding.get_IsMailNewsSave"
  get_IsMailNewsSave :: Encoding obj -> IO (Bool)

foreign import dotnet
  "method System.Text.Encoding.get_IsMailNewsDisplay"
  get_IsMailNewsDisplay :: Encoding obj -> IO (Bool)

foreign import dotnet
  "method System.Text.Encoding.get_IsBrowserSave"
  get_IsBrowserSave :: Encoding obj -> IO (Bool)

foreign import dotnet
  "method System.Text.Encoding.get_IsBrowserDisplay"
  get_IsBrowserDisplay :: Encoding obj -> IO (Bool)

foreign import dotnet
  "method System.Text.Encoding.get_WindowsCodePage"
  get_WindowsCodePage :: Encoding obj -> IO (Int)

foreign import dotnet
  "method System.Text.Encoding.get_WebName"
  get_WebName :: Encoding obj -> IO (String)

foreign import dotnet
  "method System.Text.Encoding.get_HeaderName"
  get_HeaderName :: Encoding obj -> IO (String)

foreign import dotnet
  "method System.Text.Encoding.get_EncodingName"
  get_EncodingName :: Encoding obj -> IO (String)

foreign import dotnet
  "method System.Text.Encoding.get_BodyName"
  get_BodyName :: Encoding obj -> IO (String)

foreign import dotnet
  "method System.Text.Encoding.GetPreamble"
  getPreamble :: Encoding obj -> IO (System.Array.Array (System.Byte.Byte a0))

foreign import dotnet
  "method System.Text.Encoding.GetHashCode"
  getHashCode :: Encoding obj -> IO (Int)

foreign import dotnet
  "method System.Text.Encoding.Equals"
  equals :: System.Object.Object a0 -> Encoding obj -> IO (Bool)

foreign import dotnet
  "static method System.Text.Encoding.Convert"
  convert :: System.Text.Encoding.Encoding a0 -> System.Text.Encoding.Encoding a1 -> System.Array.Array (System.Byte.Byte a2) -> IO (System.Array.Array (System.Byte.Byte a3))

foreign import dotnet
  "static method System.Text.Encoding.Convert"
  convert_1 :: System.Text.Encoding.Encoding a0 -> System.Text.Encoding.Encoding a1 -> System.Array.Array (System.Byte.Byte a2) -> Int -> Int -> IO (System.Array.Array (System.Byte.Byte a5))

foreign import dotnet
  "static method System.Text.Encoding.GetEncoding"
  getEncoding :: Int -> IO (System.Text.Encoding.Encoding a1)

foreign import dotnet
  "static method System.Text.Encoding.GetEncoding"
  getEncoding_1 :: String -> IO (System.Text.Encoding.Encoding a1)

foreign import dotnet
  "static method System.Text.Encoding.get_ASCII"
  get_ASCII :: IO (System.Text.Encoding.Encoding a0)

foreign import dotnet
  "static method System.Text.Encoding.get_Default"
  get_Default :: IO (System.Text.Encoding.Encoding a0)

foreign import dotnet
  "static method System.Text.Encoding.get_Unicode"
  get_Unicode :: IO (System.Text.Encoding.Encoding a0)

foreign import dotnet
  "static method System.Text.Encoding.get_BigEndianUnicode"
  get_BigEndianUnicode :: IO (System.Text.Encoding.Encoding a0)

foreign import dotnet
  "static method System.Text.Encoding.get_UTF7"
  get_UTF7 :: IO (System.Text.Encoding.Encoding a0)

foreign import dotnet
  "static method System.Text.Encoding.get_UTF8"
  get_UTF8 :: IO (System.Text.Encoding.Encoding a0)


