module System.Text.Decoder where

import DotNet
import qualified System.Object
import System.Array
import System.Byte
import System.Char

data Decoder_ a
type Decoder a = System.Object.Object (Decoder_ a)

foreign import dotnet
  "method System.Text.Decoder.GetChars"
  getChars :: System.Array.Array (System.Byte.Byte a0) -> Int -> Int -> System.Array.Array (System.Char.Char a3) -> Int -> Decoder obj -> IO (Int)

foreign import dotnet
  "method System.Text.Decoder.GetCharCount"
  getCharCount :: System.Array.Array (System.Byte.Byte a0) -> Int -> Int -> Decoder obj -> IO (Int)


