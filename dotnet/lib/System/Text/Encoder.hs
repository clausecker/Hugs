module System.Text.Encoder where

import DotNet
import qualified System.Object
import System.Array
import System.Char
import System.Byte

data Encoder_ a
type Encoder a = System.Object.Object (Encoder_ a)

foreign import dotnet
  "method System.Text.Encoder.GetBytes"
  getBytes :: System.Array.Array (System.Char.Char a0) -> Int -> Int -> System.Array.Array (System.Byte.Byte a3) -> Int -> Bool -> Encoder obj -> IO (Int)

foreign import dotnet
  "method System.Text.Encoder.GetByteCount"
  getByteCount :: System.Array.Array (System.Char.Char a0) -> Int -> Int -> Bool -> Encoder obj -> IO (Int)


