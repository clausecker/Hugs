module System.IO.SeekOrigin where

import DotNet
import qualified System.Enum

data SeekOrigin_ a
type SeekOrigin a = System.Enum.Enum (SeekOrigin_ a)


