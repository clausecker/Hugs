module System.UInt32 where

import DotNet
import qualified System.ValueType

data UInt32_ a
type UInt32 a = System.ValueType.ValueType (UInt32_ a)
