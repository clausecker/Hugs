module System.UInt64 where

import DotNet
import qualified System.ValueType

data UInt64_ a
type UInt64 a = System.ValueType.ValueType (UInt64_ a)
