module System.Int64 where

import DotNet
import qualified System.ValueType

data Int64_ a
type Int64 a = System.ValueType.ValueType (Int64_ a)
