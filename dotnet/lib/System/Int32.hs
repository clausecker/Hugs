module System.Int32 where

import DotNet
import qualified System.ValueType

data Int32_ a
type Int32 a = System.ValueType.ValueType (Int32_ a)
