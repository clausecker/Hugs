module System.ValueType where

import DotNet
import qualified System.Object

data ValueType_ a
type ValueType a = System.Object.Object (ValueType_ a)

