module System.DateTime where

import DotNet
import System.ValueType

data DateTime_ a
type DateTime a = ValueType (DateTime_ a)
