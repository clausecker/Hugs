module System.Enum where

import DotNet
import qualified System.ValueType

data Enum_ a
type Enum a = System.ValueType.ValueType (Enum_ a)

