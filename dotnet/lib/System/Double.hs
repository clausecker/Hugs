module System.Double where

import DotNet
import qualified System.ValueType

data Double_ a
type Double a = System.ValueType.ValueType (Double_ a)
