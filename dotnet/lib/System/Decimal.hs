module System.Decimal where

import DotNet
import qualified System.ValueType

data Decimal_ a
type Decimal a = System.ValueType.ValueType (Decimal_ a)
