module System.Single where

import DotNet
import qualified System.ValueType

data Single_ a
type Single a = System.ValueType.ValueType (Single_ a)
