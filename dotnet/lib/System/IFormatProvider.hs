module System.IFormatProvider where

import DotNet
import System.Object

data IFormatProvider_ a
type IFormatProvider a = System.Object.Object (IFormatProvider_ a)

