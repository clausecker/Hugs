module System.Byte where

import DotNet
import qualified System.Object

data Byte_ a
type Byte a = System.Object.Object (Byte_ a)

