module System.Char where

import DotNet
import qualified System.Object

data Char_ a
type Char a = System.Object.Object (Char_ a)

