module System.MarshalByRefObject where

import DotNet
import qualified System.Object

data MarshalByRefObject_ a
type MarshalByRefObject a = System.Object.Object (MarshalByRefObject_ a)
