module System.Xml.WriteState where

import DotNet
import qualified System.Enum

data WriteState_ a
type WriteState a = System.Enum.Enum (WriteState_ a)


