module System.TypeCode where

import DotNet
import qualified System.Enum

data TypeCode_ a
type TypeCode a = System.Enum.Enum (TypeCode_ a)


