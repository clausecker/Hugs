module System.UriHostNameType where

import DotNet
import qualified System.Enum

data UriHostNameType_ a
type UriHostNameType a = System.Enum.Enum (UriHostNameType_ a)


