module System.UriPartial where

import DotNet
import qualified System.Enum

data UriPartial_ a
type UriPartial a = System.Enum.Enum (UriPartial_ a)


