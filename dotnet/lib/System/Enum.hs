module System.Enum where

import DotNet
import qualified System.ValueType
import qualified System.TypeTy

data Enum_ a
type Enum a = System.ValueType.ValueType (Enum_ a)

foreign import dotnet
  "static System.Enum.Parse"
  parse :: System.TypeTy.Type a -> String -> IO (Enum b)


