module System.String 
	( module System.String,
	  module System.StringTy
 	) where

import DotNet hiding ( Object, new )
import System.Object
import System.StringTy

new :: String -> IO (StringTy ())
new str = newString str

foreign import dotnet
  "static field System.String.Empty"
  empty :: IO (StringTy a)

foreign import dotnet
  "method System.String.get_Chars"
  charAt :: Int -> StringTy a -> IO Char

foreign import dotnet
  "method System.String.get_Length"
  lengthString :: StringTy a -> IO Int

foreign import dotnet
  "method System.String.Clone"
  clone :: StringTy a -> IO (StringTy a)

foreign import dotnet
  "method System.String.EndsWith"
  endsWith :: String -> StringTy a -> IO Bool

foreign import dotnet
  "method System.String.StartsWith"
  startsWith :: String -> StringTy a -> IO Bool

