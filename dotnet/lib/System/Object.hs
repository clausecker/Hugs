module System.Object 
	(module System.Object, 
         module System.ObjectTy
	 ) where

import qualified DotNet ( Object )
import DotNet hiding ( Object )
import System.TypeTy
import System.ObjectTy
import System.StringTy

foreign import dotnet
  "method System.Object.Equals"
  equals :: Object a -> Object b -> IO Bool

foreign import dotnet
  "method System.Object.GetHashCode"
  getHashCode :: Object a -> IO Int

foreign import dotnet
  "method System.Object.GetType"
  getType :: Object a -> IO (Type ())

foreign import dotnet
  "method System.Object.MemberwiseClone"
  memberwiseClone :: Object a -> IO (Type a)

foreign import dotnet
  "static method System.Object.ReferenceEquals"
  referenceEquals :: Object a -> Object b -> IO Bool

foreign import dotnet
  "method System.Object.ToString"
  toString :: Object a -> IO String
