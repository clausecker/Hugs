module System.Type 
	( module System.Type,
	  module System.TypeTy
	) where

import System.TypeTy

foreign import dotnet
  "static System.Type.GetType"
  getType :: String -> IO (Type ())
