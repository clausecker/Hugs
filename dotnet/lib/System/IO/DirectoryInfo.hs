module System.IO.DirectoryInfo 
	( module System.IO.DirectoryInfo ) where

import qualified System.Object

data DirectoryInfo_ a
type DirectoryInfo a = System.Object.Object (DirectoryInfo_ a)

