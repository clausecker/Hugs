-----------------------------------------------------------------------------
-- Non-standard extensions to IO monad.
--
-- Binary file extensions:
--
--   readBinaryFile         : versions of readFile, writeFile, appendFile,
--   writeBinaryFile        : and openFile for use on binary files
--   appendBinaryFile       : (These don't do LF <-> CR-LF translation on
--   openBinaryFile         :  DOS/Windows systems.)
--
-- Miscellaneous extensions:
--
--   getCh                  : like getChar but doesn't echo to screen
--   argv                   : value returned by getArgv     
-- 
-- None of these operations can be implemented in standard Haskell using the
-- standard Haskell prelude.
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------

module IOExtensions(
	readBinaryFile, writeBinaryFile, appendBinaryFile,
	openBinaryFile, 
        getCh,
	argv
	) where

import System( getArgs )
import IO( Handle, IOMode )
import IOExts( unsafePerformIO )

argv :: [String]
argv = unsafePerformIO getArgs

primitive writeBinaryFile   	 :: FilePath -> String -> IO ()
primitive appendBinaryFile  	 :: FilePath -> String -> IO ()
primitive readBinaryFile    	 :: FilePath -> IO String
primitive openBinaryFile         :: FilePath -> IOMode -> IO Handle

primitive getCh                  :: IO Char -- non-echoing getchar



