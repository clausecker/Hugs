-----------------------------------------------------------------------------
-- IO monad extensions:
--
-- Suitable for use with Hugs 98.
-----------------------------------------------------------------------------

module Hugs.IOExts
	( fixIO				-- :: (a -> IO a) -> IO a
	, unsafePerformIO		-- :: IO a -> a
	, unsafeInterleaveIO		-- :: IO a -> IO a

	, RealWorld

	, performGC

	, IOModeEx(..)	      	-- instance (Eq, Read, Show)
	, openFileEx	      	-- :: FilePath -> IOModeEx -> IO Handle

	, unsafePtrEq
	, unsafePtrToInt
	, unsafeCoerce
	
	  -- backward compatibility with IOExtensions
	, readBinaryFile        -- :: FilePath -> IO String
	, writeBinaryFile       -- :: FilePath -> String -> IO ()
	, appendBinaryFile      -- :: FilePath -> String -> IO ()
	, openBinaryFile        -- :: FilePath -> IOMode -> IO Handle

	   -- non-echoing getchar
	, getCh                 -- :: IO Char
	, argv                  -- :: [String]
	) where

import Hugs.Prelude
import Hugs.IO( IOMode(..), Handle, openFile )
import Hugs.IORef
import Hugs.System ( getArgs )

-----------------------------------------------------------------------------

data RealWorld = RealWorld

primitive performGC "primGC" :: IO ()

unsafePerformIO :: IO a -> a
unsafePerformIO m = valueOf (basicIORun m)

unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO m = return (unsafePerformIO m)

primitive unsafePtrEq    :: a -> a -> Bool
primitive unsafePtrToInt :: a -> Int

fixIO :: (a -> IO a) -> IO a
fixIO f = do
	r <- newIORef (throw NonTermination)
	x <- f (unsafePerformIO (readIORef r))
	writeIORef r x
	return x

primitive unsafeCoerce "primUnsafeCoerce" :: a -> b

valueOf :: IOFinished a -> a
valueOf (Finished_Return a) = a
valueOf _ = error "IOExts.valueOf: thread failed"	-- shouldn't happen

-----------------------------------------------------------------------------
-- Binary files 
-----------------------------------------------------------------------------
data IOModeEx 
 = BinaryMode IOMode
 | TextMode   IOMode
   deriving (Eq, Read, Show)

openFileEx :: FilePath -> IOModeEx -> IO Handle
openFileEx fp m = 
  case m of
    BinaryMode m -> openBinaryFile fp m
    TextMode m   -> openFile fp m

argv :: [String]
argv = unsafePerformIO getArgs

primitive writeBinaryFile   	 :: FilePath -> String -> IO ()
primitive appendBinaryFile  	 :: FilePath -> String -> IO ()
primitive readBinaryFile    	 :: FilePath -> IO String
primitive openBinaryFile         :: FilePath -> IOMode -> IO Handle

primitive getCh                  :: IO Char -- non-echoing getchar
