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

	, trace

	, performGC

	, IOModeEx(..)	      	-- instance (Eq, Read, Show)
	, openFileEx	      	-- :: FilePath -> IOModeEx -> IO Handle

	, unsafePtrEq
	, unsafePtrToInt
	, unsafeCoerce
	) where

import Hugs.Prelude
import Hugs.IO( ioeGetErrorString, IOMode(..), Handle, openFile )

-----------------------------------------------------------------------------

data RealWorld = RealWorld

primitive performGC "primGC" :: IO ()

primitive trace :: String -> a -> a

unsafePerformIO :: IO a -> a
unsafePerformIO m = performIO (runAndShowError m)

unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO m = interleaveIO (runAndShowError m)

primitive unsafePtrEq    :: a -> a -> Bool
primitive unsafePtrToInt :: a -> Int

fixIO :: (a -> IO a) -> IO a
fixIO m = IO fixIO'
 where
  fixIO' fail succ =
    case r of
    Finished_Return a   -> succ a
    Finished_Error err  -> fail err
    other               -> error "IOExts.fixIO: failed"
   where
    r = basicIORun (m a)
    a = case r   of 
        Finished_Return a  -> a
        _                  -> error "IOExts.fixIO: thread exited with error"

primitive unsafeCoerce "primUnsafeCoerce" :: a -> b

performIO :: IO a -> a
performIO m = 
  case basicIORun m of
    Finished_Return a  -> a
    _                  -> error "IOExts.performIO: thread exited with error"

interleaveIO :: IO a -> IO a
interleaveIO m = IO (\ f s -> 
  s (case basicIORun m of
       Finished_Return a  -> a
       _                  -> error "IOExts.interleaveIO: thread exited with error"
     ))

runAndShowError :: IO a -> IO a
runAndShowError m =
  m `catch` \err -> do 
      putChar '\n'
      putStr (show err)
      return undefined

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

primitive openBinaryFile         :: FilePath -> IOMode -> IO Handle
