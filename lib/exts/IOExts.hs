-----------------------------------------------------------------------------
-- IO monad extensions:
--
-- Suitable for use with Hugs 98.
-----------------------------------------------------------------------------

module IOExts
	( fixIO
	, unsafePerformIO
	, unsafeInterleaveIO

	, IORef
	  -- instance Eq (IORef a)
	, newIORef
	, readIORef
	, writeIORef

        , IOArray
          -- instance Eq (IOArray ix elt)
        , newIOArray
        , boundsIOArray
        , readIOArray
        , writeIOArray
        , thawIOArray
        , freezeIOArray
        , unsafeFreezeIOArray

	, performGC
	, trace
	, unsafePtrEq
	, unsafePtrToInt
	) where

import Trace( trace )
import IO( ioeGetErrorString )
import Array

-----------------------------------------------------------------------------

primitive performGC "primGC" :: IO ()

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
    other               -> error "IOExts:fixIO: failed"
   where
    r = basicIORun (m a)
    a = case r   of 
        Finished_Return a  -> a
        _                  -> error "IOExts:fixIO: thread exited with error"

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
      putStr (ioeGetErrorString err)
      return undefined

-----------------------------------------------------------------------------

data IORef a        -- mutable variables containing values of type a

primitive newIORef   "newRef" :: a -> IO (IORef a)
primitive readIORef  "getRef" :: IORef a -> IO a
primitive writeIORef "setRef" :: IORef a -> a -> IO ()
primitive eqIORef    "eqRef"  :: IORef a -> IORef a -> Bool

instance Eq (IORef a) where
    (==) = eqIORef

-----------------------------------------------------------------------------

data IOArray ix elt -- implemented as an internal primitive

newIOArray          :: Ix ix => (ix,ix) -> elt -> IO (IOArray ix elt)
boundsIOArray       :: Ix ix => IOArray ix elt -> (ix, ix)
readIOArray         :: Ix ix => IOArray ix elt -> ix -> IO elt
writeIOArray        :: Ix ix => IOArray ix elt -> ix -> elt -> IO ()
thawIOArray         :: Ix ix => Array ix elt -> IO (IOArray ix elt)
freezeIOArray       :: Ix ix => IOArray ix elt -> IO (Array ix elt)
unsafeFreezeIOArray :: Ix ix => IOArray ix elt -> IO (Array ix elt)

newIOArray bs e      = primNewArr bs (rangeSize bs) e
boundsIOArray a      = primBounds a
readIOArray a i      = primReadArr index a i
writeIOArray a i e   = primWriteArr index a i e
thawIOArray arr      = do a <- newIOArray (bounds arr) err
			  let fillin []          = return a
			      fillin((ix,v):ixs) = do writeIOArray a ix v
                                                      fillin ixs
                          fillin (assocs arr)
                       where err =  error "thawArray: element not overwritten"

freezeIOArray a      = primFreeze a
unsafeFreezeIOArray  = freezeIOArray  -- not as fast as GHC

instance Eq (IOArray ix elt) where
  (==) = eqIOArray

primitive primNewArr   "IONewArr"
          :: (a,a) -> Int -> b -> IO (IOArray a b)
primitive primReadArr  "IOReadArr"
          :: ((a,a) -> a -> Int) -> IOArray a b -> a -> IO b
primitive primWriteArr "IOWriteArr"
          :: ((a,a) -> a -> Int) -> IOArray a b -> a -> b -> IO ()
primitive primFreeze   "IOFreeze"
          :: IOArray a b -> IO (Array a b)
primitive primBounds   "IOBounds"
          :: IOArray a b -> (a,a)
primitive eqIOArray    "IOArrEq"
          :: IOArray a b -> IOArray a b -> Bool

-----------------------------------------------------------------------------
