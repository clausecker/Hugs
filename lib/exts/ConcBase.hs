-----------------------------------------------------------------------------
-- This implements Concurrent Haskell's "MVar"s as described in the paper
--
--   "Concurrent Haskell"
--   Simon Peyton Jones, Andrew Gordon and Sigbjorn Finne.
--   In Proceedings of the ACM Symposium on Principles of Programming
--   Languages,St Petersburg Beach, Florida, January 1996. 
--   http://www.dcs.gla.ac.uk/fp/authors/Simon_Peyton_Jones/
--     concurrent-haskell.ps
--
-- except that we have made the following name changes for compatability
-- with GHC 2.05.
--
--   newMVar  -> newEmptyMVar
--
-- There is one significant difference between this implementation and
-- GHC 2.05: 
--
-- o GHC uses preemptive multitasking.
-- 
--   Context switches can occur at any time (except if you call a C
--   function (like "getchar") which blocks the entire process while
--   waiting for input.
-- 
-- o Hugs uses cooperative multitasking.  
-- 
--   Context switches only occur when you use one of the primitives
--   defined in this module.  This means that programs such as:
-- 
--     main = forkIO (write 'a') >> write 'b'
-- 	where
-- 	 write c = putChar c >> write c
-- 
--   will print either "aaaaaaaaaaaaaa..." or "bbbbbbbbbbbb..."
--   instead of some random interleaving of 'a's and 'b's.
-- 
-- Cooperative multitasking is sufficient for writing coroutines and simple
-- graphical user interfaces but the usual assumptions of fairness don't
-- apply and Channel.getChanContents cannot be implemented.
-----------------------------------------------------------------------------
module ConcBase(
	forkIO,
	runOrBlockIO,
	MVar,
	newEmptyMVar, newMVar, takeMVar, putMVar,
	swapMVar, readMVar, isEmptyMVar,
        yield
	) where

import IO(IOMode, Handle, ioeGetErrorString) -- for binary file ops
import IOExts

----------------------------------------------------------------
-- The interface
----------------------------------------------------------------

forkIO      :: IO () -> IO () -- Spawn a thread

newEmptyMVar :: IO (MVar a)
newMVar      :: a -> IO (MVar a)
takeMVar     :: MVar a -> IO a
putMVar      :: MVar a -> a -> IO ()

instance Eq (MVar a) where
  (==) = primEqMVar

-- Spawn a thread and wait for it to return or block
runOrBlockIO :: IO a -> IO (IOResult a)

swapMVar :: MVar a -> a -> IO a

readMVar :: MVar a -> IO a

isEmptyMVar :: MVar a -> IO Bool

----------------------------------------------------------------
-- Easy implementations (definable using the primitive operations)
----------------------------------------------------------------

swapMVar var new = do
  old <- takeMVar var
  putMVar var new
  return old

readMVar mvar =
    takeMVar mvar	>>= \ value ->
    putMVar mvar value	>>
    return value

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

suspend :: IO a
suspend = IO (\f s -> Hugs_SuspendThread)

yield   :: IO ()
yield    = suspend

-- The thread is scheduled immediately and runs with its own success/error
-- continuations.
runOrBlockIO (IO m) = IO (\f s -> s $! (m Hugs_Error Hugs_Return))  

-- suspend current thread passing its continuation to m
blockIO :: ((a -> IOResult a) -> IO a) -> IO a
blockIO m = IO (\ f s -> 
  case m s of { IO ms -> ms f (const Hugs_SuspendThread) }
  )

-- continue the continuation, then go on
continueIO :: IOResult a -> IO ()
continueIO cc = IO (\ f s -> cc `seq` s ())

-- The thread is scheduled immediately and runs with its own success/error
-- continuations.
forkIO m = runOrBlockIO (m `catch` forkErrHandler) >> return ()

forkErrHandler :: IOError -> IO a
forkErrHandler e = do
    putStr "Uncaught error in forked process: \n  "
    putStr (ioeGetErrorString e)
    putStr "\n"           
    suspend

newtype MVar a = MkMVar (IORef (Either a [a -> IOResult a]))

newEmptyMVar = fmap MkMVar (newIORef (Right []))

newMVar x    = fmap MkMVar (newIORef (Left x))

takeMVar (MkMVar v) =
  readIORef v >>= \ state ->
  case state of
  Left a ->
    writeIORef v (Right []) >>
    return a
  Right cs ->
    blockIO (\cc ->
      writeIORef v (Right (cc:cs)) >>
      suspend
    )

putMVar (MkMVar v) a =
  readIORef v >>= \ state ->
  case state of
  Left a ->
    error "putMVar {full MVar}"
  Right [] ->
    writeIORef v (Left a)   >>
    return ()
  Right (c:cs) ->
    writeIORef v (Right cs) >>
    continueIO (c a)       >> -- schedule the blocked process
    return ()                 -- continue with this process

primEqMVar   :: MVar a -> MVar a -> Bool
MkMVar v1 `primEqMVar` MkMVar v2 = v1 == v2

isEmptyMVar (MkMVar v) =
  readIORef v >>= \state -> case state of
                              Left a  -> return False
                              Right a -> return True

-----------------------------------------------------------------------------
