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
module Hugs.ConcBase(
	forkIO,
	MVar,
	newEmptyMVar, newMVar, takeMVar, tryTakeMVar, putMVar, tryPutMVar,
	swapMVar, readMVar, isEmptyMVar,
        yield
	) where

import Hugs.Prelude(
	IO(..), IOResult(..), threadToIOResult,
	HugsException, catchHugsException, blockIO)
import Hugs.IO(IOMode, Handle, ioeGetErrorString) -- for binary file ops
import Hugs.IORef

----------------------------------------------------------------
-- The interface
----------------------------------------------------------------

forkIO       :: IO () -> IO () -- Spawn a thread
yield        :: IO ()

newEmptyMVar :: IO (MVar a)
newMVar      :: a -> IO (MVar a)
takeMVar     :: MVar a -> IO a
putMVar      :: MVar a -> a -> IO ()
tryPutMVar   :: MVar a -> a -> IO Bool
tryTakeMVar  :: MVar a -> IO (Maybe a)

instance Eq (MVar a) where
  (==) = primEqMVar

swapMVar    :: MVar a -> a -> IO a
readMVar    :: MVar a -> IO a
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

kill :: IO a
kill = IO (\f s -> Hugs_DeadThread)

yield = IO (\ f s -> Hugs_YieldThread (s ()))

-- add the continuation to the runnable list, and continue
continueIO :: IOResult -> IO ()
continueIO cc = IO (\ f s -> Hugs_ForkThread (s ()) cc)

-- The thread is scheduled immediately and runs with its own success/error
-- continuations.
forkIO m = continueIO (threadToIOResult ((m `catchHugsException` forkExnHandler) `catch` forkErrHandler))

forkErrHandler :: IOError -> IO a
forkErrHandler e = do
    putStr "\nThread raised error:\n"
    putStr (show e)
    putStr "\n"           
    kill

forkExnHandler :: HugsException -> IO a
forkExnHandler e = do
    putStr "\nThread raised exception: "
    putStr (show e)
    putStr "\n"           
    kill

-- An MVar can be either:
-- o full : Left (a,ts)
--   Has a value 'a' and a list 'ts' of value-thread pairs blocked waiting
--   to write to the MVar.
--   The ()-> part of the thread is because blocked threads have to be
--   functions. :-(
-- o empty : Right ts
--   Has a list of threads 'ts' waiting to receive a value
newtype MVar a = MkMVar (IORef (Either (a,[(a,()->IOResult)]) [a -> IOResult]))

newEmptyMVar = fmap MkMVar (newIORef (Right []))

newMVar x    = fmap MkMVar (newIORef (Left (x,[])))

takeMVar (MkMVar v) =
  readIORef v >>= \ state ->
  case state of
  Left (a,[]) ->
    writeIORef v (Right []) >>
    return a
  Left (a,(a',t):ts) ->
    writeIORef v (Left (a', ts)) >>
    continueIO (t ())            >>  -- reschedule t
    return a
  Right cs ->
    blockIO (\cc -> writeIORef v (Right (cs ++ [cc])))

-- tryTakeMVar is a non-blocking takeMVar
tryTakeMVar (MkMVar v) =
  readIORef v >>= \ state ->
  case state of
  Left (a,[]) ->
    writeIORef v (Right []) >>
    return (Just a)
  Left (a,(a',t):ts) ->
    writeIORef v (Left (a', ts)) >>
    continueIO (t ())            >> -- reschedule t
    return (Just a)
  Right cs ->
    return Nothing

putMVar (MkMVar v) a =
  readIORef v >>= \ state ->
  case state of
  Left (a',ts) ->
    blockIO (\cc -> writeIORef v (Left (a',(ts++[(a,cc)]))))
  Right [] ->
    writeIORef v (Left (a,[]))
  Right (c:cs) ->
    writeIORef v (Right cs) >>
    continueIO (c a)   -- reschedule the blocked thread

tryPutMVar (MkMVar v) a =
  readIORef v >>= \ state ->
  case state of
  Left _ ->
    return False
  Right [] ->
    writeIORef v (Left (a,[]))   >>
    return True
  Right (c:cs) ->
    writeIORef v (Right cs) >>
    continueIO (c a)  >> -- reschedule the blocked thread
    return True

primEqMVar   :: MVar a -> MVar a -> Bool
MkMVar v1 `primEqMVar` MkMVar v2 = v1 == v2

{- 
 Low-level op. for checking whether an MVar is filled-in or not.
 Notice that the boolean value returned  is just a snapshot of
 the state of the MVar. By the time you get to react on its result,
 the MVar may have been filled (or emptied) - so be extremely
 careful when using this operation.  

 Use tryTakeMVar instead if possible.

 If you can re-work your abstractions to avoid having to
 depend on isEmptyMVar, then you're encouraged to do so,
 i.e., consider yourself warned about the imprecision in
 general of isEmptyMVar :-)
-}
isEmptyMVar (MkMVar v) =
  readIORef v >>= \state -> 
  case state of
  Left  _ -> return False
  Right _ -> return True

-----------------------------------------------------------------------------

