%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[MVars]{MVars: Synchronising variables}

\begin{code}
module MVar
	( MVar		-- abstract
	, newEmptyMVar  -- :: IO (MVar a)
	, newMVar 	-- :: a -> IO (MVar a)
	, takeMVar 	-- :: MVar a -> IO a
	, putMVar  	-- :: MVar a -> a -> IO ()
	, readMVar 	-- :: MVar a -> IO a
	, swapMVar 	-- :: MVar a -> a -> IO a
	, tryTakeMVar   -- :: MVar a -> IO (Maybe a)
	, tryPutMVar    -- :: MVar a -> a -> IO Bool
	, isEmptyMVar	-- :: MVar a -> IO Bool
	, withMVar	-- :: MVar a -> (a -> IO b) -> IO b
	, modifyMVar_ 	-- :: MVar a -> (a -> IO a) -> IO ()
	, modifyMVar 	-- :: MVar a -> (a -> IO (a,b)) -> IO b

    ) where


import ConcBase	( MVar, newEmptyMVar, newMVar, takeMVar, putMVar,
		  tryTakeMVar, tryPutMVar, isEmptyMVar,
                  readMVar, swapMVar,
		)
import Prelude hiding( catch )

import Exception


-- This is as close as Hugs gets to providing throw
throw :: Exception -> IO a
throw = throwIO




-- put back the same value, return something
withMVar :: MVar a -> (a -> IO b) -> IO b
withMVar m io = 
  block $ do
    a <- takeMVar m
    b <- Exception.catch (unblock (io a))
      	    (\e -> do putMVar m a; throw e)
    putMVar m a
    return b

-- put back a new value, return ()
modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_ m io = 
  block $ do
    a  <- takeMVar m
    a' <- Exception.catch (unblock (io a))
      	    (\e -> do putMVar m a; throw e)
    putMVar m a'

-- put back a new value, return something
modifyMVar :: MVar a -> (a -> IO (a,b)) -> IO b
modifyMVar m io = 
  block $ do
    a      <- takeMVar m
    (a',b) <- Exception.catch (unblock (io a))
      	        (\e -> do putMVar m a; throw e)
    putMVar m a'
    return b
\end{code}
