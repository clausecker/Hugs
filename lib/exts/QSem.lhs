%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[Semaphore]{General semaphores}

General semaphores

\begin{code}
module QSem
	( QSem,		-- abstract
	  newQSem,	-- :: Int  -> IO QSem
	  waitQSem,	-- :: QSem -> IO ()
	  signalQSem	-- :: QSem -> IO ()
	) where



import IOExts   ( unsafeInterleaveIO )
import ConcBase ( MVar
		, newMVar
		, newEmptyMVar
		, takeMVar
		, putMVar
		, readMVar
	        )


\end{code}

General semaphores are also implemented readily in terms of shared
@MVar@s, only have to catch the case when the semaphore is tried
waited on when it is empty (==0). Implement this in the same way as
shared variables are implemented - maintaining a list of @MVar@s
representing threads currently waiting. The counter is a shared
variable, ensuring the mutual exclusion on its access.

\begin{code}
newtype QSem = QSem (MVar (Int, [MVar ()]))

newQSem :: Int -> IO QSem
newQSem init = do
   sem <- newMVar (init,[])
   return (QSem sem)

waitQSem :: QSem -> IO ()
waitQSem (QSem sem) = do
   (avail,blocked) <- takeMVar sem  -- gain ex. access
   if avail > 0 then
     putMVar sem (avail-1,[])
    else do
     block <- newEmptyMVar
      {-
	Stuff the reader at the back of the queue,
	so as to preserve waiting order. A signalling
	process then only have to pick the MVar at the
	front of the blocked list.

	The version of waitQSem given in the paper could
	lead to starvation.
      -}
     putMVar sem (0, blocked++[block])
     takeMVar block

signalQSem :: QSem -> IO ()
signalQSem (QSem sem) = do
   (avail,blocked) <- takeMVar sem
   case blocked of
     [] -> putMVar sem (avail+1,[])

     (block:blocked') -> do
	   putMVar sem (0,blocked')
	   putMVar block ()

\end{code}
