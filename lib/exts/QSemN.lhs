%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[Semaphore]{Quantity semaphores}

Quantity semaphores

\begin{code}
module QSemN
	( QSemN,	-- abstract
	  newQSemN,	-- :: Int   -> IO QSemN
	  waitQSemN,	-- :: QSemN -> Int -> IO ()
	  signalQSemN	-- :: QSemN -> Int -> IO ()
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

\begin{code}
newtype QSemN = QSemN (MVar (Int,[(Int,MVar ())]))

newQSemN :: Int -> IO QSemN 
newQSemN init = do
   sem <- newMVar (init,[])
   return (QSemN sem)

waitQSemN :: QSemN -> Int -> IO ()
waitQSemN (QSemN sem) sz = do
  (avail,blocked) <- takeMVar sem   -- gain ex. access
  if (avail - sz) >= 0 then
       -- discharging 'sz' still leaves the semaphore
       -- in an 'unblocked' state.
     putMVar sem (avail-sz,[])
   else do
     block <- newEmptyMVar
     putMVar sem (avail, blocked++[(sz,block)])
     takeMVar block

signalQSemN :: QSemN -> Int  -> IO ()
signalQSemN (QSemN sem) n = do
   (avail,blocked)   <- takeMVar sem
   (avail',blocked') <- free (avail+n) blocked
   putMVar sem (avail',blocked')
 where
   free avail []    = return (avail,[])
   free avail ((req,block):blocked)
     | avail >= req = do
	putMVar block ()
	free (avail-req) blocked
     | otherwise    = do
	(avail',blocked') <- free avail blocked
        return (avail',(req,block):blocked')
\end{code}
