%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[Channel]{Unbounded Channels}

Standard, unbounded channel abstraction.

\begin{code}
module Channel
       (
	 {- abstract type defined -}
        Chan,

	 {- creator -}
	newChan,	 -- :: IO (Chan a)

	 {- operators -}
	writeChan,	 -- :: Chan a -> a -> IO ()
	readChan,	 -- :: Chan a -> IO a
	dupChan,	 -- :: Chan a -> IO (Chan a)
	unReadChan,	 -- :: Chan a -> a -> IO ()
        isEmptyChan,     -- :: Chan a -> IO Bool	-- PRH

	 {- stream interface -}
	getChanContents, -- :: Chan a -> IO [a]
	writeList2Chan	 -- :: Chan a -> [a] -> IO ()

       ) where

import Prelude
import IOExts( unsafeInterleaveIO )
import ConcBase
\end{code}

A channel is represented by two @MVar@s keeping track of the two ends
of the channel contents,i.e.,  the read- and write ends. Empty @MVar@s
are used to handle consumers trying to read from an empty channel.

\begin{code}

data Chan a
 = Chan (MVar (Stream a))
        (MVar (Stream a))

type Stream a = MVar (ChItem a)

data ChItem a = ChItem a (Stream a)


\end{code}

See the Concurrent Haskell paper for a diagram explaining the
how the different channel operations proceed.

@newChan@ sets up the read and write end of a channel by initialising
these two @MVar@s with an empty @MVar@.

\begin{code}

newChan :: IO (Chan a)
newChan
 = newEmptyMVar	     >>= \ hole ->
   newMVar hole      >>= \ read ->
   newMVar hole      >>= \ write ->
   return (Chan read write)

\end{code}

To write an element on a channel, a new hole at the write end is created.
What was previously the empty @MVar@ at the back of the channel is then
filled in with a new stream element holding the entered value and the
new hole.

\begin{code}

writeChan :: Chan a -> a -> IO ()
writeChan (Chan read write) val
 = newEmptyMVar		    >>= \ new_hole ->
   takeMVar write	    >>= \ old_hole ->
   putMVar write new_hole   >> 
   putMVar old_hole (ChItem val new_hole) >>
   return ()


readChan :: Chan a -> IO a
readChan (Chan read write)
 = takeMVar read	  >>= \ rend ->
   readMVar rend          >>= \ (ChItem val new_rend) ->
   putMVar read new_rend  >>
   return val

isEmptyChan :: Chan a -> IO Bool
isEmptyChan (Chan read write)
 = takeMVar read      >>= \r ->
   readMVar write     >>= \w ->
   putMVar read r     >>
   return (r == w)

{-
-- PRH:
isEmptyChan :: Chan a -> IO Bool
isEmptyChan (Chan read write)
 = readMVar read      >>= \ rend ->
   isEmptyMVar rend   >>= \ yes  ->
   return yes
-}

-- or just:
-- isEmptyChan (Chan read write)
--  = readMVar read      >>=
--    isEmptyMVar

dupChan :: Chan a -> IO (Chan a)
dupChan (Chan read write)
 = newEmptyMVar		  >>= \ new_read ->
   takeMVar write	  >>= \ hole ->
   putMVar new_read hole  >>
   return (Chan new_read write)

unReadChan :: Chan a -> a -> IO ()
unReadChan (Chan read write) val
 = newEmptyMVar			      >>= \ new_rend ->
   takeMVar read		      >>= \ rend ->
   putMVar new_rend (ChItem val rend) >> 
   putMVar read new_rend              >>
   return ()

\end{code}

Operators for interfacing with functional streams.

\begin{code}

getChanContents :: Chan a -> IO [a]
-- Rewritten by ADR to use IO monad instead of PrimIO Monad
getChanContents ch = unsafeInterleaveIO $ do
	     x  <- readChan ch
	     xs <- getChanContents ch
	     return (x:xs)

--ADR: my_2_IO :: PrimIO (Either IOError a) -> IO a -- simple; primIOToIO does too much!
--ADR: my_2_IO m = IO m
--ADR: 
--ADR: readChan_prim	     :: Chan a -> PrimIO (Either IOError  a)
--ADR: readChanContents_prim :: Chan a -> PrimIO (Either IOError [a])
--ADR: 
--ADR: readChan_prim ch = ST $ \ s ->
--ADR: 	   case (readChan ch) of { IO (ST read) ->
--ADR: 	   read s }
--ADR: 
--ADR: readChanContents_prim ch = ST $ \ s ->
--ADR: 	   case (readChanContents ch) of { IO (ST read) ->
--ADR: 	   read s }

-------------
writeList2Chan :: Chan a -> [a] -> IO ()
writeList2Chan ch ls = sequence_ (map (writeChan ch) ls)

\end{code}
