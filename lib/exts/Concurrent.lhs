%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[Concurrent]{Concurrent Haskell constructs}

A common interface to a collection of useful concurrency abstractions.
Currently, the collection only contains the abstractions found in the
{\em Concurrent Haskell} paper (presented at the Haskell Workshop
1995, draft available via \tr{ftp} from
\tr{ftp.dcs.gla.ac.uk/pub/glasgow-fp/drafts}.)  plus a couple of
others. See the paper and the individual files containing the module
definitions for explanation on what they do.

\begin{code}
module Concurrent
	( module Chan
	, module CVar
	, module MVar
	, module QSem
	, module QSemN
	, module SampleVar


	, forkIO	-- :: IO () -> IO ()

	, seq  		-- :: a -> b -> b

	, yield         -- :: IO ()



	 -- merging of streams
	, mergeIO	-- :: [a]   -> [a] -> IO [a]
	, nmergeIO	-- :: [[a]] -> IO [a]
    ) where


import Prelude hiding( catch )

import Exception


import IOExts ( unsafeInterleaveIO, unsafePerformIO )
import ConcBase


import MVar
import CVar
import Chan
import QSem
import QSemN
import SampleVar
import Parallel


\end{code}

Thread Ids, specifically the instances of Eq and Ord for these things.
The ThreadId type itself is defined in std/PrelConc.lhs.

Rather than define a new primitve, we use a little helper function
cmp_thread in the RTS.

\begin{code}

\end{code}

\begin{code}

-- forkIO in prelude



\end{code}


\begin{code}
max_buff_size :: Int
max_buff_size = 1

mergeIO :: [a] -> [a] -> IO [a]
nmergeIO :: [[a]] -> IO [a]

mergeIO ls rs
 = newEmptyMVar		       >>= \ tail_node ->
   newMVar tail_node	       >>= \ tail_list ->
   newQSem max_buff_size       >>= \ e ->
   newMVar 2                   >>= \ branches_running ->
   let
    buff = (tail_list,e)
   in
    forkIO (suckIO branches_running buff ls) >>
    forkIO (suckIO branches_running buff rs) >>
    takeMVar tail_node	>>= \ val ->
    signalQSem e 	>>
    return val

type Buffer a 
 = (MVar (MVar [a]), QSem)

suckIO :: MVar Int -> Buffer a -> [a] -> IO ()

suckIO branches_running buff@(tail_list,e) vs
 = case vs of
	[] -> takeMVar branches_running >>= \ val ->
	      if val == 1 then
		 takeMVar tail_list     >>= \ node ->
		 putMVar node []        >>
		 putMVar tail_list node
	      else 	
  		 putMVar branches_running (val-1)
	(x:xs) ->
		waitQSem e 	   		 >>
		takeMVar tail_list 		 >>= \ node ->
	        newEmptyMVar 	   		 >>= \ next_node ->
		unsafeInterleaveIO (
			takeMVar next_node  >>= \ y ->
			signalQSem e	    >>
			return y)	         >>= \ next_node_val ->
		putMVar node (x:next_node_val)   >>
		putMVar tail_list next_node 	 >>
		suckIO branches_running buff xs

nmergeIO lss
 = let
    len = length lss
   in
    newEmptyMVar	  >>= \ tail_node ->
    newMVar tail_node	  >>= \ tail_list ->
    newQSem max_buff_size >>= \ e ->
    newMVar len		  >>= \ branches_running ->
    let
     buff = (tail_list,e)
    in
    mapIO (\ x -> forkIO (suckIO branches_running buff x)) lss >>
    takeMVar tail_node	>>= \ val ->
    signalQSem e 	>>
    return val
  where
    mapIO f xs = sequence (map f xs)
\end{code}
