-- !!! Testing the MVar primitives

-- Some of these tests are marked as non-deterministic.
-- Tests involving these will "fail" if the scheduling order
-- is changed.

module TestMVar(test1,test2,test3,test4,test5,test6,test7,test8) where

import ConcBase

-- should print "a"
test1 =
  newEmptyMVar    >>= \ v ->
  putMVar v 'a'   >>
  takeMVar v      >>= \ x ->
  putChar x       >>
  takeMVar v      >>= \ x ->
  putChar x

-- Nondeterministic - may deadlock
test2 = 
  newEmptyMVar    >>= \ v ->
  forkIO (p1 v)   >>
  get v           >>
  put v 'b'
 where
  p1 v = 
   put v 'a'      >>
   get v

-- should print "a"
test3 = 
  newEmptyMVar         >>= \ v ->
  forkIO (put v 'a')   >>
  get v

-- should print "ab"   
test4 = 
  newEmptyMVar      >>= \ v1 ->
  newEmptyMVar      >>= \ v2 ->
  forkIO (p1 v1 v2) >>
  get v1          >>
  put v2 'b'
 where
  p1 v1 v2 = 
   put v1 'a'     >>
   get v2

-- used to abort: primPutMVar: full MVar
-- under the new semantics, it blocks leading to deadlock
test5 = 
  newEmptyMVar    >>= \ v ->
  put v 'a'       >>
  put v 'b'

-- tests multiple writes to the same mvar and fairness
-- should output a{bcd} and then deadlock
-- where {bcd} stands for some permutation of b c and d
test5a = do
  v <- newEmptyMVar
  put v 'a'
  forkIO $ put v 'b'
  forkIO $ put v 'c'
  forkIO $ put v 'd'
  get v
  get v
  get v
  get v
  get v

-- Tests blocking of two processes on the same variable.
-- should print "aa"
test6 = 
  newEmptyMVar         			>>= \ ack ->
  newEmptyMVar         			>>= \ a1 ->
  newEmptyMVar         			>>= \ a2 ->
  forkIO (putMVar a1 () >> get ack)  	>>
  forkIO (putMVar a2 () >> get ack)  	>>
  takeMVar a2     			>> 
  takeMVar a1     			>>
  put ack 'a'     			>>
  put ack 'a'

----------------------------------------------------------------
-- Non-deterministic tests below this point
-- Must be tested interactively and probably don't work using 
-- "logical concurrency".


-- should print interleaving of a's and b's
-- (degree of interleaving depends on granularity of concurrency)
test7 =
  forkIO a >> b
 where
  a = putStr "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  b = putStr "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

-- should give infinite interleaving of a's and b's
-- (degree of interleaving depends on granularity of concurrency)
-- Ming's example.  The Hugs read-eval-print loop gets confused if 
-- there's no type signature
test8 :: IO ()
test8 =
  forkIO a >> b
 where
  -- symbols carefully chosen to make them look very different on screen
  a = putChar 'a' >> a
  b = putChar 'B' >> b

-- Tests blocking of two processes on the same variable.
-- may print "ab" or "ba"
test9 = 
  newEmptyMVar         			>>= \ ack ->
  newEmptyMVar         			>>= \ a1 ->
  newEmptyMVar         			>>= \ a2 ->
  forkIO (putMVar a1 () >> get ack)  	>>
  forkIO (putMVar a2 () >> get ack)  	>>
  takeMVar a2     			>> 
  takeMVar a1     			>>
  put ack 'a'     			>>
  put ack 'b'

put v x =
  putMVar v x

get v =
  takeMVar v      >>= \ x ->
  putChar x

