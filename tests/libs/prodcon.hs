-- !!! Testing derived Concurrency primitives and producer-consumer code

-- NB  Code which uses cooperative scheduling can be sensitive to the
-- scheduling order if it calls any IO operations which might block
-- waiting for input from the user, from X11, from Win32, etc.
-- Changes to the scheduling order could break libraries which use
-- cooperative scheduling and should be avoided if at all possible.
--
-- This file will detect changes in the scheduling order.
-- (The most important test is t5 which checks the producer-consumer pattern.
-- Its output should look like this: aabbccddeeff.  If it looks more like
-- abacbcd... then it is definitely broken.)
--
-- If the change is unavoidable, the authors of libraries which use
-- concurrency should be informed and asked to check that nothing has
-- broken.  These libraries include: 
-- o HGL (Alastair Reid)
-- o Fran (Conal Elliott)
-- o FRP (John Peterson)

import Concurrent

par_ m1 m2 = do
  v1 <- newEmptyMVar 
  v2 <- newEmptyMVar 
  forkIO (m1 >> putMVar v1 ())
  print '1'
  forkIO (m2 >> putMVar v2 ())
  print '2'
  takeMVar v1
  print '3'
  takeMVar v2
  print '4'
  return ()

t1 = par_ (print 'a') (print 'b')
t2 = par_ (print 'a') (par_ (print 'b') (return ()))
t3 = par_ (par_ (print 'a') (print 'b')) (return ())

prot :: MVar () -> IO a -> IO a
prot crit m = do
  x <- takeMVar crit
  a <- m
  putMVar crit x
  return a

task crit c 0     = return ()
task crit c (n+1) = do
  prot crit (putChar c)
  task crit c n

t4 = do
  crit <- newMVar ()
  forkIO $ task crit 'a' 10
  forkIO $ task crit 'b' 10
  task crit 'c' 10

producer 0 mv = do
  return ()
producer (n+1) mv = do
  yield
  c <- getChar 
  writeCVar mv c
  producer n mv

consumer mv = do
  c <- readCVar mv
  putChar c
  consumer mv

t5 = do
  mv <- newCVar 
  forkIO $ producer 10 mv
  forkIO $ consumer mv
  return ()