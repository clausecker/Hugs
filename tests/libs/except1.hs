-- !!! Testing interaction of exception handling with threads

-- As with prodcon, this test is sensitive to the order that threads
-- get scheduled but what it's really testing is that the right errors
-- get caught.

import Exception
import Concurrent
import IOExts
import Prelude hiding (catch)

-- This is a test to check for a suspected type error.
-- There's no type error because one of the invariants of the system
-- is that the thread list contains at most one thread which will
-- Hugs_Return a value
t1 :: IO ()
t1 = do
  r <- fixIO (\ _ -> do
         forkIO (return ())
         yield
         return True
         )
  print (r :: Bool)


-- Experiment to check if yielding a thread preserves the exception handler
t2 :: IO ()
t2 = do
  try (yield >> print "Foo" >> lose2 "foo")
  print "Bar"

try2 :: IO a -> IO (Either Exception a)
try2 m = catch (m >>= return . Right) (return . Left)

lose1 x = ioError (userError x)
lose2 x = error x

test :: String -> IO () -> IO ()
test x m = do
  y <- try m
--  y <- try2 m
  either (\ _ -> putStrLn (x ++ " failed"))
         (\ _ -> putStrLn (x ++ " worked"))
         y

-- Use these guys to build up stacks of threads that fail in various ways
t3, t3', t3'' :: IO () -> IO ()
t3 m = test "a" $ forkIO $ test "b" $ m
t3' m = test "a" $ test "b" $ m
t3'' m = test "a" $ forkIO $ m

t4, t5, t6 :: IO ()
t4 = lose1 "x"
t5 = lose2 "x"
t6 = putStrLn "y"
