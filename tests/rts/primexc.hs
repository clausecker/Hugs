-- !!! Testing primitive exception support

import Hugs.Prelude( HugsException, IO(..) )

-- data HugsException

-- Note that these primitives break referential transparency.
-- They should not be exported to the user in this form.
-- They should always be wrapped in a referentially transparent
-- cover.

primitive primCatchException :: a -> Either HugsException a
-- primitive primThrowException :: HugsException -> a
primitive primShowException  :: HugsException -> String

-- One level of error catching
test1 :: Int -> IO ()
test1 x = case primCatchException x of
  Left err -> do
    putStr "Caught error: "
    putStrLn $ primShowException err
  Right x -> do
    print x

-- Exception catching in the IO monad
-- Won't behave correctly if combined with threads - use the Prelude
-- code for that!
catch' :: IO a -> (HugsException -> IO a) -> IO a
catch' (IO m) h = IO (\ f s -> 
  case primCatchException (m f s) of
    Left exn -> case h exn of { (IO h') -> h' f s }
    Right m' -> m'
  )

t1 :: IO ()
t1 = do
  putStrLn "Start of test 1"
  test1 3
  test1 (error "a")
  test1 (div 1 0)
  putStrLn "End of test 1"

t2 :: IO ()
t2 = do
  putStrLn "Start of test 2"
  err1 `catch'` (\exn -> putStrLn $ "caught error " ++ show exn)
  err2 `catch'` (\exn -> putStrLn $ "caught error " ++ show exn)
  ok   `catch'` (\exn -> putStrLn $ "caught error " ++ show exn)
  putStrLn "End of test 2"

err1 :: a
err1 = error "b"

err2 :: IO ()
err2 = if 42 == div 1 0 then return () else return ()

ok :: IO ()
ok = putStrLn "ok"
