--!!! Testing the IO module
module Main where

import IO
-- assume we've got a Directory impl
import Directory ( removeFile )


exHandler :: (IOError -> Bool)
	  -> IO ()
	  -> IO ()
exHandler pred x = x `catch` (\ err -> if pred err then 
					   putStrLn "got expected IO error"
					else
					   print err)


-- should fail with isDoesNotExistError
io1 :: IO ()
io1 = exHandler isDoesNotExistError $ do
  h <- openFile "some_non_existing_file" ReadMode
  hClose h
  return ()

io2 :: IO ()
io2 = exHandler (const False) $ do
  h <- openFile "some_non_existing_file" WriteMode
  hClose h
  io1
  removeFile "some_non_existing_file"
  return ()

io3 :: IO ()
io3 = exHandler (const False) $ do
  h <- openFile "some_non_existing_file" ReadWriteMode
  hClose h
  io1
  removeFile "some_non_existing_file"
  return ()

-- testing whether hGetChar returns EOF error.
io4 :: IO ()
io4 = exHandler (const False) $ do
  writeFile "io4_test_file" "ab"
  h <- openFile "io4_test_file" ReadMode
  exHandler isEOFError (loop h)
  hClose h
  removeFile "io4_test_file"
 where
  loop h = do
     x <- hGetChar h
     loop h

-- repeated hCloses is an error; should raise isIllegalOperation
io5 :: IO ()
io5 = exHandler (const False) $ do
  h <- openFile "io5_test_file" WriteMode
  hClose h
  exHandler isIllegalOperation (hClose h)
  removeFile "io5_test_file" `catch` (\ _ -> return ())
  return ()

-- hFileSize test
io6 :: IO ()
io6 = exHandler (const False) $ do
  writeFile "io6_test_file" "abcde"
  h  <- openFile "io6_test_file" ReadMode
  sz <- hFileSize h
  hClose h
  putStrLn ("File size: " ++ show sz)
  removeFile "io6_test_file"
  return ()

-- hIsEOF test
io7 :: IO ()
io7 = exHandler (const False) $ do
  writeFile "io7_test_file" "abcde"
  h  <- openFile "io7_test_file" ReadMode
  exHandler isEOFError (loop h)
  hClose h
  removeFile "io7_test_file"
  return ()
 where
  loop h = do
     x   <- hGetChar h
     flg <- hIsEOF h
     print (x,flg)
     loop h

-- handle buffering
io8 :: IO ()
io8 = exHandler (const False) $ do
  h   <- openFile "io8_test_file" WriteMode
  buf0 <- hGetBuffering h
  hSetBuffering h NoBuffering
  buf <- hGetBuffering h
  print buf
  hSetBuffering h LineBuffering
  buf <- hGetBuffering h
  print buf
  hSetBuffering h (BlockBuffering Nothing)
  buf <- hGetBuffering h
  print (buf==buf0)
  hSetBuffering h (BlockBuffering (Just 23))
  buf <- hGetBuffering h
  print buf
  hClose h
  removeFile "io8_test_file"
  return ()

-- flushing
io9 :: IO ()
io9 = exHandler (const False) $ do
  h   <- openFile "io9_test_file" WriteMode
  hFlush h
  hPutChar h 'a'
  hFlush h
  hClose h
  h   <- openFile "io9_test_file" ReadMode
  exHandler (isIllegalOperation) $ hFlush h
  ch <- hGetChar h
  exHandler (isIllegalOperation) $ hFlush h
  hClose h
  print ch
  removeFile "io9_test_file"
  return ()

-- getting/setting file posns.
io10 :: IO ()
io10 = exHandler (const False) $ do
  writeFile "io10_test_file" "abcdefg"
  h   <- openFile "io10_test_file" ReadMode
  hGetChar h >>= print
  hGetChar h >>= print
  pos <- hGetPosn h
  hGetChar h >>= print
  hSetPosn pos
  hGetChar h >>= print
  hClose h
  removeFile "io10_test_file"
  return ()
  
  
  

