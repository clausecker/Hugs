--!!! Testing the Directory module
module Main where

import Directory
import IO
import List ( isPrefixOf )


exHandler :: (IOError -> Bool)
	  -> IO ()
	  -> IO ()
exHandler pred x = x `catch` (\ err -> if pred err then 
					   putStrLn "got expected IO error"
					else
					   print err)

-- testing file/dir predicates
dir1 :: IO ()
dir1 = exHandler (const False) $ do
  let tstName = "dir1_test_file"
  flg <- doesFileExist tstName
  putStrLn ("Does the '" ++ tstName ++ "' file exist? " ++ show flg)
  createDirectory tstName
  flg <- doesFileExist tstName
  putStrLn ("Does the '" ++ tstName ++ "' file exist? " ++ show flg)
  flg <- doesDirectoryExist tstName
  putStrLn ("Does the '" ++ tstName ++ "' directory exist? " ++ show flg)
  removeDirectory tstName
  return ()

-- removeDirectory error handling
dir2 :: IO ()
dir2 = exHandler (isDoesNotExistError) $ do  
  removeDirectory "dir2_test_file"
  
-- createDirectory error handling
dir3 :: IO ()
dir3 = exHandler (const False) $ do
    createDirectory "dir3_test_file"
    exHandler (isAlreadyExistsError) (createDirectory "dir3_test_file")
    removeDirectory "dir3_test_file"
  
-- removeFile error handling
dir4 :: IO ()
dir4 = exHandler (const False) $ do
    writeFile "dir4_test_file" "ab"
    removeFile "dir4_test_file"
    exHandler (isDoesNotExistError) (removeFile "dir4_test_file")
    return ()

-- getDirectoryContents 
dir5 :: IO ()
dir5 = exHandler (const False) $ do
    createDirectory "dir5_test_dir"
    writeFile "dir5_test_dir/a" "ab"
    writeFile "dir5_test_dir/b" "cd"
    getDirectoryContents "dir5_test_dir" >>= print
    removeFile "dir5_test_dir/b"
    getDirectoryContents "dir5_test_dir" >>= print
    removeFile "dir5_test_dir/a"
    getDirectoryContents "dir5_test_dir" >>= print
    removeDirectory "dir5_test_dir"
    return ()
    
-- get/setCurrentDirectory
dir6 :: IO ()
dir6 = exHandler (const False) $ do
    orig <- getCurrentDirectory
    createDirectory "dir6_test_dir"
    setCurrentDirectory "dir6_test_dir"
    dir <- getCurrentDirectory
      -- remove prefix (so that the testing output doesn't need to use an abs. path here)
    print (if orig `isPrefixOf` dir then drop (length orig + 1) dir else dir)
    setCurrentDirectory orig
    removeDirectory "dir6_test_dir"
    return ()


-- permissions
dir7 :: IO ()
dir7 = exHandler (const False) $ do
    createDirectory "dir7_test_dir"
    perms <- getPermissions "dir7_test_dir"
    print perms
    writeFile "dir7_test_dir/a" "a"
    perms <- getPermissions "dir7_test_dir/a"
    print perms
    setPermissions "dir7_test_dir/a" perms{readable=False}
    perms <- getPermissions "dir7_test_dir/a"
    print perms
    exHandler (isPermissionError) (readFile "dir7_test_dir/a" >> return ())
    removeFile "dir7_test_dir/a"
    removeDirectory "dir7_test_dir"
    return ()

