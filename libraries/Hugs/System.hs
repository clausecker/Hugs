-----------------------------------------------------------------------------
-- Standard Library: System operations
--
-- Warning: the implementation of these functions in Hugs 98 is very weak.
-- The functions themselves are best suited to uses in compiled programs,
-- and not to use in an interpreter-based environment like Hugs.
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------

module Hugs.System (
	getArgs, getProgName, getEnv, 
	system
	) where

import Hugs.Prelude( ExitCode(..) )

primitive primArgc          :: IO Int
primitive primArgv          :: Int -> IO String

getArgs                     :: IO [String]
getArgs                      = do argc <- primArgc
               		          mapM primArgv [1..argc-1]

getProgName                 :: IO String
getProgName                  = primArgv 0

primitive getEnv            :: String -> IO String

system                      :: String -> IO ExitCode
system s                     = do r <- primSystem s
                                  return (toExitCode r)

primitive primSystem        :: String -> IO Int

toExitCode                  :: Int -> ExitCode
toExitCode 0                 = ExitSuccess
toExitCode n                 = ExitFailure n

-----------------------------------------------------------------------------
