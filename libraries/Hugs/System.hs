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
	getArgs, getProgName, withArgs, withProgName, getEnv,
	system
	) where

import Hugs.Prelude( ExitCode(..), catchException, throw )

primitive getArgs     "primGetArgs"     :: IO [String]
primitive getProgName "primGetProgName" :: IO String

primitive setArgs     "primSetArgs"     :: [String] -> IO ()
primitive setProgName "primSetProgName" :: String -> IO ()

-- Run an action with a value temporarily overridden
-- (a special case of Control.Exception.bracket)
with :: IO a -> (a -> IO ()) -> a -> IO b -> IO b
with getVal setVal newVal act = do
    oldVal <- getVal
    setVal newVal
    r <- act `catchException` \e -> setVal oldVal >> throw e
    setVal oldVal
    return r

withArgs :: [String] -> IO a -> IO a
withArgs = with getArgs setArgs

withProgName :: String -> IO a -> IO a
withProgName = with getProgName setProgName

primitive getEnv            :: String -> IO String

system                      :: String -> IO ExitCode
system s                     = do r <- primSystem s
                                  return (toExitCode r)

primitive primSystem        :: String -> IO Int

toExitCode                  :: Int -> ExitCode
toExitCode 0                 = ExitSuccess
toExitCode n                 = ExitFailure n

-----------------------------------------------------------------------------
