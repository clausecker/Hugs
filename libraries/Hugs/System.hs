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

primitive primSetArgs       :: [String] -> IO ()
primitive primSetProgName   :: String -> IO ()

-- duplicated from Control.Exception
act `finally` sequel = do
    r <- act `catchException` \e -> sequel >> throw e
    sequel
    return r

withArgs :: [String] -> IO a -> IO a
withArgs args act = do
    old_args <- getArgs
    primSetArgs args
    act `finally` primSetArgs old_args

withProgName :: String -> IO a -> IO a
withProgName name act = do
    old_name <- getProgName
    primSetProgName name
    act `finally` primSetProgName old_name

primitive getEnv            :: String -> IO String

system                      :: String -> IO ExitCode
system s                     = do r <- primSystem s
                                  return (toExitCode r)

primitive primSystem        :: String -> IO Int

toExitCode                  :: Int -> ExitCode
toExitCode 0                 = ExitSuccess
toExitCode n                 = ExitFailure n

-----------------------------------------------------------------------------
