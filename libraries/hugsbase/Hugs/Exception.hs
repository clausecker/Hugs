-- This is a cut-down version of GHC's Exception module
--
-- The main difference is that Hugs does not throw asynchronous
-- exceptions, in particular heap and stack overflow and ctrl-C.
-- Indeed, it is not entirely clear what to do in response to ctrl-C.

module Hugs.Exception(
        SomeException(..),
	IOException(..),
	ArithException(..),
	ArrayException(..),

	catchException,		-- :: IO a -> (Exception -> IO a) -> IO a
	throwIO,		-- :: Exception -> IO a
	throw,			-- :: Exception -> a
	evaluate,		-- :: a -> IO a
  ) where

import Hugs.Prelude

----------------------------------------------------------------
-- Primitive throw and catch
----------------------------------------------------------------

throwIO :: SomeException -> IO a
throwIO exn = IO (\ s -> throw exn)

evaluate :: a -> IO a
evaluate x = IO (\ s -> x `seq` s x)

----------------------------------------------------------------
-- End
----------------------------------------------------------------
