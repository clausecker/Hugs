-- This is a very cut-down version of GHC's Exception module
--
-- There are some big differences:
-- 1) The IOException type has a smaller set of different constructors.
--    This is because Hugs uses a different internal representation for
--    errors.
-- 2) It is not possible to catch certain kinds of exception in the current
--    Hugs implementation.  In particular, heap and stack overflow and
--    ctrl-C.  Indeed, it is not entirely clear what to do in response to
--    ctrl-C.
-- 3) We don't support an arbitrary throw.
--    It just isn't possible to throw an IOError from outside the
--    IO monad.  You can throw a HugsException or you can use throwIO.
-- 4) We don't support evaluate properly.

module Hugs.Exception(
        Exception(..),

	catchException,		-- :: IO a -> (Exception -> IO a) -> IO a

	evaluate,		-- :: a -> IO a

	-- Hugs-only stuff

	HugsException,
	catchHugsException,	-- :: IO a -> (HugsException -> IO a) -> IO a
	primThrowException,	-- :: HugsException -> a

	-- Exception predicates

	justIoErrors,		-- :: Exception -> Maybe IOError
	justUserErrors,		-- :: Exception -> Maybe String
        justHugsExceptions,     -- :: Exception -> Maybe HugsException

	-- Throwing exceptions

	throwIO,		-- :: Exception -> IO a

	-- Async exception control

        block,			-- :: IO a -> IO a
        unblock,		-- :: IO a -> IO a
  ) where

import Prelude hiding ( ioError )
import Hugs.Prelude hiding ( ioError )
import qualified Hugs.Prelude
import Hugs.IO

----------------------------------------------------------------
-- Exception datatype and operations
----------------------------------------------------------------

data Exception
  = IOException 	IOError		-- IO exceptions (from 'ioError')
  | HugsException  	HugsException	-- An error detected by Hugs

instance Show Exception where
  showsPrec _ (IOException err)	  = shows err
  showsPrec _ (HugsException exn) = shows exn

----------------------------------------------------------------
-- Primitive throw and catch
----------------------------------------------------------------

throwIO :: Exception -> IO a
throwIO (IOException err)   = Hugs.Prelude.ioError err
throwIO (HugsException exn) = primThrowException exn

ioError :: Exception -> IO a
ioError = throwIO

catchException :: IO a -> (Exception -> IO a) -> IO a
catchException m k = do
  (m `catchHugsException` (k . HugsException)) `Prelude.catch` (k . IOException)

----------------------------------------------------------------
-- evaluate
----------------------------------------------------------------

evaluate :: a -> IO a
evaluate a = a `seq` return a

----------------------------------------------------------------
-- dummy implementations of block and unblock
----------------------------------------------------------------

block, unblock :: IO a -> IO a
block   m = m
unblock m = m

----------------------------------------------------------------
-- Exception Predicates
----------------------------------------------------------------

justIoErrors		:: Exception -> Maybe IOError
justUserErrors		:: Exception -> Maybe String
justHugsExceptions	:: Exception -> Maybe HugsException

justIoErrors (IOException e) = Just e
justIoErrors _ = Nothing

justUserErrors (IOException e) | isUserError e = Just (ioeGetErrorString e)
justUserErrors _ = Nothing

justHugsExceptions (HugsException e) = Just e
justHugsExceptions _ = Nothing

----------------------------------------------------------------
-- End
----------------------------------------------------------------
