-- This is a very cut-down version of GHC's Exception module
--
-- There are two big differences:
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
-- 
-- The code is directly based on the code in GHC.

module Exception( 
        Exception(..),

	try,       -- :: IO a -> IO (Either Exception a)
	tryJust,   -- :: (Exception -> Maybe b) -> a    -> IO (Either b a)

	catch,     -- :: IO a -> (Exception -> IO a) -> IO a
	catchJust, -- :: (Exception -> Maybe b) -> IO a -> (b -> IO a) -> IO a

	tryAll,    -- :: a    -> IO (Either Exception a)
	tryAllIO,  -- :: IO a -> IO (Either Exception a)

	catchAll,  -- :: a    -> (Exception -> IO a) -> IO a
	catchAllIO,-- :: IO a -> (Exception -> IO a) -> IO a

	evaluate,  -- :: a -> IO a

	-- Exception predicates

	justIoErrors,		-- :: Exception -> Maybe IOError
        justHugsExceptions,     -- :: Exception -> Maybe HugsException

	-- Throwing exceptions

	throwIO,		-- :: Exception -> IO a

	-- Async exception control

        block,          -- :: IO a -> IO a
        unblock,        -- :: IO a -> IO a

	-- Utilities
		
	bracket,  	-- :: IO a -> (a -> IO b) -> (a -> IO c) -> IO ()
	bracket_, 	-- :: IO a -> IO b -> IO c -> IO ()
        finally,        -- :: IO a -> IO b
  ) where

import Prelude hiding (catch)
import qualified Prelude ( catch )

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
-- Primitive catch
----------------------------------------------------------------

throwIO :: Exception -> IO a
throwIO (IOException err)   = ioError err
throwIO (HugsException exn) = primThrowException exn

catchException :: IO a -> (Exception -> IO a) -> IO a
catchException m k = do
  (m `catchHugsException` (k . HugsException)) `Prelude.catch` (k . IOException)

----------------------------------------------------------------
-- Catching Exceptions
----------------------------------------------------------------

catch  :: IO a  -> (Exception -> IO a) -> IO a
catch a handler = catchException a handler

catchAll  :: a  -> (Exception -> IO a) -> IO a
catchAll a handler = catchException (a `seq` return a) handler

catchAllIO :: IO a -> (Exception -> IO a) -> IO a
catchAllIO =  catchException

catchJust :: (Exception -> Maybe b) -> IO a -> (b -> IO a) -> IO a
catchJust p a handler = catchAllIO a handler'
  where handler' e = case p e of 
			Nothing -> throwIO e
			Just b  -> handler b

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
-- 'try' and variations.
----------------------------------------------------------------

tryAll :: a    -> IO (Either Exception a)
tryAll a = catchException (a `seq` return (Right a)) (\e -> return (Left e))

tryAllIO :: IO a -> IO (Either Exception a)
tryAllIO = try

try :: IO a -> IO (Either Exception a)
try a = catchAllIO (a >>= \ v -> return (Right v))
			(\e -> return (Left e))

tryJust :: (Exception -> Maybe b) -> a -> IO (Either b a)
tryJust p a = do
  r <- tryAll a
  case r of
	Right v -> return (Right v)
	Left  e -> case p e of
			Nothing -> throwIO e
			Just b  -> return (Left b)

bracket        :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after m = do
        x  <- before
        rs <- tryAllIO (m x)
        after x
        case rs of
           Right r -> return r
           Left  e -> throwIO e

bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ before after thing = bracket before (const after) (const thing)

finally :: IO a -> IO b -> IO b
finally m k = tryAllIO m >> k

----------------------------------------------------------------
-- Exception Predicates
----------------------------------------------------------------

justIoErrors		:: Exception -> Maybe IOError
justHugsExceptions      :: Exception -> Maybe HugsException

justIoErrors (IOException e) = Just e
justIoErrors _ = Nothing

justHugsExceptions (HugsException e) = Just e
justHugsExceptions _ = Nothing

----------------------------------------------------------------
-- End
----------------------------------------------------------------
