-----------------------------------------------------------------------------
-- Standard Library: IO operations, beyond those included in the prelude
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------

module Hugs.IO (
    Handle,          -- instances: Eq, Show.
    HandlePosn,      -- instances: Eq, Show.
    
    IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),
    BufferMode(NoBuffering,LineBuffering,BlockBuffering),
    SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),
    
    stdin, stdout, stderr,  -- :: Handle
    openFile,		    -- :: FilePath -> IOMode -> IO Handle
    hClose, 		    -- :: Handle -> IO ()

    hFileSize,		    -- :: Handle -> IO Integer

    hIsEOF,                 -- :: Handle -> IO Bool
    isEOF,                  -- :: IO Bool

    hSetBuffering,          -- :: Handle -> BufferMode -> IO ()
    hGetBuffering,          -- :: Handle -> IO BufferMode

    hFlush,                 -- :: Handle -> IO ()
    hGetPosn,		    -- :: Handle -> IO HandlePosn
    hSetPosn,               -- :: HandlePosn -> IO ()
    hSeek,                  -- :: Handle -> SeekMode -> Integer -> IO ()
    hTell,                  -- :: Handle -> IO Integer

    hLookAhead,             -- :: Handle -> IO Char

    hWaitForInput,          -- :: Handle -> Int -> IO Bool

    hGetChar,               -- :: Handle -> IO Char
    hGetLine,               -- :: Handle -> IO String
    hGetContents,           -- :: Handle -> IO String

    hPutChar,               -- :: Handle -> Char -> IO ()
    hPutStr,                -- :: Handle -> String -> IO ()

    hIsOpen,		    -- :: Handle -> IO Bool
    hIsClosed,		    -- :: Handle -> IO Bool
    hIsReadable,            -- :: Handle -> IO Bool
    hIsWritable,            -- :: Handle -> IO Bool
    hIsSeekable,            -- :: Handle -> IO Bool

    isAlreadyExistsError,   -- :: IOError -> Bool
    isDoesNotExistError,    -- :: IOError -> Bool
    isAlreadyInUseError,    -- :: IOError -> Bool
    isFullError,            -- :: IOError -> Bool
    isEOFError,             -- :: IOError -> Bool
    isIllegalOperation,     -- :: IOError -> Bool
    isPermissionError,      -- :: IOError -> Bool
    isUserError,            -- :: IOError -> Bool

    ioeGetErrorString,      -- :: IOError -> String
    ioeGetHandle,           -- :: IOError -> Maybe Handle
    ioeGetFileName,         -- :: IOError -> Maybe FilePath

    try,                    -- :: IO a -> IO (Either IOError a)
    bracket,                -- :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
    bracket_,               -- :: IO a -> (a -> IO b) -> IO c -> IO c

    -- Non-standard extensions 
    hugsIsEOF,              -- :: IO Bool
    hugsHIsEOF,             -- :: Handle  -> IO Bool
    hugsIsSearchErr,        -- :: IOError -> Bool
    hugsIsNameErr,          -- :: IOError -> Bool
    hugsIsWriteErr,         -- :: IOError -> Bool
    ) where

import Hugs.Prelude

data Handle
instance Eq Handle where (==) = primEqHandle
primitive primEqHandle :: Handle -> Handle -> Bool

data IOMode      =  ReadMode | WriteMode | AppendMode | ReadWriteMode
                    deriving (Eq, Ord, Ix, Bounded, Enum, Read, Show)
data BufferMode  =  NoBuffering | LineBuffering 
                 |  BlockBuffering (Maybe Int)
                    deriving (Eq, Ord, Read, Show)
data SeekMode    =  AbsoluteSeek | RelativeSeek | SeekFromEnd
                    deriving (Eq, Ord, Ix, Bounded, Enum, Read, Show)

primitive stdin       :: Handle
primitive stdout      :: Handle
primitive stderr      :: Handle
primitive openFile    :: FilePath -> IOMode -> IO Handle
primitive hClose      :: Handle -> IO ()

primitive hFileSize   :: Handle -> IO Integer

primitive hIsEOF      :: Handle -> IO Bool

isEOF               :: IO Bool
isEOF                = hIsEOF stdin

hSetBuffering       :: Handle  -> BufferMode -> IO ()
hSetBuffering h bMode = 
  case bMode of
    NoBuffering   -> hSetBuff h 0 0
    LineBuffering -> hSetBuff h 1 0
    BlockBuffering (Just x) -> hSetBuff h 2 x
    BlockBuffering _        -> hSetBuff h 2 0

primitive hSetBuff  :: Handle -> Int -> Int -> IO ()

hGetBuffering       :: Handle  -> IO BufferMode
hGetBuffering h = do
  (k, sz) <- hGetBuff h
  case k of
    1 -> return NoBuffering
    2 -> return LineBuffering
    3 -> return (BlockBuffering (Just sz))
     -- fatal - never to happen.
    _ -> error "IO.hGetBuffering: unknown buffering mode"

primitive hGetBuff :: Handle -> IO (Int,Int)

primitive hFlush   :: Handle -> IO ()

data HandlePosn = HandlePosn Handle Int deriving Eq

hGetPosn :: Handle -> IO HandlePosn
hGetPosn h = do
  p <- hGetPosnPrim h
  return (HandlePosn h p)

hTell :: Handle -> IO Integer
hTell h = do
  p <- hGetPosnPrim h
  return (toInteger p)

primitive hGetPosnPrim :: Handle -> IO Int

hSetPosn :: HandlePosn -> IO ()
hSetPosn (HandlePosn h p) = hSetPosnPrim h p

primitive hSetPosnPrim  :: Handle -> Int -> IO () 

hSeek :: Handle -> SeekMode -> Integer -> IO () 
hSeek h sMode int 
 | int >  fromIntegral (maxBound :: Int) ||
   int <  fromIntegral (minBound :: Int) =
   ioError (userError ("IO.hSeek: seek offset out of supported range"))
 | otherwise = 
   hSeekPrim h (fromEnum sMode) ((fromIntegral int)::Int)
  
primitive hSeekPrim :: Handle -> Int -> Int -> IO () 

primitive hWaitForInput :: Handle -> Int -> IO Bool

primitive hGetChar    :: Handle -> IO Char

hGetLine   :: Handle -> IO String
hGetLine h = do
  c <- hGetChar h
  if c=='\n'
   then return ""
   else do
     ls <- getRest 
     return (c:ls)
  where
   getRest = do
     c <- catch (hGetChar h)
                (\ ex -> if isEOFError ex then 
			    return '\n'
			 else
			    ioError ex)
     if c=='\n'
      then return ""
      else do
       cs <- getRest 
       return (c:cs)


primitive hLookAhead    :: Handle -> IO Char
primitive hGetContents  :: Handle -> IO String
primitive hPutChar      :: Handle -> Char -> IO ()
primitive hPutStr       :: Handle -> String -> IO ()

primitive hIsOpen,    
   	  hIsClosed,  
   	  hIsReadable,
   	  hIsWritable,
	  hIsSeekable :: Handle -> IO Bool

isIllegalOperation, 
	  isAlreadyExistsError, 
	  isDoesNotExistError, 
          isAlreadyInUseError,   
	  isFullError,     
          isEOFError, 
	  isPermissionError,
          isUserError        :: IOError -> Bool

isIllegalOperation   ioe = ioe_kind ioe == IOError_IllegalError
isAlreadyExistsError ioe = ioe_kind ioe == IOError_AlreadyExists
isDoesNotExistError  ioe = ioe_kind ioe == IOError_DoesNotExist
isAlreadyInUseError  ioe = ioe_kind ioe == IOError_AlreadyInUse
isFullError          ioe = ioe_kind ioe == IOError_FullError
isEOFError           ioe = ioe_kind ioe == IOError_EOF
isPermissionError    ioe = ioe_kind ioe == IOError_PermDenied
isUserError          ioe = ioe_kind ioe == IOError_UserError

ioeGetErrorString :: IOError -> String
ioeGetErrorString ioe
 | isUserError ioe = ioe_description ioe
 | otherwise       = show (ioe_kind ioe)

primitive ioeGetHandle      :: IOError -> Maybe Handle

ioeGetFileName    :: IOError -> Maybe FilePath
ioeGetFileName ioe = ioe_fileName ioe

try       :: IO a -> IO (Either IOError a)
try p      = catch (p >>= (return . Right)) (return . Left)

bracket        :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after m = do
        x  <- before
        rs <- try (m x)
        after x
        case rs of
           Right r -> return r
           Left  e -> ioError e

-- variant of the above where middle computation doesn't want x
bracket_        :: IO a -> (a -> IO b) -> IO c -> IO c
bracket_ before after m = do
         x  <- before
         rs <- try m
         after x
         case rs of
            Right r -> return r
            Left  e -> ioError e

-----------------------------------------------------------------------------
-- Non-standard extensions 
-- (likely to disappear when IO library is more complete)
--
-- keep them around for now.

-- C library style test for EOF (doesn't obey Haskell semantics)
primitive hugsHIsEOF "hugsHIsEOF" :: Handle -> IO Bool
hugsIsEOF             :: IO Bool
hugsIsEOF              = hugsHIsEOF stdin

hugsIsNameErr  = isIllegalOperation
hugsIsWriteErr = isAlreadyExistsError

hugsIsSearchErr :: IOError -> Bool
hugsIsSearchErr = isDoesNotExistError

-----------------------------------------------------------------------------
