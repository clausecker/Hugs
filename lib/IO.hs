-----------------------------------------------------------------------------
-- Standard Library: IO operations, beyond those included in the prelude
--
-- WARNING: The names and semantics of functions defined in this module
-- may change as the details of the IO standard are clarified.
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------

module IO (
    Handle, HandlePosn,
--  IOMode(ReadMode,WriteMode,AppendMode,ReadWriteMode),
    IOMode(ReadMode,WriteMode,AppendMode),
    BufferMode(NoBuffering,LineBuffering,BlockBuffering),
    SeekMode(AbsoluteSeek,RelativeSeek,SeekFromEnd),
    stdin, stdout, stderr, 
    openFile, hClose, 
--  hFileSize, hIsEOF, isEOF,
--  hSetBuffering, hGetBuffering, hFlush, 
    hFlush, 
    hGetPosn, hSetPosn, 
--  hSeek, hIsSeekable,
--  hReady, hGetChar, hLookAhead, hGetContents, 
    hGetChar, hGetLine, hGetContents, 
    hPutChar, hPutStr, hPutStrLn, hPrint,
    hIsOpen, hIsClosed, hIsReadable, hIsWritable, 
    isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError, 
    isFullError, isEOFError,
    isIllegalOperation, isPermissionError, isUserError, 
    ioeGetErrorString, ioeGetHandle, ioeGetFileName,
    try, bracket, bracket_,

    -- Non-standard extensions 
    hugsIsEOF, hugsHIsEOF,
    hugsIsSearchErr, hugsIsNameErr, hugsIsWriteErr,

    -- ... and what the Prelude exports
    IO,
    FilePath, IOError, ioError, userError, catch,
    putChar, putStr, putStrLn, print,
    getChar, getLine, getContents, interact,
    readFile, writeFile, appendFile, readIO, readLn
    ) where

import Ix(Ix)

data Handle
instance Eq Handle where (==) = primEqHandle
primitive primEqHandle :: Handle -> Handle -> Bool
newtype HandlePosn = HandlePosn Int deriving Eq

--data IOMode      =  ReadMode | WriteMode | AppendMode | ReadWriteMode
data IOMode      = ReadMode | WriteMode | AppendMode
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
--Not yet implemented:
--hFileSize           :: Handle -> IO Integer
--hIsEOF              :: Handle -> IO Bool
--isEOF               :: IO Bool
--isEOF                = hIsEOF stdin

--hSetBuffering       :: Handle  -> BufferMode -> IO ()
--hGetBuffering       :: Handle  -> IO BufferMode
primitive hFlush      :: Handle -> IO ()
primitive hGetPosn    :: Handle -> IO HandlePosn
primitive hSetPosn    :: HandlePosn -> IO () 
--hSeek               :: Handle -> SeekMode -> Integer -> IO () 

--hWaitForInput	      :: Handle -> Int -> IO Bool
--hReady              :: Handle -> IO Bool 
--hReady h	       = hWaitForInput h 0
primitive hGetChar    :: Handle -> IO Char

hGetLine              :: Handle -> IO String
hGetLine h             = do c <- hGetChar h
                            if c=='\n' then return ""
                              else do cs <- hGetLine h
                                      return (c:cs)

--hLookAhead          :: Handle -> IO Char
primitive hGetContents:: Handle -> IO String
primitive hPutChar    :: Handle -> Char -> IO ()
primitive hPutStr     :: Handle -> String -> IO ()

hPutStrLn             :: Handle -> String -> IO ()
hPutStrLn h s          = do { hPutStr h s; hPutChar h '\n' }

hPrint                :: Show a => Handle -> a -> IO ()
hPrint h               = hPutStrLn h . show

primitive hIsOpen,    
   	  hIsClosed,  
   	  hIsReadable,
   	  hIsWritable :: Handle -> IO Bool
--hIsSeekable         :: Handle -> IO Bool

primitive isIllegalOperation, 
	  isAlreadyExistsError, 
	  isDoesNotExistError, 
          isAlreadyInUseError,   
	  isFullError,     
          isEOFError, 
	  isPermissionError,
          isUserError        :: IOError -> Bool

primitive ioeGetErrorString "primShowIOError" :: IOError -> String
primitive ioeGetHandle      :: IOError -> Maybe Handle
primitive ioeGetFileName    :: IOError -> Maybe FilePath

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

-- C library style test for EOF (doesn't obey Haskell semantics)
primitive hugsHIsEOF "hIsEOF" :: Handle -> IO Bool
hugsIsEOF             :: IO Bool
hugsIsEOF              = hugsHIsEOF stdin

primitive hugsIsSearchErr :: IOError -> Bool
primitive hugsIsNameErr   :: IOError -> Bool
primitive hugsIsWriteErr  :: IOError -> Bool

-----------------------------------------------------------------------------
