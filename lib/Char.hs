-----------------------------------------------------------------------------
-- Standard Library: Char operations
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------

module Char 
	( Char

	   -- predicates
	, isAscii, isLatin1, isControl
    	, isPrint, isSpace, isUpper
	, isLower, isAlpha, isDigit
	, isOctDigit, isHexDigit, isAlphaNum -- :: Char -> Bool
	
	, toUpper, toLower   -- :: Char -> Char

	, digitToInt	     -- :: Char -> Int
	, intToDigit	     -- :: Int  -> Char
	
	, ord		     -- :: Char -> Int
	, chr		     -- :: Int  -> Char

	, readLitChar	     -- :: ReadS Char
	, showLitChar	     -- :: Char -> ShowS
	, lexLitChar	     -- :: ReadS String
	
	, String
	) where

-- This module is (almost) empty; Char operations are currently defined in
-- the prelude, but should eventually be moved to this library file instead.
-- No Unicode support yet. 

isLatin1 :: Char -> Bool
isLatin1 c = True -- c <= '\xff'

{- ToDo: delay moving these out of the Prelude.
isAscii, isControl, isPrint :: Char -> Bool
isAscii c   =  fromEnum c < 128
isControl c =  c < ' ' ||  c >= '\DEL' && c <= '\x9f'
isPrint c   =  not (isControl c)

-- Digit conversion operations
digitToInt :: Char -> Int
digitToInt c
  | isDigit c            =  fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' =  fromEnum c - fromEnum 'a' + 10
  | c >= 'A' && c <= 'F' =  fromEnum c - fromEnum 'A' + 10
  | otherwise            =  error "Char.digitToInt: not a digit"

intToDigit :: Int -> Char
intToDigit i
  | i >= 0  && i <=  9   =  toEnum (fromEnum '0' + i)
  | i >= 10 && i <= 15   =  toEnum (fromEnum 'a' + i - 10)
  | otherwise            =  error "Char.intToDigit: not a digit"

-}
-----------------------------------------------------------------------------
