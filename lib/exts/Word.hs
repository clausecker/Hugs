-----------------------------------------------------------------------------
-- Unsigned Integers
-- Suitable for use with Hugs 98 on 32 bit systems.
-----------------------------------------------------------------------------
module Word
	( Word8
	, Word16
	, Word32
	, Word64
	, word8ToWord32  -- :: Word8  -> Word32
	, word32ToWord8  -- :: Word32 -> Word8
	, word16ToWord32 -- :: Word16 -> Word32
	, word32ToWord16 -- :: Word32 -> Word16
	, word8ToInt     -- :: Word8  -> Int
	, intToWord8     -- :: Int    -> Word8
	, word16ToInt    -- :: Word16 -> Int
	, intToWord16    -- :: Int    -> Word16
	, word32ToInt    -- :: Word32 -> Int
	, intToWord32    -- :: Int    -> Word32
	) where
import Bits
import Int

-----------------------------------------------------------------------------
-- The "official" coercion functions
-----------------------------------------------------------------------------

word8ToWord32  :: Word8  -> Word32
word32ToWord8  :: Word32 -> Word8
word16ToWord32 :: Word16 -> Word32
word32ToWord16 :: Word32 -> Word16

word8ToInt   :: Word8  -> Int
intToWord8   :: Int    -> Word8
word16ToInt  :: Word16 -> Int
intToWord16  :: Int    -> Word16

word8ToInt  = word32ToInt    . word8ToWord32
intToWord8  = word32ToWord8  . intToWord32
word16ToInt = word32ToInt    . word16ToWord32
intToWord16 = word32ToWord16 . intToWord32

primitive intToWord32 "intToWord" :: Int    -> Word32
primitive word32ToInt "wordToInt" :: Word32 -> Int

-----------------------------------------------------------------------------
-- Word8
-----------------------------------------------------------------------------

newtype Word8  = W8 Word32

word8ToWord32 (W8 x) = x .&. 0xff
word32ToWord8 = W8

instance Eq  Word8     where (==)    = binop (==)
instance Ord Word8     where compare = binop compare

instance Num Word8 where
    x + y         = to (binop (+) x y)
    x - y         = to (binop (-) x y)
    negate        = to . negate . from
    x * y         = to (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = to . primIntegerToWord
    fromInt       = intToWord8

instance Bounded Word8 where
    minBound = 0
    maxBound = 0xff

instance Real Word8 where
    toRational x = toInteger x % 1

instance Integral Word8 where
    x `div` y     = to  (binop div x y)
    x `quot` y    = to  (binop quot x y)
    x `rem` y     = to  (binop rem x y)
    x `mod` y     = to  (binop mod x y)
    x `quotRem` y = to2 (binop quotRem x y)
    divMod        = quotRem
    even          = even      . from
    toInteger     = toInteger . from
    toInt         = word8ToInt

instance Ix Word8 where
    range (m,n)          = [m..n]
    index b@(m,n) i
	   | inRange b i = word32ToInt (from (i - m))
	   | otherwise   = error "index: Index out of range"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word8 where
    toEnum         = to . intToWord32
    fromEnum       = word32ToInt . from
    enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Word8)]
    enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Word8)]
		       where last = if d < c then minBound else maxBound

instance Read Word8 where
    readsPrec p = readDec

instance Show Word8 where
    showsPrec p = showInt  -- a particularily counterintuitive name!

instance Bits Word8 where
  x .&. y       = to (binop (.&.) x y)
  x .|. y       = to (binop (.|.) x y)
  x `xor` y     = to (binop xor x y)
  complement    = to . complement . from
  x `shift` i   = to (from x `shift` i)
  x `rotate` i  = to (from x `rot` i)
    where rot = primRotateWord 8
  bit           = to . bit
  setBit x i    = to (setBit (from x) i)
  clearBit x i  = to (clearBit (from x) i)
  complementBit x i = to (complementBit (from x) i)
  testBit x i   = testBit (from x) i
  bitSize  _    = 8
  isSigned _    = False

-----------------------------------------------------------------------------
-- Word16
-----------------------------------------------------------------------------

newtype Word16 = W16 Word32

word16ToWord32 (W16 x) = x .&. 0xffff
word32ToWord16 = W16

instance Eq  Word16     where (==)    = binop (==)
instance Ord Word16     where compare = binop compare

instance Num Word16 where
    x + y         = to (binop (+) x y)
    x - y         = to (binop (-) x y)
    negate        = to . negate . from
    x * y         = to (binop (*) x y)
    abs           = absReal
    signum        = signumReal
    fromInteger   = to . primIntegerToWord
    fromInt       = intToWord16

instance Bounded Word16 where
    minBound = 0
    maxBound = 0xffff

instance Real Word16 where
  toRational x = toInteger x % 1

instance Integral Word16 where
  x `div` y     = to  (binop div x y)
  x `quot` y    = to  (binop quot x y)
  x `rem` y     = to  (binop rem x y)
  x `mod` y     = to  (binop mod x y)
  x `quotRem` y = to2 (binop quotRem x y)
  divMod        = quotRem
  even          = even      . from
  toInteger     = toInteger . from
  toInt         = word16ToInt

instance Ix Word16 where
  range (m,n)          = [m..n]
  index b@(m,n) i
         | inRange b i = word32ToInt (from (i - m))
         | otherwise   = error "index: Index out of range"
  inRange (m,n) i      = m <= i && i <= n

instance Enum Word16 where
  toEnum         = to . intToWord32
  fromEnum       = word32ToInt . from
  enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Word16)]
  enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Word16)]
		       where last = if d < c then minBound else maxBound

instance Read Word16 where
  readsPrec p = readDec

instance Show Word16 where
  showsPrec p = showInt  -- a particularily counterintuitive name!

instance Bits Word16 where
  x .&. y       = to (binop (.&.) x y)
  x .|. y       = to (binop (.|.) x y)
  x `xor` y     = to (binop xor x y)
  complement    = to . complement . from
  x `shift` i   = to (from x `shift` i)
  x `rotate` i  = to (from x `rot` i)
    where rot = primRotateWord 16
  bit           = to . bit
  setBit x i    = to (setBit (from x) i)
  clearBit x i  = to (clearBit (from x) i)
  complementBit x i = to (complementBit (from x) i)
  testBit x i   = testBit (from x) i
  bitSize  _    = 16
  isSigned _    = False

-----------------------------------------------------------------------------
-- Word32
-----------------------------------------------------------------------------

data Word32     -- builtin datatype of 32 bit naturals

instance Eq  Word32     where (==)    = primEqWord
instance Ord Word32     where compare = primCmpWord

instance Num Word32 where
    (+)           = primPlusWord
    (-)           = primMinusWord
    negate        = primNegateWord
    (*)           = primMulWord
    abs           = absReal
    signum        = signumReal
    fromInteger   = primIntegerToWord
    fromInt       = intToWord32

instance Bounded Word32 where
    minBound = 0
    maxBound = primMaxWord

instance Real Word32 where
    toRational x = toInteger x % 1

instance Integral Word32 where
    div       = primDivWord
    quot      = primQuotWord
    rem       = primRemWord
    mod       = primModWord
    quotRem   = primQrmWord
    divMod    = quotRem
    even      = primEvenWord
    toInteger = primWordToInteger
    toInt     = word32ToInt 

instance Ix Word32 where
    range (m,n)          = [m..n]
    index b@(m,n) i
	   | inRange b i = word32ToInt (i - m)
	   | otherwise   = error "index: Index out of range"
    inRange (m,n) i      = m <= i && i <= n

instance Enum Word32 where
    toEnum        = intToWord32
    fromEnum      = word32ToInt

    --No: suffers from overflow problems: 
    --   [4294967295 .. 1] :: [Word32]
    --   = [4294967295,0,1]
    --enumFrom c       = map toEnum [fromEnum c .. fromEnum (maxBound::Word32)]
    --enumFromThen c d = map toEnum [fromEnum c, fromEnum d .. fromEnum (last::Word32)]
    --     	           where last = if d < c then minBound else maxBound

    enumFrom       = numericEnumFrom
    enumFromTo     = numericEnumFromTo
    enumFromThen   = numericEnumFromThen
    enumFromThenTo = numericEnumFromThenTo

instance Read Word32 where
    readsPrec p = readDec

instance Show Word32 where
    showsPrec p = showInt  -- a particularily counterintuitive name!

instance Bits Word32 where
  (.&.)         = primAndWord
  (.|.)         = primOrWord
  xor           = primXorWord
  complement    = primComplementWord
  shift         = primShiftWord
  rotate        = primRotateWord 32
  bit           = primBitWord
  setBit x i    = x .|. bit i
  clearBit x i  = x .&. complement (bit i)
  complementBit x i = x `xor` bit i
  testBit       = primTestWord
  bitSize  _    = 32
  isSigned _    = False

-----------------------------------------------------------------------------
-- Word64
-----------------------------------------------------------------------------

data Word64 = W64 {lo,hi::Word32} deriving (Eq, Ord, Bounded)

w64ToInteger W64{lo=lo,hi=hi} = toInteger lo + 0x100000000 * toInteger hi 
integerToW64 x = case x `quotRem` 0x100000000 of 
                 (h,l) -> W64{lo=fromInteger l, hi=fromInteger h}

instance Show Word64 where
  showsPrec p = showInt . w64ToInteger

instance Read Word64 where
  readsPrec p s = [ (integerToW64 x,r) | (x,r) <- readDec s ]

-----------------------------------------------------------------------------
-- End of exported definitions
--
-- The remainder of this file consists of definitions which are only
-- used in the implementation.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Enumeration code: copied from Prelude
-----------------------------------------------------------------------------

numericEnumFrom        :: Real a => a -> [a]
numericEnumFromThen    :: Real a => a -> a -> [a]
numericEnumFromTo      :: Real a => a -> a -> [a]
numericEnumFromThenTo  :: Real a => a -> a -> a -> [a]
numericEnumFrom n            = n : (numericEnumFrom $! (n+1))
numericEnumFromThen n m      = iterate ((m-n)+) n
numericEnumFromTo n m        = takeWhile (<= m) (numericEnumFrom n)
numericEnumFromThenTo n n' m = takeWhile (if n' >= n then (<= m) else (>= m))
                                         (numericEnumFromThen n n')

-----------------------------------------------------------------------------
-- Coercions - used to make the instance declarations more uniform
-----------------------------------------------------------------------------

class Coerce a where
  to   :: Word32 -> a
  from :: a -> Word32

instance Coerce Word8 where
  from = word8ToWord32
  to   = word32ToWord8

instance Coerce Word16 where
  from = word16ToWord32
  to   = word32ToWord16

binop :: Coerce word => (Word32 -> Word32 -> a) -> (word -> word -> a)
binop op x y = from x `op` from y

to2 :: Coerce word => (Word32, Word32) -> (word, word)
to2 (x,y) = (to x, to y)

-----------------------------------------------------------------------------
-- primitives
-----------------------------------------------------------------------------

primitive primEqWord        :: Word32 -> Word32 -> Bool
primitive primCmpWord       :: Word32 -> Word32 -> Ordering
primitive primPlusWord,
	  primMinusWord,
	  primMulWord	    :: Word32 -> Word32 -> Word32
primitive primNegateWord    :: Word32 -> Word32
primitive primIntegerToWord :: Integer -> Word32
primitive primMaxWord       :: Word32
primitive primDivWord,
	  primQuotWord,
	  primRemWord,
	  primModWord       :: Word32 -> Word32 -> Word32
primitive primQrmWord       :: Word32 -> Word32 -> (Word32,Word32)
primitive primEvenWord      :: Word32 -> Bool
primitive primWordToInteger :: Word32 -> Integer
primitive primAndWord       :: Word32 -> Word32 -> Word32
primitive primOrWord        :: Word32 -> Word32 -> Word32
primitive primXorWord       :: Word32 -> Word32 -> Word32
primitive primComplementWord:: Word32 -> Word32
primitive primShiftWord     :: Word32 -> Int -> Word32
primitive primRotateWord    :: Int -> Word32 -> Int -> Word32
primitive primBitWord       :: Int -> Word32
primitive primTestWord      :: Word32 -> Int -> Bool

-----------------------------------------------------------------------------
-- Code copied from the Prelude
-----------------------------------------------------------------------------

absReal x    | x >= 0    = x
	     | otherwise = -x

signumReal x | x == 0    =  0
	     | x > 0     =  1
	     | otherwise = -1

-----------------------------------------------------------------------------
-- End
-----------------------------------------------------------------------------
