-----------------------------------------------------------------------------
-- Bit twiddling operations
--
-- This library defines bitwise operations for signed and unsigned ints.
--
-- Suitable for use with Hugs 98.
-----------------------------------------------------------------------------

module Bits where

infixl 8 `shift`, `rotate`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.

class Num a => Bits a where
  (.&.), (.|.), xor :: a -> a -> a
  complement        :: a -> a
  shift             :: a -> Int -> a
  rotate            :: a -> Int -> a
  bit               :: Int -> a        
  setBit            :: a -> Int -> a   
  clearBit          :: a -> Int -> a   
  complementBit     :: a -> Int -> a   
  testBit           :: a -> Int -> Bool
  bitSize           :: a -> Int
  isSigned          :: a -> Bool

  bit i               = 1 `shift` i
  x `setBit` i        = x .|. bit i
  x `clearBit` i      = x .&. complement (bit i)
  x `complementBit` i = x `xor` bit i
  x `testBit` i       = (x .&. bit i) /= 0

shiftL, shiftR   :: Bits a => a -> Int -> a
rotateL, rotateR :: Bits a => a -> Int -> a
shiftL  a i = shift  a i
shiftR  a i = shift  a (-i)
rotateL a i = rotate a i
rotateR a i = rotate a (-i)

-----------------------------------------------------------------------------
