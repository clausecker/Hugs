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

class Bits a where
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

shiftL, shiftR   :: Bits a => a -> Int -> a
rotateL, rotateR :: Bits a => a -> Int -> a
shiftL  a i = shift  a i
shiftR  a i = shift  a (-i)
rotateL a i = rotate a i
rotateR a i = rotate a (-i)

-----------------------------------------------------------------------------
