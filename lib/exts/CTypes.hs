module CTypes( module CTypes ) where

type CChar = Char
type CSize = Int

castCCharToChar :: CChar -> Char
castCharToCChar :: Char -> CChar

castCCharToChar = id
castCharToCChar = id

