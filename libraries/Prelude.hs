{----------------------------------------------------------------------------
__   __ __  __  ____   ___    _______________________________________________
||   || ||  || ||  || ||__    Hugs 98: The Nottingham and Yale Haskell system
||___|| ||__|| ||__||  __||   Copyright (c) 1994-1999
||---||         ___||         World Wide Web: http://haskell.org/hugs
||   ||                       Report bugs to: hugs-bugs@haskell.org
||   || Version: February 1999_______________________________________________

 This is the Hugs 98 Standard Prelude, based very closely on the Standard
 Prelude for Haskell 98.

 WARNING: This file is an integral part of the Hugs source code.  Changes to
 the definitions in this file without corresponding modifications in other
 parts of the program may cause the interpreter to fail unexpectedly.  Under
 normal circumstances, you should not attempt to modify this file in any way!

-----------------------------------------------------------------------------
 The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 1994-2002, All rights reserved.  It is distributed as free software under
 the license in the file "License", which is included in the distribution.
----------------------------------------------------------------------------}

module Prelude (
--  module PreludeList,
    map, (++),filter,  concat, concatMap, 
    head, last, tail, init, null, length, (!!),
    foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
    iterate, repeat, replicate, cycle,
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    lines, words, unlines, unwords, reverse, and, or,
    any, all, elem, notElem, lookup,
    sum, product, maximum, minimum,
    zip, zip3, zipWith, zipWith3, unzip, unzip3,
--  module PreludeText, 
    ReadS, ShowS,
    Read(readsPrec, readList),
    Show(showsPrec, show, showList),
    reads, shows, read, lex,
    showChar, showString, readParen, showParen,
--  module PreludeIO,
    FilePath, IOError, ioError, userError, catch,
    putChar, putStr, putStrLn, print,
    getChar, getLine, getContents, interact,
    readFile, writeFile, appendFile, readIO, readLn,

    Bool(False, True),
    Maybe(Nothing, Just),
    Either(Left, Right),
    Ordering(LT, EQ, GT),
    Char, String, Int, Integer, Float, Double, Rational, IO,
--  List type: []((:), [])
    (:),
--  Tuple types: (,), (,,), etc.
--  Trivial type: ()
--  Functions: (->)

    Eq((==), (/=)),
    Ord(compare, (<), (<=), (>=), (>), max, min),
    Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen,
         enumFromTo, enumFromThenTo),
    Bounded(minBound, maxBound),
    Num((+), (-), (*), negate, abs, signum, fromInteger, {-NONSTANDARD:-}fromInt),
    Real(toRational),
    Integral(quot, rem, div, mod, quotRem, divMod, even, odd, toInteger, {-NONSTANDARD:-}toInt),
    Fractional((/), recip, fromRational, {-NONSTANDARD-}fromDouble),
    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    RealFrac(properFraction, truncate, round, ceiling, floor),
    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),
    Monad((>>=), (>>), return, fail),
    Functor(fmap),
    mapM, mapM_, sequence, sequence_, (=<<),
    maybe, either,
    (&&), (||), not, otherwise,
    subtract, even, odd, gcd, lcm, (^), (^^), 
    fromIntegral, realToFrac,
    fst, snd, curry, uncurry, id, const, (.), flip, ($), until,
    asTypeOf, error, undefined,
    seq, ($!),
    {-NONSTANDARD-}Ix
  ) where

import Hugs.Prelude hiding 
   (Ix(..), 
    Ratio,(%),numerator,denominator,approxRational,
    showSigned,showInt,readSigned,readInt,
    readDec,readOct,readHex,readSigned,
    readFloat,lexDigits,
    isAscii, isControl, isPrint, isSpace, isUpper, isLower,
    isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum,
    digitToInt, intToDigit,
    toUpper, toLower,
    ord,chr,
    readLitChar, showLitChar, lexLitChar,
    IO(..), IOResult(..), primExitWith, 
    FunPtr, Ptr, Addr,
    Word, StablePtr, ForeignObj, ForeignPtr,
    Int8, Int16, Int32, Int64,
    Word8, Word16, Word32, Word64,
    basicIORun, blockIO, IOFinished(..),
    threadToIOResult,
    HugsException, catchHugsException, primThrowException
    )
