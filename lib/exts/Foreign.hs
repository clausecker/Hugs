module Foreign (
        module Bits,
        module Int,
        module Word,
        module Ptr,
        module ForeignObj, -- deprecated
        module ForeignPtr,
        module StablePtr,
        module Storable,
        module MarshalAlloc,
        module MarshalArray,
        module MarshalError,
        module MarshalUtils,
        unsafePerformIO,
	) where

import Bits
import Int
import Word
import Ptr
import ForeignObj  -- deprecated
import ForeignPtr
import StablePtr
import Storable
import MarshalAlloc
import MarshalArray
import MarshalError
import MarshalUtils

import IOExts(unsafePerformIO)
