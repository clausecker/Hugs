-----------------------------------------------------------------------------
-- A simple "dynamic typing" library
-----------------------------------------------------------------------------
module HugsDynamic
   ( Typeable(typeOf)
   , Dynamic, toDynamic, fromDynamic, dynApply  -- the primitives
   , fromDyn, dynApp                            -- raise errors instead of Maybes
   , intToDyn, fromDynInt, strToDyn, fromDynStr -- specialised versions
   , Tycon(..), Type(..)			       -- added by sof
   ) where

-- Added nicer printers for better error messages  -- jcp

import IOExts(unsafePerformIO)

data Tycon = Tycon String     deriving Eq

instance Show Tycon where
  showsPrec p (Tycon s) = showString s
 
data Type  = App Tycon [Type] deriving Eq

instance Show Type where
  showsPrec p (App tycon tys) 
    | tycon == listTC && onearg 
    = showString "[" . shows arg1 . showString "]"
    | tycon == funTC && twoarg 
    = showParen (p > 8) $
      showsPrec 9 arg1 . showString " -> " . showsPrec 8 arg2
    | tycon == tup2TC && twoarg 
    = showString "(" . showsPrec 0 arg1 . showString ", " . showsPrec 0 arg2 .
      showString ")"
    | zeroarg
    = showsPrec p tycon 
    | otherwise
    = showParen (p > 9) $
      showsPrec p tycon . showArgs tys
   where
    (arg1 : arg2 : _) = tys
    l = length tys
    zeroarg = l == 0
    onearg = l == 1
    twoarg = l == 2  
    showArgs [] = id
    showArgs (a:as) = showsPrec 10 a . showString " " . showArgs as 

unitTC    = Tycon "()"
intTC     = Tycon "Int"
integerTC = Tycon "Integer"
floatTC   = Tycon "Float"
doubleTC  = Tycon "Double"
charTC    = Tycon "Char"
ioTC      = Tycon "IO"
funTC     = Tycon "->"
listTC    = Tycon "[]"
tup2TC    = Tycon "(,)"

-- ToDo: Either might be more useful for reporting errors
tyApp :: Type -> Type -> Maybe Type
tyApp (App tc [t1,t2]) t3
  | tc == funTC
  = if t1 == t3 then Just t2 else Nothing
tyApp _ _ = Nothing

---------------------------------------------------------------

class Typeable a where
  typeOf :: a -> Type

instance Typeable ()      where typeOf x = App unitTC    []
instance Typeable Int     where typeOf x = App intTC     []
instance Typeable Integer where typeOf x = App integerTC []
instance Typeable Float   where typeOf x = App floatTC   []
instance Typeable Double  where typeOf x = App doubleTC  []
instance Typeable Char    where typeOf x = App charTC    []

instance Typeable a => Typeable (IO a) where 
  typeOf m = 
    case unsafePerformIO m of { r ->
    App ioTC  [typeOf r]
    }

instance (Typeable a, Typeable b) => Typeable (a -> b) where
  typeOf f = 
    -- We use case to bind arg and result to avoid excess polymorphism
    case undefined of { arg ->
    case f arg     of { result ->
    App funTC [typeOf arg, typeOf result]
    }}

instance Typeable a => Typeable [a] where
  typeOf xs = App listTC [typeOf (head xs)]

instance (Typeable a, Typeable b) => Typeable (a,b) where
  typeOf p = App tup2TC [typeOf (fst p), typeOf (snd p)]

----------------------------------------------------------------

data Object  = Object -- dummy type - we're going to switch the typechecker off

data Dynamic = Dynamic Type Object
instance Show Dynamic where
  showsPrec _ (Dynamic ty _) = showString "<<" . showsPrec 0 ty . showString ">>"

toDynamic :: Typeable a => a -> Dynamic
toDynamic x = Dynamic (typeOf x) (unsafeCoerce x)

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic (Dynamic ty x) =
  -- We use case to bind r to avoid excess polymorphism
  case unsafeCoerce x of { r -> 
  if ty == typeOf r then Just r else Nothing
  }

dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dynamic t1 f) (Dynamic t2 x) =
  tyApp t1 t2   >>= \ t3 -> 
  return (Dynamic t3 ((unsafeCoerce f) x))

----------------------------------------------------------------

fromDyn :: Typeable a => Dynamic -> a
fromDyn d = res
   where res = case fromDynamic d of
                Just r -> r
                Nothing -> error ("fromDyn failed.  Expecting " ++
                                  show expectedType ++
                                  " found " ++ show d) 
         expectedType = toDynamic res

intToDyn :: Int -> Dynamic
intToDyn = toDynamic

strToDyn :: String -> Dynamic
strToDyn = toDynamic

fromDynInt :: Dynamic -> Int
fromDynInt = fromDyn

fromDynStr :: Dynamic -> String
fromDynStr = fromDyn

runDyn :: Dynamic -> IO ()
runDyn = fromDyn

dynApp :: Dynamic -> Dynamic -> Dynamic
dynApp f x = case dynApply f x of 
             Just r -> r
             Nothing -> error ("Type error in dynamic application.\n" ++
                               "Can't apply function " ++ show f ++
                               " to argument " ++ show x)


----------------------------------------------------------------

test1 = toDynamic (1::Int)
test2 = toDynamic ((+) :: Int -> Int -> Int)
test3 = dynApp test2 test1
test4 = dynApp test3 test1
test5 = fromDyn test4 

test5,test6,test7 :: Int
test6 = fromDyn test1
test7 = fromDyn test2

----------------------------------------------------------------

primitive unsafeCoerce "primUnsafeCoerce" :: a -> b

