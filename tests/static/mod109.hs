-- !!! Re-defining/shadowing a Prelude type.
module M where

data Maybe a = Nothing | Just a deriving ( Prelude.Eq )

x = Nothing

