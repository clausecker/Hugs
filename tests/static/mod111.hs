-- !!! Hiding an abstract (Prelude) type
module M where

import Prelude hiding ( Char, ord )
import qualified Prelude ( ord )

type Char = Int

ord :: Char -> Int
ord x = Prelude.ord (chr x) + 1

