-- !!! Imported tycon clashes with local definition
module M where
import Prelude(Int,Char)
type Int = Char