-- !!! ambiguous re-exportation. (shouldn't fail)
module M where 

import Prelude as M
id x = x
