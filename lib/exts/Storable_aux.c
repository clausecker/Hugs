#include "HsFFI.h"

#define FOO(T) \
void wr##T(Hs##T *arg1, HsInt arg2, Hs##T arg3) { arg1[arg2] = arg3; } \
Hs##T rd##T(Hs##T *arg1, HsInt arg2) { return arg1[arg2]; } \
HsInt sz##T(void) { return sizeof(Hs##T); } 


FOO(Int       )
FOO(Char      )
/* FOO(WideChar  ) */
FOO(Word      )
FOO(Ptr       )
FOO(FunPtr    )
FOO(Float     )
FOO(Double    )
FOO(StablePtr )
FOO(Int8      )
FOO(Int16     )
FOO(Int32     )
FOO(Int64     )
FOO(Word8     )
FOO(Word16    )
FOO(Word32    )
FOO(Word64    )



