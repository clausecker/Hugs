#include "HsFFI.h"
#include "types_aux.h"

extern HsInt       iInt       (HsInt       x) { return x; }
extern HsChar      iChar      (HsChar      x) { return x; }
extern HsAddr      iAddr      (HsAddr      x) { return x; }
extern HsPtr       iPtr       (HsPtr       x) { return x; }
extern HsFunPtr    iFunPtr    (HsFunPtr    x) { return x; }
extern HsFloat     iFloat     (HsFloat     x) { return x; }
extern HsDouble    iDouble    (HsDouble    x) { return x; }
extern HsStablePtr iStablePtr (HsStablePtr x) { return x; }
extern HsInt8      iInt8      (HsInt8      x) { return x; }
extern HsInt16     iInt16     (HsInt16     x) { return x; }
extern HsInt32     iInt32     (HsInt32     x) { return x; }
extern HsInt64     iInt64     (HsInt64     x) { return x; }
extern HsWord8     iWord8     (HsWord8     x) { return x; }
extern HsWord16    iWord16    (HsWord16    x) { return x; }
extern HsWord32    iWord32    (HsWord32    x) { return x; }
extern HsWord64    iWord64    (HsWord64    x) { return x; }

