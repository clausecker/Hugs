/* --------------------------------------------------------------------------
 * Machine dependent code
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2002, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * ------------------------------------------------------------------------*/
#ifndef __MACHDEP_H__
#define __MACHDEP_H__
#include "prelude.h"

#ifdef HAVE_TIME_H
# include <time.h>
#endif

/* --------------------------------------------------------------------------
 * Find information about a file:
 * ------------------------------------------------------------------------*/

#if RISCOS
typedef struct { unsigned hi, lo; } Time;
#define timeChanged(now,thn)    (now.hi!=thn.hi || now.lo!=thn.lo)
#define timeSet(var,tm)         var.hi = tm.hi; var.lo = tm.lo
#else
typedef time_t Time;
#define timeChanged(now,thn)    (now!=thn)
#define timeSet(var,tm)         var = tm
#endif

extern Void getFileInfo Args((String, Time *, Long *));

/* --------------------------------------------------------------------------
 * Prototypes for registry reading
 * ------------------------------------------------------------------------*/

#if USE_REGISTRY

#define ProjectRoot ("SOFTWARE\\Haskell\\Projects\\")

#if HUGS_FOR_WINDOWS
extern Int    readRegInt     Args((String,Int));
extern Bool   writeRegInt    Args((String,Int));
#endif

extern String readRegString       Args((HKEY, String, String, String));
extern Bool   writeRegString      Args((String,String));
extern String readRegChildStrings Args((HKEY, String, String, String));
#endif /* USE_REGISTRY */

/* --------------------------------------------------------------------------
 * Search for script files on the HUGS path:
 * ------------------------------------------------------------------------*/

extern String RealPath  Args((String));
extern String substPath Args((String,String));

#ifndef HUGS_SERVER
extern Bool startEdit   Args((Int,String));
#endif

#endif /* __MACHDEP_H__ */
