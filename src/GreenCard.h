/* --------------------------------------------------------------------------
 * GreenCard include file.
 *
 * Hugs 98 is Copyright (c) Mark P Jones, Alastair Reid and the Yale
 * Haskell Group 1994-99, and is distributed as Open Source software
 * under the Artistic License; see the file "Artistic" that is included
 * in the distribution for details.
 *
 * $RCSfile: GreenCard.h,v $
 * $Revision: 1.1 $
 * $Date: 1999/06/07 23:53:37 $
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 *
 *                                  WARNING
 *
 * Most of the code in this file must exactly match corresponding definitions
 * in the Hugs source code.
 *
 * We have chosen to copy this code over to avoid the need to #include huge
 * chunks of the Hugs internal definitions (which sometimes conflict with
 * Xlib, Win32 or other libraries which we might also have to #include).
 *
 * ------------------------------------------------------------------------*/

/* Configuration details */
#include "config.h"
#include "options.h"

#if HAVE_PROTOTYPES       /* To enable use of prototypes whenever possible */
#define Args(x) x
#else
#define Args(x) ()
#endif

/* based on code in builtin.c */

typedef int   HugsStackPtr;
typedef int   HugsStablePtr;
typedef void* HugsForeign;

#define PROTO_PRIM(name) static void name Args((HugsStackPtr))
#define primFun(name)	 static void name(HugsStackPtr hugs_root)
#define hugs_returnIO(n) hugs->returnIO(hugs_root,n)
#define hugs_returnId(n) hugs->returnId(hugs_root,n)

/* These declarations must exactly match those in storage.h */

typedef void (*Prim) Args((HugsStackPtr)); /* primitive function	   */

extern struct primitive {		/* table of primitives		   */
    char*  ref;				/* primitive reference string	   */
    int	   arity;			/* primitive function arity	   */
    Prim   imp;				/* primitive implementation	   */
} primitives[];

struct primInfo {
    void              (*controlFun) Args((int));
    struct primitive  *primFuns;
    struct primInfo   *nextPrimInfo;
};

/* This is an exact copy of the declaration found in storage.h */

typedef struct {

  /* evaluate next argument */
  int            (*getInt   )     Args(());  
  unsigned int   (*getWord)       Args(());
  void*     	 (*getAddr  )     Args(());
  float     	 (*getFloat )     Args(());
  double    	 (*getDouble)     Args(());
  char      	 (*getChar  )     Args(());
  HugsForeign    (*getForeign)    Args(());
  HugsStablePtr  (*getStablePtr)  Args(());

  /* push part of result   */
  void      	 (*putInt   )     Args((int));           
  void      	 (*putWord  )     Args((unsigned int));
  void      	 (*putAddr  )     Args((void *));
  void      	 (*putFloat )     Args((double));
  void      	 (*putDouble)     Args((double));
  void      	 (*putChar  )     Args((char));
  void      	 (*putForeign)    Args((HugsForeign, void (*)(HugsForeign)));
  void      	 (*putStablePtr)  Args((HugsStablePtr));

  /* return n values in IO monad or Id monad */
  void      	 (*returnIO)      Args((HugsStackPtr, int));
  void      	 (*returnId)      Args((HugsStackPtr, int));
  int      	 (*runIO)         Args((int));

  /* free a stable pointer */	    			 
  void      	 (*freeStablePtr) Args((HugsStablePtr));

  /* register the prim table */	    			 
  void      	 (*registerPrims) Args((struct primInfo*));
			   
  /* garbage collect */
  void		 (*garbageCollect) Args(());

} HugsAPI2;

HugsAPI2 *hugs; /* pointer to virtual function table */


/* Copied verbatim from prelude.h */

#ifdef _MSC_VER /* Microsoft Visual C++ */
#define DLLIMPORT(rty) __declspec(dllimport) rty
#define DLLEXPORT(rty) __declspec(dllexport) rty
#elif defined __BORLANDC__ 
#define DLLIMPORT(rty) rty far _import
#define DLLEXPORT(rty) rty far _export
#else 
#define DLLIMPORT(rty) rty
#define DLLEXPORT(rty) rty
#endif /* Don't need to declare DLL exports */

