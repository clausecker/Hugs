/* --------------------------------------------------------------------------
 * Basic data type definitions, prototypes and standard macros including
 * machine dependent variations...
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: prelude.h,v $
 * $Revision: 1.15 $
 * $Date: 2001/02/14 12:15:05 $
 * ------------------------------------------------------------------------*/

#include "config.h"
#include "options.h"
#include <stdio.h>

/*---------------------------------------------------------------------------
 * Most of the configuration code from earlier versions of Hugs has been moved
 * into config.h (which is usually automatically generated).  
 *
 * Most of the configuration code is "feature based".  That is, the 
 * configure script looks to see if a particular feature (or misfeature)
 * is present on the compiler/OS.  
 *
 * A small amount of configuration code is still "system based": it tests
 * flags to determine what kind of compiler/system it's running on - from
 * which it infers what features the compiler/system has.  Use of system
 * based tests generally indicates that we can't remember/figure out
 * what the original problem was and so we can't add an appropriate feature
 * test to the configure script.
 *-------------------------------------------------------------------------*/

#ifdef __RISCOS__ /* Acorn DesktopC running RISCOS2 or 3 */
# define RISCOS 1
#else
# define RISCOS 0
#endif

#if defined __DJGPP__ && __DJGPP__==2
# define DJGPP2 1
#else
# define DJGPP2 0
#endif

#if defined __MSDOS__ && __MSDOS__ && !DJGPP2
# define DOS 1
#else
# define DOS 0
#endif

#ifdef __SYMBIAN32__
#define IS_WIN32 0
#else
#if defined _WIN32 | defined __WIN32__
# define IS_WIN32 1
#else
# define IS_WIN32 0
#endif
#endif


/*---------------------------------------------------------------------------
 * Configuration options
 *
 * Most configuration options are arguments to the configure script
 * (try running "configure --help").  The following options are either
 * experimental or require changes to "Prelude.hs", the standard libraries
 * and demos and therefore cannot be modified using the configure script.
 * Most users should leave them alone!
 *
 *   OBSERVATIONS   to include support for `observe' and friends
 *   TREX	    to include support for Typed Rows and EXtensions.
 *   IPARAM	    to include support for Implicit Parameters.
 *   MUDO	    to include support for Recursive-do notation
 *   ZIP_COMP       to include support for Zip Comprehensions
 *   MULTI_INST	    to include support for Multi-Instance Resolution.
 *   HASKELL_ARRAYS to include support for Haskell array primitives.
 *   IO_MONAD	    to include the IO monad primitives and support.
 *   IO_HANDLES     to include the IO file operations.
 *   IO_REFS	    Ref type for IO_MONAD, and simple operations.
 *   FLUSHEVERY	    to force a fflush after every char in putStr/hPutStr.
 *   LAZY_ST	    to include support for lazy state threads.
 *   NPLUSK	    to include support for (n+k) patterns.
 *   BIGNUMS	    to include support for Integer bignums.
 *   FIXED_SUBST    to force a fixed size for the current substitution.
 *   DYN_TABLES	    to allocate tables dynamically, currently just a memory
 *		    saving trick, but this may be extended at a later stage
 *		    to allow at least some of the tables to be extended
 *		    dynamically at run-time to avoid exhausted space errors.
 *   GC_STABLEPTRS  to include support for safely passing Haskell
 *                  pointers over to C
 *                  (only required if you use callbacks in the foreign
 *                  language interface)
 *   GC_MALLOCPTRS  to include support for automatic deallocation of 
 *                  C objects when Haskell is done with them.
 *   GC_WEAKPTRS    to include support for weak pointers.
 *   STABLE_NAMES   stable names a la Simon PJ
 *   EVAL_INSTANCES to generate instances of the Eval class automatically.
 *   MONAD_COMPS    to allow monad comprehensions.
 *   REDIRECT_OUTPUT ability to redirect stdout/stderr to a buffer.
 *                  Only necessary for the Hugs server interface
 *                  (which is used in the Netscape plugin and the standalone
 *                  evaluator). 
 *   WORD_OPS       to include operations on unsigned ints
 *   ADDR_OPS       to include operations on addresses
 *   SHORT_CIRCUIT_COERCIONS to try to apply these rewrites at runtime:
 *                    integerToInt (intToInteger x) -> x
 *                    rationalToFloat  (fromDouble {dict} x) -> doubleToFloat x
 *                    rationalToDouble (fromDouble {dict} x) -> x
 *-------------------------------------------------------------------------*/

#if !HASKELL_98_ONLY
#define TREX		1
#define IPARAM		1
#define MUDO		1
#define MULTI_INST	1
#define OBSERVATIONS    1
#define ZIP_COMP	1
#else
#define TREX            0
#define IPARAM          0
#define MUDO		0
#define MULTI_INST      0
#define OBSERVATIONS    0
#define ZIP_COMP	0
#endif
#define HERE_DOC	0
#define HASKELL_ARRAYS	1
#define IO_MONAD	1
#define IO_HANDLES      1
#define IO_REFS		1 /* Experimental IO Ref type			   */
#define FLUSHEVERY	1
#define LAZY_ST		(IO_MONAD)
#define NPLUSK		1 /* Warning: There are those that would prefer 0  */
#define BIGNUMS		1 /* Experimental bignum implementation		   */
#define FIXED_SUBST	0 /* Warning: This may not be appropriate for PCs  */
#define DYN_TABLES	SMALL_HUGS /* For dynamically allocated tables	   */
#define GC_STABLEPTRS   1 /* May be required by external libraries         */
#define GC_MALLOCPTRS   1 /* May be required by external libraries         */
#define GC_WEAKPTRS     1
#define STABLE_NAMES    1
#define EVAL_INSTANCES  0
#define MONAD_COMPS	0
#define REDIRECT_OUTPUT (!HUGS_FOR_WINDOWS)
#define WORD_OPS        1
#define ADDR_OPS        1

#define SHORT_CIRCUIT_COERCIONS 1

/*---------------------------------------------------------------------------
 * Platform-dependent settings:
 *-------------------------------------------------------------------------*/

#ifdef HAVE_MACSYSTEM	/* Macintosh system() prototype. */
int macsystem(char *filenames);
#endif


/*---------------------------------------------------------------------------
 * Include windows.h and friends:
 *-------------------------------------------------------------------------*/

#if     HUGS_FOR_WINDOWS
#include <windows.h>			/* Misc. Windows hackery	   */

#if	__MSDOS__
# define INT           int
# define UNSIGNED      unsigned
# define CHAR	       char
# define TCHAR         char
# define UCHAR	       UNSIGNED CHAR
# define ULONG	       unsigned long
# define APIENTRY      PASCAL
# define HUGE          huge
# define LPOFNHOOKPROC FARPROC
# define CMDdata(w,l)  (HIWORD(l))	/* decoding WM_COMMAND message	   */
# define CMDitem(w,l)  (w)
# define CMDhwnd(w,l)  ((HWND)(LOWORD(l)))
#else
# define HUGE
# define CMDdata(w,l)  (HIWORD(w))	/* decoding WM_COMMAND message	   */
# define CMDitem(w,l)  (LOWORD(w))
# define CMDhwnd(w,l)  ((HWND)(l))
#endif

#if HUGS_FOR_WINDOWS
#include "winhugs\winmenu.h"
#endif
extern char *appName;
extern HWND		hWndText;	/* text output window handle	   */
extern HWND		hWndMain;	/* main window handle		   */
#if HUGS_FOR_WINDOWS
#include "winhugs\wintext.h"
#endif
#endif


/*---------------------------------------------------------------------------
 * Macros used in declarations:
 *  function prototypes
 *  local/far declarations
 *  HUGS_noreturn/HUGS_unused (prevent spurious warnings)
 *  result type of main
 *  dynamic linking declarations
 *-------------------------------------------------------------------------*/

#if HAVE_PROTOTYPES       /* To enable use of prototypes whenever possible */
#define Args(x) x
#else
#define Args(x) ()
#endif

/* local = prefix for locally defined functions */
/* far   = prefix for far pointers              */
#if DOS
# define local near pascal
#else
# define local
# define far
#endif

#ifdef __GNUC__     /* Avoid spurious warnings                             */
#if __GNUC__ >= 2 && __GNUC_MINOR__ >= 7
#define HUGS_noreturn  __attribute__ ((noreturn))
#define HUGS_unused    __attribute__ ((unused))
#else
#define HUGS_noreturn  
#define HUGS_unused
#endif
#else
#define HUGS_noreturn  
#define HUGS_unused
#endif

/* result type of main function */
/* Hugs 1.01 could be configured to return void on Unix-like systems
 * but I don't think this is necessary.  ADR
 */
#define Main int
#define MainDone() return 0/*NOTUSED*/

/*---------------------------------------------------------------------------
 * Dynamic linking tricks
 *-------------------------------------------------------------------------*/

#if 0
/* DLLs, shareable libraries, etc generated by the foreign language
 * interface generator need some way to access the Hugs stack, standard
 * constructor functions, the garbage collector, etc.
 *
 * Most UNIX systems use the same mechanisms as for static linking - when
 * you load the shareable object file, it patches it with the values of
 * the required symbols.
 *
 * DOS/Windows uses a different mechanism - a DLL (or EXE) accesses code and
 * data from other DLLs (or EXEs) via an indirection.  No big deal for code
 * but it makes a huge difference when accessing data and the compiler
 * _HAS TO KNOW_ whether a piece of data is accessed directly (it's in
 * the same DLL/EXE) or indirectly.
 *
 * On Microsoft Visual C++, this is done using a VC++ specific language
 * extension on declarations and definitions of all imported/exported
 * symbols.  The "extern" declarations of imported symbols are modified
 * with "__declspec(dllimport)" and the definitions of exported symbols
 * are marked with "__declspec(dllexport)".  If you want both the 
 * declaration and the definition to coexist in the same file without 
 * generating warning messages, you have to go through contortions.
 *
 * Sigh, to add to the confusion, MS C and Borland C disagree about whether
 * to put the export declaration before or after the return type - so we
 * have to parameterise it to allow both.
 */
#endif

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

#ifdef __EXTERNAL
#define HUGSAPI(rty) DLLIMPORT(rty)
#else
#define HUGSAPI(rty) DLLEXPORT(rty)
#endif

/*---------------------------------------------------------------------------
 * String operations:
 *-------------------------------------------------------------------------*/

#if HAVE_STRING_H
# include <string.h>
#else
extern int      strcmp     Args((const char*, const char*));
extern int      strncmp    Args((const char*, const char*, int));
extern char     *strchr    Args((const char*, int));
extern char     *strrchr   Args((const char*, int));
extern size_t   strlen     Args((const char *));
extern char	*strcpy	   Args((char *, const char*));
extern char     *strcat	   Args((char *, const char*));
#endif
#if HAVE_STRCMP
#if HUGS_FOR_WINDOWS
#define strCompare stricmp
#else
#define strCompare strcmp
#endif
#else /* probably only used for DOS - ADR */
extern  int     stricmp	   Args((const char *, const char*));
#define strCompare stricmp
#endif

#if HAVE_CTYPE_H
# include <ctype.h>
#endif
#ifndef isascii
#define  isascii(c)	(((unsigned)(c))<128)
#endif

/*---------------------------------------------------------------------------
 * Printf-related operations:
 *-------------------------------------------------------------------------*/

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#if !defined(HAVE_SNPRINTF)
# if __MWERKS__ && macintosh
extern int snprintf  Args((char*, unsigned long, const char*, va_list));
# else
extern int snprintf   Args((char*, int, const char*, ...));
# endif
#endif

#if !defined(HAVE_VSNPRINTF)
# if __MWERKS__ && macintosh
extern int vsnprintf  Args((char*, unsigned long, const char*, va_list));
# else
extern int vsnprintf  Args((char*, int, const char*, va_list));
# endif
#endif

/*---------------------------------------------------------------------------
 * Pipe-related operations:
 *
 * On Windows, many standard Unix names acquire a leading underscore.
 * Irritating, but easy to work around.
 *-------------------------------------------------------------------------*/

#if !defined(HAVE_POPEN) && defined(HAVE__POPEN)
#define popen(x,y) _popen(x,y)
#endif
#if !defined(HAVE_PCLOSE) && defined(HAVE__PCLOSE)
#define pclose(x) _pclose(x)
#endif

/*---------------------------------------------------------------------------
 * Interrupting execution (signals, allowBreak):
 *-------------------------------------------------------------------------*/

#if !DOS && VOID_INT_SIGNALS
# define sigProto(nm)	void nm Args((int))
# define sigRaise(nm)	nm(1)
# define sigHandler(nm)	void nm(sig_arg) int sig_arg;
# define sigResume	return
#else
# define sigProto(nm)	int nm Args((Void))
# define sigRaise(nm)	nm()
# define sigHandler(nm)	int nm(Void)
# define sigResume	return 1
#endif

/* allowBreak: call to allow user to interrupt computation
 * ctrlbrk:    set control break handler
 */

/* On Unix (and almost every other system), the interrupt handlers perform
 * a longjmp to break out of the current computation.
 * On Win32 this does not work because the interrupt handler is run in
 * a separate thread from the main computation.  Instead we set a 
 * flag (the global variable "broken") to request an interrupt and
 * all potentially infinite loops of the evaluator check the flag using
 * the "allowBreak" call.
 */ 
#define HANDLERS_CANT_LONGJMP IS_WIN32


#if DOS

# if !HUGS_FOR_WINDOWS
extern  int  kbhit	Args((void));
# endif /* !HUGS_FOR_WINDOWS */
# define allowBreak()	kbhit()

#elif HANDLERS_CANT_LONGJMP /* eg Win32 */

# if HUGS_FOR_WINDOWS
#  if USE_THREADS
#   define ctrlbrk(bh)	do { signal(SIGINT,bh); signal(SIGBREAK,bh); } while (0)
#   define allowBreak()	if (broken) { broken = FALSE; sigRaise(breakHandler); }
#  else
#   define ctrlbrk(bh)  do { signal(SIGINT,bh); signal(SIGBREAK,bh); } while (0)
#   define allowBreak()	kbhit(); if (broken) { broken = FALSE; sigRaise(breakHandler); }
#  endif /* USE_THREADS */
# else /* !HUGS_FOR_WINDOWS */
#  define ctrlbrk(bh)	do { signal(SIGINT,bh); signal(SIGBREAK,bh); } while (0)
#  define allowBreak()	if (broken) { broken = FALSE; sigRaise(breakHandler); }
# endif /* !HUGS_FOR_WINDOWS */

#else /* !DOS && !HANDLERS_CANT_LONGJMP - eg Unix */

# if HAVE_SIGPROCMASK
#  include <signal.h>
#  define ctrlbrk(bh)	{ sigset_t mask; \
			  signal(SIGINT,bh); \
			  sigemptyset(&mask); \
			  sigaddset(&mask, SIGINT); \
			  sigprocmask(SIG_UNBLOCK, &mask, NULL); \
			}
# else
#  define ctrlbrk(bh)	signal(SIGINT,bh)
# endif
#if __MWERKS__ && macintosh
# define allowBreak()   doNothing()
#else
# define allowBreak()   doNothing()
#endif

#endif /* !DOS && !HANDLERS_CANT_LONGJMP */


#ifndef SIGBREAK /* Sigh, not defined in cygwin32 beta release 16 */
# define SIGBREAK 21
#endif

/*---------------------------------------------------------------------------
 * Floating point support
 *-------------------------------------------------------------------------*/

/* Can we fit floats into ints? */
#if USE_DOUBLE_PRECISION
#define BREAK_FLOATS (SIZEOF_DOUBLE > SIZEOF_INT)
#else
#define BREAK_FLOATS (SIZEOF_FLOAT  > SIZEOF_INT)
#endif

#ifdef  HAVE_LIBM

#if USE_DOUBLE_PRECISION
#define FloatImpType	   double
#define FloatPro	   double
#define FloatFMT           "%.9g"
#else
#define FloatImpType	   float
#define FloatPro	   double  /* type to use in prototypes		   */
				   /* strictly ansi (i.e. gcc) conforming  */
				   /* but breaks data hiding :-(	   */
#define FloatFMT	   "%g"
#endif

#if HAVE_FLOAT_H

#include <float.h>

#if USE_DOUBLE_PRECISION
# define HUGS_RADIX    FLT_RADIX
# define HUGS_MANT_DIG DBL_MANT_DIG
# define HUGS_MIN_EXP  DBL_MIN_EXP
# define HUGS_MAX_EXP  DBL_MAX_EXP
#else
# define HUGS_RADIX    FLT_RADIX
# define HUGS_MANT_DIG FLT_MANT_DIG
# define HUGS_MIN_EXP  FLT_MIN_EXP
# define HUGS_MAX_EXP  FLT_MAX_EXP
#endif

#elif HAVE_VALUES_H

#include <values.h>

#if USE_DOUBLE_PRECISION
# define HUGS_RADIX    _EXPBASE
# define HUGS_MANT_DIG DSIGNIF 
# define HUGS_MIN_EXP  DMINEXP 
# define HUGS_MAX_EXP  DMAXEXP 
#else
# define HUGS_RADIX    _EXPBASE
# define HUGS_MANT_DIG FSIGNIF
# define HUGS_MIN_EXP  FMINEXP
# define HUGS_MAX_EXP  FMAXEXP 
#endif

#endif

#ifdef __SYMBIAN32__
/* Guesswork, really */
#define HUGS_RADIX          2
#define HUGS_MANT_DIG      53
#define HUGS_MIN_EXP    -1021
#define HUGS_MAX_EXP     1024
#endif

#else /* !HAVE_LIBM */

#define FloatImpType	   int     /*dummy*/
#define FloatPro	   int
#define FloatFMT	   "%d"

#endif /* !HAVE_LIBM */

/*---------------------------------------------------------------------------
 * Memory allocation
 *-------------------------------------------------------------------------*/

#if HAVE_FARCALLOC
# include <alloc.h>
# define farCalloc(n,s)	farcalloc((unsigned long)n,(unsigned long)s)
#elif HAVE_VALLOC
# include <stdlib.h>
#ifndef __SYMBIAN32__
# include <malloc.h>
#endif
# define farCalloc(n,s)	(Void *)valloc(((unsigned)n)*((unsigned)s))
#else
# define farCalloc(n,s)	(Void *)calloc(((unsigned)n),((unsigned)s))
#endif

/* bison-generated parsers like to have alloca - so try to define it */
#if HAVE__ALLOCA
#ifndef __SYMBIAN32__
#include <malloc.h>
#endif
#ifndef alloca
#define alloca _alloca
#endif
#endif

/*---------------------------------------------------------------------------
 * Assertions
 *-------------------------------------------------------------------------*/

#if HAVE_ASSERT_H
#include <assert.h>
#else
#define assert(x) doNothing()
#endif

/*---------------------------------------------------------------------------
 * Environment variables and the registry
 *-------------------------------------------------------------------------*/

/* On Win32 we can use the registry to supplement info in environment 
 * variables.
 */
#define USE_REGISTRY (HAVE_WINDOWS_H && !__MSDOS__)

/*---------------------------------------------------------------------------
 * File operations:
 *-------------------------------------------------------------------------*/

#if HAVE_UNISTD_H
# ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
# endif
# include <unistd.h>
#elif !HUGS_FOR_WINDOWS
extern int 	chdir 	   Args((const char*));
#endif

#if HAVE_STDLIB_H
# include <stdlib.h>
#else
extern int      system	   Args((const char *));
extern double   atof	   Args((const char *));
extern void     exit       Args((int));
#endif

#ifndef FILENAME_MAX	   /* should already be defined in an ANSI compiler*/
#define FILENAME_MAX 256
#else
#if     FILENAME_MAX < 256
#undef  FILENAME_MAX
#define FILENAME_MAX 256
#endif
#endif

/* Hack, hack: if you have dos.h, you probably have a DOS filesystem */
#ifndef __SYMBIAN32__
/* No dos.h but a DOS filesystem */
#define DOS_FILENAMES              HAVE_DOS_H
#else
#define DOS_FILENAMES 1
#endif
/* ToDo: can we replace this with a feature test? */
#define MAC_FILENAMES              macintosh

#define CASE_INSENSITIVE_FILENAMES (DOS_FILENAMES | RISCOS)

#if CASE_INSENSITIVE_FILENAMES
# if HAVE_STRCASECMP
#  define filenamecmp(s1,s2) strcasecmp(s1,s2)
# elif HAVE__STRICMP
#  define filenamecmp(s1,s2) _stricmp(s1,s2)
# elif HAVE_STRICMP
#  define filenamecmp(s1,s2) stricmp(s1,s2)
# elif HAVE_STRCMPI
#  define filenamecmp(s1,s2) strcmpi(s1,s2)
# endif
#else
# define filenamecmp(s1,s2) strcmp(s1,s2)
#endif

/*---------------------------------------------------------------------------
 * Optimisations:
 *-------------------------------------------------------------------------*/

#ifdef  __GNUC__			/* look for GCC 2.x extensions	   */
#if     __GNUC__ >= 2 && !defined(NeXT)	/* NeXT cc lies and says it's 2.x  */

/* WARNING: if you use the following optimisations to assign registers for
 * particular global variables, you should be very careful to make sure that
 * storage(RESET) is called after a longjump (usually resulting from an error
 * condition) and before you try to access the heap.  The current version of
 * main deals with this using everybody(RESET) at the head of the main read,
 * eval, print loop
 */

#ifdef  m68k				/* global registers on an m68k	   */
#define GLOBALfst	asm("a4")
#define GLOBALsnd	asm("a5")
#define GLOBALsp	asm("a3")
#endif

#ifdef  sparc				/* global registers on a sparc	   */
/* sadly, although the gcc documentation suggests that the following reg   */
/* assignments should be ok, experience shows (at least on Suns) that they */
/* are not -- it seems that atof() and friends spoil things.		   */
/*#define GLOBALfst	asm("g5")*/
/*#define GLOBALsnd	asm("g6")*/
/*#define GLOBALsp	asm("g7")*/
#endif /* sparc */

#endif
#endif /* defined(__GNUC__) */

/*---------------------------------------------------------------------------
 * General settings:
 *-------------------------------------------------------------------------*/

#define Void     void   /* older compilers object to: typedef void Void;   */
#if !defined(_XLIB_H_)  /* clashes with similar declaration in Xlib.h      */
typedef unsigned Bool;
#endif
#define TRUE     1
#define FALSE    0
#ifndef _XtIntrinsic_h
typedef char    *String;
#endif
typedef int      Int;
typedef long     Long;
typedef int      Char;
typedef unsigned Unsigned;
typedef void*    Pointer;

#define doNothing() do { } while (0) /* Null statement */

#ifndef STD_PRELUDE
#if     RISCOS
#define STD_PRELUDE	   "prelude"
#else
#define STD_PRELUDE	   "Prelude.hs"
#endif
#endif

#if IO_MONAD
#define NUM_HANDLES	   40
#endif
#define NUM_TUPLES         100
#define NUM_OFFSETS        1024
#define NUM_CHARS          256
#if TREX
#define NUM_EXT		   100
#endif

#define CHAR_MASK          0xff

#if PROFILING
#define DEF_PROFINTDIV	   10		/* hpsize/this cells between samples*/
#endif

#if     SMALL_HUGS			/* the McDonalds mentality :-)	   */
#define Pick(s,r,l)	   s
#endif
#if     REGULAR_HUGS
#define Pick(s,r,l)	   r
#endif
#if     LARGE_HUGS
#define Pick(s,r,l)	   l
#endif

#if OBSERVATIONS
#define NUM_OBS_TAGS       Pick(100,    200,        1000)
#define NUM_BRKPTS         Pick(100,    200,        200)
#endif

#define NUM_SCRIPTS        Pick(64,     100,        500)
#define NUM_FIXUPS         Pick(400,    400,        1000)
#define NUM_TYCON          Pick(60,     160,        800)
#define NUM_NAME           Pick(1000,   2000,       16000)
#define NUM_CLASSES        Pick(30,     240,        480)
#define NUM_INSTS          Pick(200,    300,        1000)
#define NUM_TEXT           Pick(12000,  20000,      160000)
#define NUM_TEXTH	   Pick(1,      10,         10)
#define NUM_TYVARS         Pick(800,    2000,       4000)
#define NUM_STACK          Pick(1800,   12000,      16000)
#define NUM_ADDRS          Pick(28000,  60000,      320000)
#define MINIMUMHEAP	   Pick(7500,   19000,      19000)
#define MAXIMUMHEAP	   Pick(32765,  0,          0)
#define DEFAULTHEAP        Pick(28000,  50000,      250000)
#define MAXPOSINT          Pick(0x7fff, 0x7fffffff, 0x7fffffff)
#define MAXHUGSWORD        Pick(0xffffU, 0xffffffffU, 0xffffffffU)
#define NUM_STABLEPTRS	   Pick(10,     100,        10000)
#define NUM_MALLOCPTRS	   Pick(10,     100,        10000)
#define NUM_DTUPLES	   Pick(3,      5,          5)
#define BIGBASE		   Pick(100,    10000,      10000)
#define BIGEXP		   Pick(2,      4,          4)

#define MINNEGINT          (-MAXPOSINT-1)

#define NUM_MODULE         NUM_SCRIPTS


#if DYN_TABLES				/* Tables may be alloc'd at runtime*/
#define DECTABLE(tab)	   far *tab	/* macros for declaration & defn   */
#define DEFTABLE(tab,sz)   far *tab = 0
#else					/* or at compile-time:		   */
#define DECTABLE(tab)	   tab[]
#define DEFTABLE(tab,sz)   tab[sz]
#endif

#define minRecovery	   Pick(1000,  1000,       1000)
#define bitsPerWord	   Pick(16,    32,         32)
#define wordShift	   Pick(4,     5,          5)
#define wordMask	   Pick(15,    31,         31)

#define bitArraySize(n)    ((n)/bitsPerWord + 1)
#define placeInSet(n)      ((-(n)-1)>>wordShift)
#define maskInSet(n)       (1<<((-(n)-1)&wordMask))

/*---------------------------------------------------------------------------
 * Compiler output
 * Tweaking this lets us redirect prompts, error messages, etc - but has no
 * effect on output of Haskell programs (which should use hPutStr and friends).
 *-------------------------------------------------------------------------*/

#if REDIRECT_OUTPUT

extern Void   hugsPrintf            Args((const char *, ...));
extern Void   hugsPutchar           Args((int));
extern Void   hugsFlushStdout       Args((Void));
extern Void   hugsEnableOutput      Args((Bool));
extern String hugsClearOutputBuffer Args((Void));
			    
extern Void   hugsFFlush    	    Args((FILE*));
extern Void   hugsFPrintf   	    Args((FILE*, const char*, ...));
extern Void   hugsPutc      	    Args((int, FILE*));

#define Printf         	     hugsPrintf
#define Putchar        	     hugsPutchar
#define FlushStdout    	     hugsFlushStdout
#define EnableOutput   	     hugsEnableOutput
#define ClearOutputBuffer    hugsClearOutputBuffer

#define FFlush               hugsFFlush
#define FPrintf              hugsFPrintf
#define Putc                 hugsPutc
			     
#else			     
			     
#define Printf               printf
#define Putchar              putchar
#define FlushStdout()        fflush(stdout)
#define EnableOutput(f)      doNothing()
#define ClearOutputBuffer()  0

#define FFlush               fflush
#define FPrintf              fprintf
#define Putc                 putc

#endif

/*-------------------------------------------------------------------------*/
