/* --------------------------------------------------------------------------
 * Implementation of the Haskell IO monad.
 *
 * The primitives below implement the standard IO monad for Haskell 1.3
 * using a continuation passing bimonad (two streams of processing for
 * `exceptional' and `normal' I/O respectively).  The primitives are
 * believed to give a reasonably good implementation of the semantics
 * specified by the Haskell 1.3 report.  There are also some additional
 * primitives, particularly for dealing with IOError and Handle values
 * that are not included in the prelude, but have been suggested for
 * inclusion in standard libraries.  I don't know what semantics and
 * specifications will be specified for these operations in the final
 * version of the 1.3 I/O specification, so be prepared for changes.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2002, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: iomonad.c,v $
 * $Revision: 1.33 $
 * $Date: 2002/09/21 00:15:18 $
 * ------------------------------------------------------------------------*/
 
Name nameIORun;			        /* run IO code                     */
Name nameIOBind;		        /* bind IO code                    */
Name namePutStr;		        /* Prelude.putStr                  */

static Name namePass;			/* auxiliary:: \f b c a -> f a b c */
#if IO_HANDLES
static Name nameHreader;	        /* auxiliary function		   */
#endif

#if IO_HANDLES
static String local toIOErrorDescr Args((int,Bool));
static Name   local toIOError      Args((int));
static Cell   local mkIOError      Args((Name,String,String,Cell));
static Cell   local openHandle     Args((StackPtr,Cell,Int,Bool,String));
#endif

#if IO_HANDLES
# if WANT_FIXED_SIZE_TABLES
#  define MAX_HANDLES NUM_HANDLES
# else
#  define MAX_HANDLES num_handles
# endif
#endif

static Void local pushString       Args((String));

/* --------------------------------------------------------------------------
 * IO monad control:
 * ------------------------------------------------------------------------*/

static Void iomonadControl Args((Int));
static Void iomonadControl(what)
Int what; {
    switch (what) {
	case INSTALL : 
		       setCurrModule(modulePrelude);
#define pFun(n,s,t)    addPrim(0,n=newName(findText(s),NIL),t,modulePrelude,NIL)
		       pFun(namePass,	 "_pass",    "passIO");
#if    IO_HANDLES
		       pFun(nameHreader, "_hreader", "hreader");
#endif
#undef pFun
#define predef(nm,str) nm=newName(findText(str),NIL); name(nm).defn=PREDEFINED
		       predef(nameIORun,    "hugsIORun");
		       predef(nameIOBind,   "primbindIO");
		       predef(namePutStr,   "putStr");
#undef predef
		       break;

	case RESET   : 
		       break;
    }
}

PROTO_PRIM(primLunit);
PROTO_PRIM(primRunit);
PROTO_PRIM(primLbind);
PROTO_PRIM(primRbind);
PROTO_PRIM(primPass);

PROTO_PRIM(primGC);
PROTO_PRIM(primGetEnv);
PROTO_PRIM(primSystem);
PROTO_PRIM(primGetRandomSeed);

PROTO_PRIM(primArgc);
PROTO_PRIM(primArgv);

PROTO_PRIM(primGetCh);
PROTO_PRIM(primGetChar);
PROTO_PRIM(primPutChar);
PROTO_PRIM(primPutStr);

#if IO_HANDLES
static Void local fwritePrim  Args((StackPtr,Bool,Bool,String));
static Void local fopenPrim   Args((StackPtr,Bool,String));

PROTO_PRIM(primHGetChar);
PROTO_PRIM(primHPutChar);
PROTO_PRIM(primHPutStr);
PROTO_PRIM(primHreader);
PROTO_PRIM(primHContents);
PROTO_PRIM(primContents);
PROTO_PRIM(primOpenFile);
PROTO_PRIM(primOpenBinaryFile);
PROTO_PRIM(primStdin);
PROTO_PRIM(primStdout);
PROTO_PRIM(primStderr);
PROTO_PRIM(primHIsEOF);
PROTO_PRIM(primHugsHIsEOF);
PROTO_PRIM(primHFlush);
PROTO_PRIM(primHClose);
PROTO_PRIM(primHGetPosn);
PROTO_PRIM(primHSetPosn);
PROTO_PRIM(primHSetBuffering);
PROTO_PRIM(primHGetBuffering);
PROTO_PRIM(primHSeek);
PROTO_PRIM(primHLookAhead);
PROTO_PRIM(primHIsOpen);
PROTO_PRIM(primHIsClosed);
PROTO_PRIM(primHIsReadable);
PROTO_PRIM(primHIsWritable);
PROTO_PRIM(primHIsSeekable);
PROTO_PRIM(primHFileSize);
PROTO_PRIM(primHWaitForInput);
PROTO_PRIM(primEqHandle);
PROTO_PRIM(primReadFile);
PROTO_PRIM(primWriteFile);
PROTO_PRIM(primAppendFile);

PROTO_PRIM(primReadBinaryFile);
PROTO_PRIM(primWriteBinaryFile);
PROTO_PRIM(primAppendBinaryFile);

PROTO_PRIM(primIsIllegal);
PROTO_PRIM(primIsEOFError);
PROTO_PRIM(primIsAlreadyExist);
PROTO_PRIM(primIsAlreadyInUse);
PROTO_PRIM(primIsDoesNotExist);
PROTO_PRIM(primIsFull);
PROTO_PRIM(primIsUserErr);
PROTO_PRIM(primIsPermDenied);

PROTO_PRIM(primGetErrorString);
PROTO_PRIM(primGetHandle);
PROTO_PRIM(primGetFileName);
#endif

#if IO_REFS
PROTO_PRIM(primNewRef);
PROTO_PRIM(primDerefRef);
PROTO_PRIM(primAssignRef);
PROTO_PRIM(primEqRef);
#endif

PROTO_PRIM(primMakeSP);
PROTO_PRIM(primDerefSP);
PROTO_PRIM(primFreeSP);
PROTO_PRIM(primCastSPToP);
PROTO_PRIM(primCastPToSP);

PROTO_PRIM(primNewFP);
PROTO_PRIM(primWriteFP);
PROTO_PRIM(primEqFP);
PROTO_PRIM(primTouchFP);
PROTO_PRIM(primFPToP);

#if GC_WEAKPTRS
PROTO_PRIM(primMakeWeakPtr);
PROTO_PRIM(primDerefWeakPtr);
PROTO_PRIM(primWeakPtrEq);
PROTO_PRIM(primMkWeak);
PROTO_PRIM(primDeRefWeak);
PROTO_PRIM(primReplaceFinalizer);
PROTO_PRIM(primFinalize);
PROTO_PRIM(primRunFinalizer);
PROTO_PRIM(primFinalizerWaiting);
#endif

#if STABLE_NAMES
PROTO_PRIM(primMakeSN);
PROTO_PRIM(primDerefSN);
PROTO_PRIM(primHashSN);
PROTO_PRIM(primEqSN);
#endif

#ifdef HSCRIPT
PROTO_PRIM(primGetCurrentScript);
#endif

static struct primitive iomonadPrimTable[] = {
  {"lunitIO",		3, primLunit},
  {"runitIO",		3, primRunit},
  {"lbindIO",		4, primLbind},
  {"rbindIO",		4, primRbind},
  {"passIO",		4, primPass},

  {"primGC",	        2, primGC},
  {"getEnv",	        3, primGetEnv},
  {"primSystem",	3, primSystem},
  {"getRandomSeed",	2, primGetRandomSeed},

  {"primArgc",	        2, primArgc},
  {"primArgv",	        3, primArgv},

  {"getCh",		2, primGetCh},
  {"getChar",		2, primGetChar},
  {"putChar",		3, primPutChar},
  {"putStr",		3, primPutStr},

  {"isUserError",	1, primIsUserErr},

#if IO_HANDLES
  {"hGetChar",		3, primHGetChar},
  {"hPutChar",		4, primHPutChar},
  {"hPutStr",		4, primHPutStr},
  {"hreader",		1, primHreader},
  {"hGetContents",	3, primHContents},
  {"getContents",	2, primContents},
  {"openFile",          4, primOpenFile},
  {"openBinaryFile",    4, primOpenBinaryFile},
  {"stdin",		0, primStdin},
  {"stdout",		0, primStdout},
  {"stderr",		0, primStderr},
  {"hIsEOF",		3, primHIsEOF},
  {"hugsHIsEOF",	3, primHugsHIsEOF},
  {"hFlush",		3, primHFlush},
  {"hClose",		3, primHClose},
  {"hGetPosnPrim",	3, primHGetPosn},
  {"hSetPosnPrim",	4, primHSetPosn},
  {"hSetBuff",          5, primHSetBuffering},
  {"hGetBuff",          3, primHGetBuffering},
  {"hSeekPrim",         5, primHSeek},
  {"hLookAhead",        3, primHLookAhead},
  {"hIsOpen",		3, primHIsOpen},
  {"hIsClosed",		3, primHIsClosed},
  {"hIsReadable",	3, primHIsReadable},
  {"hIsWritable",	3, primHIsWritable},
  {"hIsSeekable",       3, primHIsSeekable},
  {"hFileSize",         3, primHFileSize},
  {"hWaitForInput",     4, primHWaitForInput},
  {"primEqHandle",	2, primEqHandle},
  {"readFile",		3, primReadFile},
  {"writeFile",		4, primWriteFile},
  {"appendFile",	4, primAppendFile},
  {"readBinaryFile",	3, primReadBinaryFile},
  {"writeBinaryFile",	4, primWriteBinaryFile},
  {"appendBinaryFile",	4, primAppendBinaryFile},
  {"isAlreadyExistsError", 1, primIsAlreadyExist},
  {"isDoesNotExistError",  1, primIsDoesNotExist},
  {"isAlreadyInUseError",  1, primIsAlreadyInUse},
  {"isFullError",	   1, primIsFull},
  {"isEOFError",	   1, primIsEOFError},
  {"isIllegalOperation",   1, primIsIllegal},
  {"isPermissionError",	   1, primIsPermDenied},
  {"ioeGetErrorString",	   1, primGetErrorString},
  {"ioeGetHandle",	   1, primGetHandle},
  {"ioeGetFileName",	   1, primGetFileName},
#endif

#if IO_REFS
  {"newRef",            3, primNewRef},
  {"getRef",		3, primDerefRef},
  {"setRef",		4, primAssignRef},
  {"eqRef",		2, primEqRef},
#endif

  {"makeStablePtr",	3, primMakeSP},
  {"deRefStablePtr",	3, primDerefSP},
  {"freeStablePtr",	3, primFreeSP},
  {"castStablePtrToPtr",1, primCastSPToP},
  {"castPtrToStablePtr",1, primCastPToSP},

  {"makeForeignObj",	4, primNewFP},
  {"writeForeignObj",	4, primWriteFP},
  {"eqForeignObj",	2, primEqFP},

  {"newForeignPtr",	4, primNewFP},
  {"eqForeignPtr",	2, primEqFP},
  {"touchForeignPtr",	3, primTouchFP},
  {"foreignPtrToPtr",	1, primFPToP},

#if GC_WEAKPTRS
  {"makeWeakPtr",       3, primMakeWeakPtr},
  {"derefWeakPtr",      3, primDerefWeakPtr},
  {"weakPtrEq",		2, primWeakPtrEq},
  {"mkWeak",		5, primMkWeak},
  {"deRefWeak",		3, primDeRefWeak},
  {"replaceFinalizer",	4, primReplaceFinalizer},
  {"finalize",		3, primFinalize},
  {"runFinalizer",	2, primRunFinalizer},
  {"finalizerWaiting",	2, primFinalizerWaiting},
#endif

#if STABLE_NAMES
  {"makeStableName",	3, primMakeSN},
  {"deRefStableName",	1, primDerefSN},
  {"hashStableName",	1, primHashSN},
  {"eqStableName",	2, primEqSN},
#endif
  
#ifdef HSCRIPT
  {"getCurrentScript",  2, primGetCurrentScript},
#endif

  {0,			0, 0}
};

static struct primInfo iomonadPrims = { iomonadControl, iomonadPrimTable, 0 };

/* --------------------------------------------------------------------------
 * Macros
 *
 * Note: the IOReturn and IOFail macros do not use the standard "do while"
 * trick to create a single statement because some C compilers (eg sun)
 * report warning messages "end-of-loop code not reached".
 * This may lead to syntax errors if used where a statement is required - such
 * errors can be fixed by adding braces round the call.  Blech!
 * ------------------------------------------------------------------------*/

#define IOArg(n)    primArg((n)+2)
#define IOReturn(r) { updapRoot(primArg(1),r); return; }
#define IOFail(r)   { updapRoot(primArg(2),r); return; }

/* --------------------------------------------------------------------------
 * The monad combinators:
 * ------------------------------------------------------------------------*/

primFun(primLunit) {			/* bimonad left unit		   */
    updapRoot(primArg(2),primArg(3));	/* lunit 3 2 1 = 2 3		   */
}

primFun(primRunit) {			/* bimonad right unit		   */
    updapRoot(primArg(1),primArg(3));	/* lunit 3 2 1 = 1 3		   */
}

primFun(primLbind) {			/* bimonad left bind		   */
    push(ap(namePass,primArg(3)));	/* lbind 4 3 2 1 = 4 (pass 3 2 1) 1*/
    toparg(primArg(2));
    toparg(primArg(1));
    updapRoot(ap(primArg(4),top()),primArg(1));
}

primFun(primRbind) {			/* bimonad right bind		   */
    push(ap(namePass,primArg(3)));	/* rbind 4 3 2 1 = 4 2 (pass 3 2 1)*/
    toparg(primArg(2));
    toparg(primArg(1));
    updapRoot(ap(primArg(4),primArg(2)),top());
}

primFun(primPass) {			/* Auxiliary function		   */
    push(ap(primArg(4),primArg(1)));	/* pass 4 3 2 1 = 4 1 3 2	   */
    toparg(primArg(3));
    updapRoot(top(),primArg(2));
}

/* --------------------------------------------------------------------------
 * Handle operations:
 * ------------------------------------------------------------------------*/

#if IO_HANDLES

static
Cell local openHandle(root,sCell,hmode,binary,loc) /* open handle to file named s in  */
StackPtr root;
Cell   sCell;                                      /* the specified hmode             */
Int    hmode; 
Bool   binary;
String loc; {
    Int i;
    String s = evalName(sCell);

    /* openHandle() returns a *pair*, the first component contains NIL if
       opening the handle failed (and the second component contains the IOError
       describing the error). If the handled was created successfully,
       the second component contains the Handle.
    */

    if (!s) {				/* check for valid name		   */
      return(pair(NIL,
		  mkIOError(nameIllegal,
			    loc,
			    "illegal file name",
			    nameNothing)));
    }

    for (i=0; i<MAX_HANDLES && nonNull(handles[i].hcell); ++i)
	;                                       /* Search for unused handle*/
    if (i>=MAX_HANDLES) {                       /* If at first we don't    */
	garbageCollect();                       /* succeed, garbage collect*/
	for (i=0; i<MAX_HANDLES && nonNull(handles[i].hcell); ++i)
	    ;                                   /* and try again ...       */
    }
    
#if !WANT_FIXED_SIZE_TABLES
    if (i >= MAX_HANDLES) {
      int j;
      growDynTable(dynTabHandles);
      handles=(struct strHandle*)(dynTabHandles->data);
      num_handles = dynTabHandles->maxIdx;
      /* Nil out the new entries in the table */
      for (j=dynTabHandles->idx; j < num_handles; j++) {
	handles[j].hcell = NIL;
      }
    }
#endif

    if (i>=MAX_HANDLES) {                       /* ... before we give up   */
      return(pair(NIL,
		  mkIOError(nameIllegal,
			    loc,
			    "too many handles open",
			    sCell)));
    } else {                                   /* prepare to open file    */
	String stmode;
	if (binary) {
	    stmode = (hmode&HAPPEND)    ? "ab+" :
		     (hmode&HWRITE)     ? "wb+" :
		     (hmode&HREADWRITE) ? "wb+" :
		     (hmode&HREAD)      ? "rb" : (String)0;
	} else {
	    stmode = (hmode&HAPPEND)     ? "a+"  :
		     (hmode&HWRITE)      ? "w+"  :
		     (hmode&HREADWRITE)  ? "w+"  :
		     (hmode&HREAD)       ? "r"  : (String)0;
	}
	if (stmode && (handles[i].hfp=fopen(s,stmode))) {
	    handles[i].hmode = hmode;
	    handles[i].hbufMode = HUNKNOWN_BUFFERING;
	    handles[i].hbufSize = (-1);
	    if (hmode&HREADWRITE) {
	      handles[i].hHaveRead = FALSE;
	    }
	    return (pair(nameNothing,handles[i].hcell = ap(HANDCELL,i)));
	}
	return (pair(NIL,
		     mkIOError(toIOError(errno),
			       loc,
			       toIOErrorDescr(errno,TRUE),
			       sCell)));
    }
}

/* --------------------------------------------------------------------------
 * Building strings:
 * ------------------------------------------------------------------------*/

static Void local pushString(s)       /* push pointer to string onto stack */
String s; {
    Int  l      = strlen(s);
    push(nameNil);
    while (--l >= 0) {
	topfun(consChar(s[l]));
    }
}

/* Helper function for constructing IOErrors (see Prelude defn of
 * IOError for explanation of what the the individual arguments
 * do.
 */
  
static
Cell local
mkIOError(kind, loc, desc, mbF)
Name   kind;
String loc;
String desc;
Cell   mbF;
{
   Cell locStr;
   pushString(loc);
   locStr = pop();

   pushString(desc);

   return (ap(ap(ap(ap(nameIOError,
   			  kind),
		       locStr),
		    pop()),
       	     ((mbF == nameNothing) ? mbF : ap(nameJust,mbF))));
}


#endif
/* --------------------------------------------------------------------------
 * IO Errors (more defined for file ops)
 * ------------------------------------------------------------------------*/


/*
 * Map a libc error code to an IOError
 */
static Name local toIOError(errc)
int errc;
{
#if defined(HAVE_ERRNO_H)  && !(__MWERKS__ && macintosh)
  switch(errc) {

  case EEXIST:
    return nameAlreadyExists;
  case ENOENT:
  case ENOTDIR:
    return nameDoesNotExist;
  case EPERM:
  case EACCES:
    return namePermDenied;
  case ENOSPC:
  case EFBIG:
    return nameIsFull;
  default:
    return nameIllegal;
  }
#else
  return nameIllegal;
#endif
}

/*
 * Map a libc error code to an IOError descriptive string
 */
static String local toIOErrorDescr(errc,isFile)
int   errc;
Bool  isFile;
{
#if defined(HAVE_ERRNO_H)  && !(__MWERKS__ && macintosh)
  switch(errc) {

  case EEXIST:
    return (isFile ? "file already exists" : "directory already exists");
  case ENOENT:
  case ENOTDIR:
    return (isFile ? "file does not exist" : "directory does not exist");
  case EPERM:
  case EACCES:
    return ""; /* No need to replicate the info conveyed by the IOErrorKind */
  case ENOSPC:
  case EFBIG:
    return "device is full";
  default:
    return "";
  }
#else
  return "";
#endif
}

primFun(primIsUserErr) {		/* :: IOError -> Bool        	   */
    eval(primArg(1));
    eval(primArg(5));
    checkCon();
    BoolResult(whnfHead==nameUserErr);
}

primFun(primIsDoesNotExist) {		/* :: IOError -> Bool		   */
    eval(primArg(1));   /* unwinds the IOError dcon, pushing the args onto the stack */
    eval(primArg(5));   /* select the error 'kind' and evaluate it */
    checkCon();
    BoolResult(whnfHead==nameDoesNotExist);
}

primFun(primIsAlreadyExist) {		/* :: IOError -> Bool		   */
    eval(primArg(1));
    eval(primArg(5));
    checkCon();
    BoolResult(whnfHead== nameAlreadyExists);
}

primFun(primIsAlreadyInUse) {		/* :: IOError -> Bool		   */
    eval(primArg(1));
    eval(primArg(5));
    checkCon();
    BoolResult(whnfHead== nameAlreadyInUse);
}

primFun(primIsFull) {		/* :: IOError -> Bool		   */
    eval(primArg(1));
    eval(primArg(5));
    checkCon();
    BoolResult(whnfHead== nameIsFull);
}

primFun(primIsPermDenied) {		/* :: IOError -> Bool		   */
    eval(primArg(1));
    eval(primArg(5));
    checkCon();
    BoolResult(whnfHead==namePermDenied);
}

primFun(primIsIllegal) {		/* :: IOError -> Bool	   */
    eval(primArg(1));
    eval(primArg(5));
    checkCon();
    BoolResult(whnfHead==nameIllegal);
}

primFun(primIsEOFError) {		/* :: IOError -> Bool	   */
    eval(primArg(1));
    eval(primArg(5));
    checkCon();
    BoolResult(whnfHead==nameEOFErr);
}

/* --------------------------------------------------------------------------
 * Misc.
 * ------------------------------------------------------------------------*/

primFun(primGC) {			/* force a GC right now            */
    garbageCollect();
    IOReturn(nameUnit);
}

#if BIGNUMS && defined HAVE_TIME_H
#include <time.h>

primFun(primGetRandomSeed) {		/* generate a random seed          */
    IOReturn(bigInt(clock()));
}

#else

primFun(primGetRandomSeed) {		/* generate a random seed          */
    ERRMSG(0) "getRandomSeed is not implemented on this architecture"
    EEND;
}

#endif
				     
primFun(primGetEnv) {                 /* primGetEnv :: String -> IO String */
    String s = evalName(IOArg(1));    /* Eval name	                   */
    String r;
    if (!s) {			      /* check for valid name		   */
	IOFail(mkIOError(nameIllegal,
		         "System.getEnv",
			 "illegal environment variable name",
			 IOArg(1)));
    } else if ((r = getenv(s))!=0) {    
	pushString(r);
	IOReturn(pop());
    } else {
	IOFail(mkIOError(nameDoesNotExist,
		         "System.getEnv",
			 "environment variable not found",
			 IOArg(1)));
    }
}

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif
#ifndef WEXITSTATUS
# define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif
					 
primFun(primSystem) {                   /* primSystem :: String -> IO Int  */
    String s = evalName(IOArg(1));	/* Eval name	                   */
    Int r;
    if (s) {				/* check for valid string          */
	r = shellEsc(s, TRUE/*synchronous*/, TRUE/*use shell*/);
	IOReturn(mkInt(WEXITSTATUS(r)));
    } else {
	IOFail(mkIOError(nameIllegal,
		         "System.system",
			 "illegal system command string",
			 IOArg(1)));
    }
}
    
static String defaultArgv[] = {
  "Hugs"  /* program name */
};

static String* hugsArgv = defaultArgv;
static Int     hugsArgc = sizeof defaultArgv / sizeof defaultArgv[0];

Void setHugsArgs(argc,argv)
Int    argc;
String argv[]; {
    hugsArgc = argc;
    hugsArgv = argv;
}

primFun(primArgc) {                     /* primArgc :: IO Int              */
    IOReturn(mkInt(hugsArgc));
}
    
primFun(primArgv) {                     /* primArgv :: Int -> IO String    */
    Int i;
    IntArg(i,3);
    if (0 <= i && i < hugsArgc) {
	pushString(hugsArgv[i]);
	IOReturn(pop());
    } else {
	IOFail(mkIOError(nameIllegal,
		         "System.getArgs",
			 "illegal argument",
			 nameNothing));
    }
}
    
/* --------------------------------------------------------------------------
 * Console IO
 * ------------------------------------------------------------------------*/

primFun(primGetCh) {			/* Get character from stdin wo/echo*/
    Int c = readTerminalChar();
    if (c!=EOF) {
      IOReturn(mkChar(c));
    } else {
      IOFail(mkIOError(nameEOFErr,
		       "IOExtensions.getCh",
		       "end of file",
		       nameNothing));
    }
}

#if __MWERKS__ && macintosh
primFun(primGetChar) {			 /* Metrowerks console has no NO_ECHO mode. */
    Int c = readTerminalChar();
    if (c!=EOF) {
      IOReturn(mkChar(c));
    } else {
      IOFail(mkIOError(nameEOFErr,
		       "Prelude.getChar",
		       "end of file",
		       nameNothing));
    }
}
#else
primFun(primGetChar) {			/* Get character from stdin w/ echo*/
    Int c = readTerminalChar();
    if (c != EOF) {
      putchar(c);
      fflush(stdout);
    }
    if (c!=EOF) {
      IOReturn(mkChar(c));
    } else {
      IOFail(mkIOError(nameEOFErr,
		       "Prelude.getChar",
		       "end of file",
		       nameNothing));
    }
}
#endif

primFun(primPutChar) {			/* print character on stdout	   */
    eval(pop());
    putchar(charOf(whnfHead));
    fflush(stdout);
    IOReturn(nameUnit);
}

primFun(primPutStr) {			/* print string on stdout	   */
    blackHoleRoot();            	/* supposedly = hPutStr stdout,	   */
    eval(pop());			/* included here for speed	   */
    while (whnfHead==nameCons) {
	eval(top());
	checkChar();
	putchar(charOf(whnfHead));
#if FLUSHEVERY
	fflush(stdout);
#endif
	drop();
	eval(pop());
    }
#if !FLUSHEVERY
    fflush(stdout);
#endif
    IOReturn(nameUnit);
}

/* --------------------------------------------------------------------------
 * File IO
 * ------------------------------------------------------------------------*/

#if IO_HANDLES

#define HandleArg(nm,offset)  \
    eval(primArg(offset));    \
    nm = intValOf(whnfHead)

#define IOBoolResult(e)  \
    IOReturn((e)?nameTrue:nameFalse)

primFun(primHGetChar) {			/* Read character from handle	   */
    Int h;
    HandleArg(h,3);
    
    /* Flush output buffer for R/W handles */
    if (handles[h].hmode&HREADWRITE && !handles[h].hHaveRead) {
	fflush(handles[h].hfp);
	handles[h].hHaveRead = TRUE;
    }

    if (handles[h].hmode&(HREAD|HREADWRITE)) {
	Int c = (h==HSTDIN ? readTerminalChar() : getc(handles[h].hfp));
	if (c!=EOF) {
	    IOReturn(mkChar(c));
	} else if ( feof(handles[h].hfp) ) {
	  IOFail(mkIOError(nameEOFErr,
			   "IO.hGetChar",
			   "end of file",
			   nameNothing));
	}
    }
    IOFail(mkIOError(toIOError(errno),
		     "IO.hGetChar",
		     toIOErrorDescr(errno,TRUE),
		     nameNothing));
}

primFun(primHPutChar) {			/* print character on handle	   */
    Char c = 0;
    Int  h;
    HandleArg(h,4);
    CharArg(c,3);

    /* Flush input buffer for R/W handles */
    if (handles[h].hmode&HREADWRITE && handles[h].hHaveRead) {
	fflush(handles[h].hfp);
	handles[h].hHaveRead = FALSE;
    }
    if (handles[h].hmode&(HWRITE|HAPPEND|HREADWRITE)) {
	if ( putc(c,handles[h].hfp) == EOF ) {
	  IOFail(mkIOError(toIOError(errno),
			   "IO.hPutChar",
			   toIOErrorDescr(errno,TRUE),
			   nameNothing));
	}
	IOReturn(nameUnit);
    }
    IOFail(mkIOError(nameIllegal,
		     "IO.hPutChar",
		     "handle is not writable",
		     nameNothing));
}

primFun(primHPutStr) {			/* print string on handle	   */
    Int h;
    HandleArg(h,4);
    push(primArg(3));
    primArg(3) = NIL;

    /* Make sure the input buffer is flushed for R/W handles */
    if (handles[h].hmode&HREADWRITE && handles[h].hHaveRead) {
	fflush(handles[h].hfp);
	handles[h].hHaveRead = FALSE;
    }
    if (handles[h].hmode&(HWRITE|HAPPEND|HREADWRITE)) {
	blackHoleRoot();
	eval(pop());
	while (whnfHead==nameCons) {
	    eval(pop());
	    putc(charOf(whnfHead),handles[h].hfp);
#if FLUSHEVERY
	    if ( h <= 2 ) {  /* Only flush the standard handles */
	      fflush(handles[h].hfp);
	    }
#endif
	    eval(pop());
	}
#if !FLUSHEVERY
	if (h <= 2) { /* Only flush the standard handles */
	  fflush(handles[h].hfp);
	}
#endif
	IOReturn(nameUnit);
    }
    IOFail(mkIOError(nameIllegal,
		     "IO.hPutStr",
		     "handle is not writable",
		     nameNothing));
}

primFun(primHreader) {			/* read String from a handle 	   */
    Int h;                              /* Handle -> String                */
    HandleArg(h,1);
    if (handles[h].hmode&HSEMICLOSED) {	/* read requires semi-closed handle*/
	Int c = (h==HSTDIN ? readTerminalChar() : getc(handles[h].hfp));
	if (c!=EOF && c>=0 && c<NUM_CHARS) {
	    updapRoot(consChar(c),ap(nameHreader,primArg(1)));
	    return;
	}
	clearerr(handles[h].hfp);
    }
    updateRoot(nameNil);
}

primFun(primHContents) {		/* hGetContents :: Handle -> IO Str*/
    Int h;
    HandleArg(h,3);
    if ((handles[h].hmode&(HREAD|HREADWRITE))==0) { /* must have readable handle	   */
        IOFail(mkIOError(nameIllegal,
		         "IO.hGetContents",
		         "handle is not readable",
		         nameNothing));
    } else {				/* semi-close handle		   */
	handles[h].hmode = HSEMICLOSED;
	if (handles[h].hmode&HREADWRITE && !handles[h].hHaveRead) {
	  fflush(handles[h].hfp);
	  handles[h].hHaveRead = TRUE;
	}
	IOReturn(ap(nameHreader,IOArg(1)));
    }
}

primFun(primContents) {			/* Get contents of stdin	   */
    if ((handles[HSTDIN].hmode&HREAD)==0) {
        IOFail(mkIOError(nameIllegal,
		         "Prelude.getContents",
		         "handle is not readable",
		         nameNothing));
    } else {
	handles[HSTDIN].hmode = HSEMICLOSED;
	IOReturn(ap(nameHreader,handles[HSTDIN].hcell));
    }
}

static Void local fopenPrim(root,binary,loc)/* Auxiliary function for          */
StackPtr root;                              /* opening a file                  */
Bool     binary;
String   loc; {
    Int    m = HCLOSED;

    eval(IOArg(1));			/* Eval IOMode			   */
    if (isName(whnfHead) && isCfun(whnfHead))
	switch (cfunOf(whnfHead)) {	/* we have to use numeric consts   */
	    case 1 : m = HREAD;		/* here to avoid the need to put   */
		     break;		/* IOMode in startup environment   */
	    case 2 : m = HWRITE;
		     break;
	    case 3 : m = HAPPEND;
		     break;
	    case 4 : m = HREADWRITE;
		     break;
	}

    if (m!=HCLOSED) {			/* Only accept legal modes	   */
	Cell hnd = openHandle(root,IOArg(2),m,binary,loc);
	if (!isNull(fst(hnd))) {
	   IOReturn(snd(hnd));
        } else {
	  IOFail(snd(hnd));
	}
    }
    
   IOFail(mkIOError(nameIllegal,
		    loc,
		    "unknown handle mode",
		    IOArg(2)));
}

primFun(primOpenFile) {			/* open handle to a text file	   */
    fopenPrim(root,FALSE,"IO.openFile");
}

primFun(primOpenBinaryFile) {		/* open handle to a binary file	   */
    fopenPrim(root,TRUE,"IOExtensions.openBinaryFile");
}

primFun(primStdin) {			/* Standard input handle	   */
    push(handles[HSTDIN].hcell);
}

primFun(primStdout) {			/* Standard output handle	   */
    push(handles[HSTDOUT].hcell);
}

primFun(primStderr) {			/* Standard error handle	   */
    push(handles[HSTDERR].hcell);
}

/* NOTE: this doesn't implement the Haskell 1.3 semantics */
primFun(primHugsHIsEOF) {		/* Test for end of file on handle  */
    Int h;
    HandleArg(h,3);
    if (handles[h].hmode!=HCLOSED) {
        IOBoolResult(feof(handles[h].hfp));
    } else {
        IOFail(mkIOError(nameIllegal,
		         "IO.hugsIsEOF",
		         "handle is closed",
			 nameNothing));
    }
}

primFun(primHIsEOF) {	/* Test for end of file on handle  */
                        /* :: Handle -> IO Bool */
    Int h;
    FILE* fp;
    HandleArg(h,3);
    if (handles[h].hmode&(HREAD|HREADWRITE)) {
      Bool isEOF;
      fp = handles[h].hfp;
      isEOF = feof(fp);
      if (isEOF) { /* If the EOF flag is already signalled,
		      peeking at the next char isn't likely
		      to produce a different outcome! */
	IOBoolResult(isEOF);
      } else {
	Int c = fgetc(fp);
	isEOF = feof(fp);

	/* Put the char back and clear any flags. */
	ungetc(c,fp);
	clearerr(fp);
	
	IOBoolResult(isEOF);
	}
    } else {
        IOFail(mkIOError(nameIllegal,
		         "IO.hIsEOF",
		         "handle is closed",
			 nameNothing));
    }
}


primFun(primHFlush) {			/* Flush handle			   */
    Int h;
    HandleArg(h,3);
    if (handles[h].hmode&(HWRITE|HAPPEND|HREADWRITE)) { /* Only allow flushing writable handles */
	fflush(handles[h].hfp);
	if (handles[h].hmode&HREADWRITE) {
	  handles[h].hHaveRead = FALSE;
	}
	IOReturn(nameUnit);
    }
    else
        IOFail(mkIOError(nameIllegal,
		         "IO.hFlush",
		         "handle is not writable",
			 nameNothing));
}

primFun(primHClose) {			/* Close handle                   */
    Int h;
    HandleArg(h,3);

    /* Disallow closing any of the standard handles */
    if (!IS_STANDARD_HANDLE(h) && handles[h].hmode!=HCLOSED) {
	if (h>HSTDERR && handles[h].hfp)
	    fclose(handles[h].hfp);
	handles[h].hfp   = 0;
	handles[h].hmode = HCLOSED;
    }
    /* closing an already closed handle is the identity
       (i.e., not an error.) */
    IOReturn(nameUnit);
}

primFun(primHGetPosn) {			/* Get file position               */
    Int h;
    HandleArg(h,3);
    if (handles[h].hmode!=HCLOSED) {
#if HAVE_FTELL
	long pos = ftell(handles[h].hfp);
	IOReturn(mkInt((Int)pos));
#else
	/* deliberate fall through to IOFail */
#endif
    }
    IOFail(mkIOError(nameIllegal,
		     "IO.hGetPosn",
		     "handle is closed",
		     nameNothing));
}

primFun(primHSetPosn) {			/* Set file position               */
#if HAVE_FSEEK
    long   pos = 0;
#endif
    Int    h;
    HandleArg(h,4);
    IntArg(pos,3);
    if (handles[h].hmode!=HCLOSED) {
#if HAVE_FSEEK
        fflush(handles[h].hfp);
	if (fseek(handles[h].hfp,pos,SEEK_SET) == 0) {
	    IOReturn(nameUnit);
	}
#else
	/* deliberate fall through to IOFail */
#endif
    }
    IOFail(mkIOError(nameIllegal,
		     "IO.hSetPosn",
		     "handle is closed",
		     nameNothing));
}

primFun(primHSeek) {	/* Seek to new file posn */
                        /* :: Handle -> Int -> Int -> IO () */
  Int h;
  Int sMode;
  Int off;
  
  HandleArg(h,5);
  IntArg(sMode, 4);
  IntArg(off, 3);
  
  if (sMode == 0) 
    sMode = SEEK_SET;
  else if (sMode == 1)
    sMode = SEEK_CUR;
  else
    sMode = SEEK_END;

  if (handles[h].hmode&(HWRITE|HREAD|HAPPEND|HREADWRITE)) {
    if (fseek(handles[h].hfp,off,sMode) != 0) {
      IOFail(mkIOError(toIOError(errno),
		       "IO.hSeek",
		       toIOErrorDescr(errno,TRUE),
		       nameNothing));
    }

    if (handles[h].hmode&HREADWRITE) {
      handles[h].hHaveRead = FALSE;
    }
      
    IOReturn(nameUnit);
  }
  IOFail(mkIOError(nameIllegal,
		   "IO.hSeek",
		   "handle is not seekable",
		   nameNothing));
}


primFun(primHLookAhead) { /* Peek at the next char */
                          /* :: Handle -> IO Char  */
  Int h;
  Int c;
  
  HandleArg(h,3);

  if (handles[h].hmode&(HREAD|HREADWRITE)) {
    if (!feof(handles[h].hfp)) {
      if ((c = fgetc(handles[h].hfp)) != EOF) {
	ungetc(c, handles[h].hfp);
	IOReturn(mkChar(c));
      } else {
	IOFail(mkIOError(toIOError(errno),
			 "IO.hLookAhead",
			 toIOErrorDescr(errno,TRUE),
			 nameNothing));
      }
    } else {
      IOFail(mkIOError(nameEOFErr,
		       "IO.hLookAhead",
		       "end of file",
		       nameNothing));
    }
  } else if (handles[h].hmode&HWRITE) {
    IOFail(mkIOError(nameIllegal,
		     "IO.hLookAhead",
		     "handle is not readable",
		     nameNothing));
  } else {
    IOFail(mkIOError(nameIllegal,
		     "IO.hLookAhead",
		     "handle is closed",
		     nameNothing));
  
  }
}



primFun(primHSetBuffering) {	/* Change a Handle's buffering */
                                /* :: Handle -> Int -> Int -> IO () */
    Int h;
    Int ty;
    Int sz;
    int rc;
    HandleArg(h,5);
    IntArg(ty,4);
    IntArg(sz,3);

    if (handles[h].hmode!=HCLOSED) {
        switch(ty) {
        case 0:
	  ty = _IONBF;
	  handles[h].hbufMode = HANDLE_NOTBUFFERED;
	  handles[h].hbufSize = 0;
	  break;
        case 1:
	  ty = _IOLBF;
	  sz = BUFSIZ;
	  handles[h].hbufMode = HANDLE_LINEBUFFERED;
	  handles[h].hbufSize = 0;
	  break;
        case 2:
	  ty = _IOFBF;
	  handles[h].hbufMode = HANDLE_BLOCKBUFFERED;
	  if (sz == 0) {
	    sz=BUFSIZ;
	  }
	  handles[h].hbufSize = sz;
	  break;
        default:
	  IOFail(mkIOError(nameIllegal,
		           "IO.hSetBuffering",
		           "illegal buffer mode",
		           nameNothing));
        }

	/* Change the buffering mode; setvbuf() flushes the old buffer. */
	/* Let setvbuf() allocate the buffer for us. */
	rc = setvbuf(handles[h].hfp, NULL, ty, sz);
	if (rc != 0) {
	  IOFail(mkIOError(toIOError(errno),
		           "IO.hSetBuffering",
		           "unable to change buffering",
		           nameNothing));
	}
	IOReturn(nameUnit);
    } else
        IOFail(mkIOError(nameIllegal,
		         "IO.hSetBuffering",
		         "handle is closed",
		         nameNothing));
}

primFun(primHGetBuffering) {	/* Return buffering info of a handle. */
                                /*  Handle :: IO (Int,Int)            */
  Int h;
  HandleArg(h,3);

  if (handles[h].hmode != HCLOSED) {
    if (handles[h].hbufMode == HUNKNOWN_BUFFERING) {
      /* figure out buffer mode and size. */
#if HAVE_ISATTY
      if (isatty (fileno(handles[h].hfp)) ) {
	/* TTY connected handles are linebuffered. */
	handles[h].hbufMode = HANDLE_LINEBUFFERED;
	handles[h].hbufSize = 0;
      } else {
#endif
	/* ..if not, block buffered. */
	handles[h].hbufMode = HANDLE_BLOCKBUFFERED;
	handles[h].hbufSize = BUFSIZ;
#if HAVE_ISATTY
      }
#endif
    }
    IOReturn(ap(ap(mkTuple(2),mkInt((Int)handles[h].hbufMode)),
		mkInt((Int)handles[h].hbufSize)));
  } else {
        IOFail(mkIOError(nameIllegal,
		         "IO.hGetBuffering",
		         "handle is closed",
		         nameNothing));
  }
}

primFun(primHIsOpen) {			/* Test is handle open             */
    Int h;
    HandleArg(h,3);
    IOBoolResult(handles[h].hmode!=HCLOSED 
		 && handles[h].hmode!=HSEMICLOSED);
}

primFun(primHIsClosed) {		/* Test is handle closed           */
    Int h;
    HandleArg(h,3);
    IOBoolResult(handles[h].hmode==HCLOSED);
}

primFun(primHIsReadable) {		/* Test is handle readable         */
    Int h;
    HandleArg(h,3);
    IOBoolResult(handles[h].hmode&HREAD || handles[h].hmode&HREADWRITE);
}

primFun(primHIsWritable) {		/* Test is handle writable         */
    Int h;
    HandleArg(h,3);
    IOBoolResult(handles[h].hmode&(HWRITE|HREADWRITE|HAPPEND));
}

#if defined(IS_WINDOWS) && !defined(S_ISREG)
#define S_ISREG(x)  ((x) & _S_IFREG)
#endif

primFun(primHIsSeekable) {		/* Test if handle is writable   */
  Int h;
  Bool okHandle;
#if HAVE_FSTAT
  struct stat sb;
#endif
  
  HandleArg(h,3);
    

  okHandle = (handles[h].hmode&(HREAD|HWRITE|HREADWRITE|HAPPEND));
#if HAVE_FSTAT
  if (okHandle && (fstat(fileno(handles[h].hfp), &sb) == 0)) {
    okHandle = S_ISREG(sb.st_mode);
  }
  IOBoolResult(okHandle);
#else
  IOFail(mkIOError(nameIllegal,
		   "IO.hIsSeekable",
		   "unsupported operation",
		   nameNothing));
#endif
}

primFun(primHFileSize) {  /* If handle points to a regular file,
			     return the size of the file   */
                          /* :: Handle -> IO Integer       */
  Int h;
  Bool okHandle;
#if HAVE_FSTAT
  struct stat sb;
#endif

  HandleArg(h,3);
  
  okHandle = (handles[h].hmode&(HREAD|HWRITE|HREADWRITE|HAPPEND));
#if HAVE_FSTAT
  if (okHandle && (fstat(fileno(handles[h].hfp), &sb) == 0) &&
      S_ISREG(sb.st_mode)) {
    IOReturn(bigWord(sb.st_size));
  }
  IOFail(mkIOError(nameIllegal,
		   "IO.hFileSize",
		   (okHandle ? "not a regular file" : "handle is (semi-)closed."),
		   nameNothing));
#else
  IOFail(mkIOError(nameIllegal,
		   "IO.hFileSize",
		   "unsupported operation",
		   nameNothing));
#endif
}

primFun(primEqHandle) {			/* Test for handle equality        */
    Int h1, h2;
    HandleArg(h1,1);
    HandleArg(h2,2);
    BoolResult(h1==h2);
}

primFun(primReadFile) {			/* read file as lazy string	   */
  Cell hnd = openHandle(root,IOArg(1),HREAD,FALSE,"Prelude.readFile");
  if (isNull(fst(hnd))) {
    IOFail(snd(hnd));
  }
  handles[intValOf(snd(hnd))].hmode = HSEMICLOSED;
  IOReturn(ap(nameHreader,snd(hnd)));
}

primFun(primReadBinaryFile) {		/* read file as lazy string	   */
  Cell hnd = openHandle(root,IOArg(1),HREAD,TRUE,"IOExtensions.readBinaryFile");
  if (isNull(fst(hnd))) {
    IOFail(snd(hnd));
  }
  handles[intValOf(snd(hnd))].hmode = HSEMICLOSED;
  IOReturn(ap(nameHreader,snd(hnd)));
}

primFun(primWriteFile) {		/* write string to specified file  */
    fwritePrim(root,FALSE,FALSE,"Prelude.writeFile");
}

primFun(primAppendFile) {		/* append string to specified file */
    fwritePrim(root,TRUE,FALSE,"Prelude.appendFile");
}

primFun(primWriteBinaryFile) {		/* write string to specified file  */
    fwritePrim(root,FALSE,TRUE,"IOExtensions.writeBinaryFile");
}

primFun(primAppendBinaryFile) {		/* append string to specified file */
    fwritePrim(root,TRUE,TRUE,"IOExtensions.appendBinaryFile");
}

static Void local fwritePrim(root,append,binary,loc)
                                 /* Auxiliary function for  */
StackPtr root;			 /* writing/appending to    */
Bool     append; 		 /* an output file	    */
Bool     binary;
String   loc; {
    String s    = evalName(IOArg(2));		/* Eval and check filename */
    FILE* wfp;

    if (!s) {
        IOFail(mkIOError(nameIllegal,
			 loc,
		         "illegal file name",
			 IOArg(2)));
    /* Note: there used to be a test here which would signal an
       IO error if the file didn't exist and the user was
       attempting to append to it. Haskell98 requires, quite
       sensibly, this to succeed, hence the test has been removed.
    */
    } else {
	String stmode;

	if (binary) {
	  stmode = append ? "ab+" : "wb+";
	} else {
	  stmode = append ? "a+" : "w+";
	}
	if  ( (wfp = fopen(s,stmode)) == NULL ) {
	  IOFail (mkIOError(toIOError(errno),
			    loc,
			    toIOErrorDescr(errno,TRUE),
			    IOArg(2)));
	}
	
	blackHoleRoot();
	drop();
	eval(pop());
	while (whnfHead==nameCons) {
	  eval(top());
	  checkChar();
	  fputc(charOf(whnfHead),wfp);
	  drop();
	  eval(pop());
	}
	fclose(wfp);
	IOReturn(nameUnit);
    }
}

primFun(primGetErrorString) {		/* :: IOError -> String	   */
  updateRoot(ap(nameGetErrorString,primArg(1)));
}

primFun(primGetHandle) {		/* :: IOError -> Maybe Handle	   */
    eval(primArg(1));
    /* insert tests here */
    updateRoot(nameNothing);
}

primFun(primGetFileName) {		/* :: IOError -> Maybe FilePath	   */
  updateRoot(ap(nameGetFilename,primArg(1)));
}

primFun(primHWaitForInput) { /* Check whether a character can be read
				from a handle within x msecs */
                             /* :: Handle -> Int -> IO Bool */
  Int h;
  Int msecs;
  
  HandleArg(h,4);
  IntArg(msecs,3);
  
#if defined(HAVE_SELECT)
  if (handles[h].hmode&(HREAD|HREADWRITE)) {
    /* Implementation is a rip-off of GHC's inputReady.c */
    int maxfd, fd;
    int ready;
    fd_set rfd;
    struct timeval tv;
    
    FD_ZERO(&rfd);
    fd = fileno(handles[h].hfp);
    FD_SET(fd, &rfd);
    
    maxfd = fd + 1;
    tv.tv_sec  = msecs / 1000;
    tv.tv_usec = msecs % 1000;
    
    while ( (ready = select(maxfd, &rfd, NULL, NULL, &tv)) < 0 ) {
      if (errno != EINTR) {
	IOFail(mkIOError(nameIllegal,
			 "IO.hWaitForInput",
			 "input waiting terminated by signal",
			 nameNothing));
      }
    }
    IOBoolResult(ready > 0);
  } else {
    IOFail(mkIOError(nameIllegal,
		     "IO.hWaitForInput",
		     "handle is not readable",
		     nameNothing));
  }
#else
  /* For now, punt on implementing async IO under Win32 */
  /* For other platforms that don't support select() on file
     file descs, please insert code that'll work. */
  IOFail(mkIOError(nameIllegal,
		   "IO.hWaitForInput",
		   "unsupported operation",
		   nameNothing));
#endif
}

#endif /* IO_HANDLES */

/* --------------------------------------------------------------------------
 * Mutable variables
 * ------------------------------------------------------------------------*/

#if IO_REFS

#if CHECK_TAGS
#define checkRef() if (MUTVAR != whatIs(whnfHead)) internal("Ref expected")
#else
#define checkRef() /* do nothing */
#endif

primFun(primNewRef) {			/* a -> IO (Ref a)		   */
    IOReturn(ap(MUTVAR,IOArg(1)));
}

primFun(primDerefRef) {			/* Ref a -> IO a		   */
    eval(pop());
    checkRef();
    IOReturn(snd(whnfHead));
}

primFun(primAssignRef) {		/* Ref a -> a -> IO ()		   */
    eval(IOArg(2));
    checkRef();
    snd(whnfHead) = IOArg(1);
    IOReturn(nameUnit);
}

primFun(primEqRef) {			/* Ref a -> Ref a -> Bool	   */
    eval(primArg(2));
    checkRef();
    push(whnfHead);
    eval(primArg(1));
    checkRef();
    updateRoot(pop()==whnfHead ? nameTrue : nameFalse);
}
#endif

/* --------------------------------------------------------------------------
 * Stable Pointers
 * ------------------------------------------------------------------------*/

#if CHECK_TAGS
#define checkSP() checkInt()
#else
#define checkSP() /* do nothing */
#endif

#define SPArg(nm,offset)                          \
    eval(primArg(offset));                        \
    checkSP();                                    \
    nm = (HugsStablePtr)whnfInt

/* nm should be a variable in which result is stored.
   If you use an expression, reevaluation might occur */
#define SPResult(nm)                              \
   updateRoot(mkInt((Int)(nm)))


primFun(primMakeSP) {			/* a -> IO (StablePtr a)	   */
    HugsStablePtr sp = mkStablePtr(IOArg(1));
    if (sp != 0) {
	IOReturn(mkInt(sp));
    } else {
        IOFail(mkIOError(nameIllegal,
			 "Foreign.makeStablePtr",
		         "illegal operation",
			 nameNothing));
    }
}

primFun(primDerefSP) {			/* StablePtr a -> IO a   	   */
    HugsStablePtr x;
    SPArg(x,3);
    
    IOReturn(derefStablePtr(x));
}

primFun(primFreeSP) {			/* StablePtr a -> IO ()   	   */
    HugsStablePtr x;
    SPArg(x,3);
    freeStablePtr(x);
    IOReturn(nameUnit);
}

primFun(primCastSPToP) {		/* StablePtr a -> Ptr ()   	   */
    HugsStablePtr x;
    SPArg(x,1);
    PtrResult((Pointer)x);
}

primFun(primCastPToSP) {		/* Ptr () -> StablePtr a   	   */
    Pointer x;
    PtrArg(x,1);
    SPResult((HsStablePtr)x);
}


    
/* --------------------------------------------------------------------------
 * Foreign Objects
 * ------------------------------------------------------------------------*/

#if CHECK_TAGS
#define checkForeign() if (MPCELL != whatIs(whnfHead)) internal("ForeignObj expected")
#else
#define checkForeign() /* do nothing */
#endif

primFun(primNewFP) { /* Ptr a -> FunPtr (Ptr a -> IO ()) -> IO (ForeignPtr a) */
    Pointer addr = 0;
    Void (*free)(Pointer) = 0;
    eval(IOArg(2));
    addr = ptrOf(whnfHead);
    eval(IOArg(1));
    free = (Void (*)(Pointer))ptrOf(whnfHead);
    IOReturn(mkMallocPtr(addr,free));
}

primFun(primWriteFP) {		/* ForeignPtr a -> Ptr a -> IO ()	   */
    Cell mp = NIL;
    eval(IOArg(2));
    checkForeign();
    mp = whnfHead;
    eval(IOArg(1));
    derefMP(mp) = ptrOf(whnfHead);
    IOReturn(nameUnit);
}

primFun(primEqFP) {		/* ForeignPtr a -> ForeignPtr a -> Bool    */
    eval(primArg(2));
    checkForeign();
    push(whnfHead);
    eval(primArg(1));
    checkForeign();
    updateRoot(pop()==whnfHead ? nameTrue : nameFalse);
}

primFun(primTouchFP) {		/* ForeignPtr a -> IO ()	           */
    eval(IOArg(2));
    checkForeign();
    IOReturn(nameUnit);
}

primFun(primFPToP) {		/* ForeignPtr a -> Ptr a                   */
    eval(primArg(1));
    checkForeign();
    PtrResult(derefMP(whnfHead));
}

#if STABLE_NAMES
/* --------------------------------------------------------------------------
 * Stable Names
 * ------------------------------------------------------------------------*/

primFun(primMakeSN) {		/* a -> IO (StableName a)		   */
    IOReturn(ap(STABLENAME,IOArg(1)));
}

primFun(primDerefSN) {		/* StableName a -> a			   */
    eval(primArg(1));
    updateRoot(snd(whnfHead));
}

primFun(primHashSN) {		/* StableName a -> Int			   */
    eval(primArg(1));
    updateRoot(mkInt(whnfHead));
}
primFun(primEqSN) {		/* StableName a -> StableName a -> Bool	   */
    eval(primArg(2));
    push(whnfHead);
    eval(primArg(1));
    updateRoot(pop()==whnfHead ? nameTrue : nameFalse);
}
#endif

#if GC_WEAKPTRS
/* --------------------------------------------------------------------------
 * Weak Pointers
 * ------------------------------------------------------------------------*/

#if CHECK_TAGS
#define checkWeak() if(WEAKCELL!=whatIs(whnfHead)) internal("weakPtr expected");
#else
#define checkWeak() /* do nothing */
#endif

primFun(primMakeWeakPtr) {		/* a -> IO (Weak a)		   */
    assert(isGenPair(IOArg(1)));	/* (Sadly, this may not be true)   */
    IOReturn(mkWeakPtr(IOArg(1)));	/* OLD ... retire soon		   */
}

primFun(primDerefWeakPtr) {		/* Weak a -> IO (Maybe a)	   */
    eval(IOArg(1));			/* OLD ... retire soon		   */
    checkWeak();
    if (isNull(derefWeakPtr(whnfHead))) {
	IOReturn(nameNothing);
    } else {
	IOReturn(ap(nameJust,derefWeakPtr(whnfHead)));
    }
}

primFun(primWeakPtrEq) {		/* Weak a -> Weak a -> Bool	   */
    eval(primArg(2));
    push(whnfHead);
    eval(primArg(1));
    updateRoot(pop()==whnfHead ? nameTrue : nameFalse);
}

primFun(primMkWeak) {			/* k -> v -> Maybe (IO ())	   */
    Cell w = NIL;			/*		    -> IO (Weak v) */
    eval(IOArg(1));
    if (whnfHead==nameJust) {		/* Look for finalizer		   */
	w = pop();
    }
    w		     = ap(NIL,ap(NIL,ap(NIL,w)));
    fst(snd(w))      = IOArg(3);
    fst(snd(snd(w))) = IOArg(2);
    liveWeakPtrs     = cons(w,liveWeakPtrs);
    fst(w)           = WEAKFIN;
    IOReturn(w);
}

primFun(primDeRefWeak) {		/* Weak v -> IO (Maybe v)	   */
    eval(IOArg(1));
    if (whatIs(whnfHead)!=WEAKFIN) {
	internal("primDeRefWeak");
    }
    if (nonNull(snd(whnfHead))) {
	IOReturn(ap(nameJust,fst(snd(snd(whnfHead)))));
    } else {
	IOReturn(nameNothing);
    }
}

primFun(primReplaceFinalizer) {		/* Weak v -> Maybe (IO ())	   */
					/*	-> IO (Maybe (IO ()))	   */
    eval(IOArg(1));			/* Grab new finalizer ...	   */
    if (whnfHead!=nameJust) {
	push(NIL);
    }
    eval(IOArg(2));			/* Get weak pointer ...		   */
    if (whatIs(whnfHead)!=WEAKFIN) {
	internal("primReplaceFinalizer");
    } else if (nonNull(snd(whnfHead))) {/* ... and replace finalizer	   */
	Cell oldfin = snd(snd(snd(whnfHead)));
	snd(snd(snd(whnfHead))) = pop();
	if (nonNull(oldfin)) {
	    IOReturn(ap(nameJust,oldfin));
	}
    }
    IOReturn(nameNothing);
}

primFun(primFinalize) {			/* Weak v -> IO ()		   */
    eval(IOArg(1));			/* Bring weak pointer to an early  */
    if (whatIs(whnfHead)!=WEAKFIN) {	/* end ...			   */
	internal("primFinalize");
    } else if (nonNull(snd(whnfHead))) {
	Cell wp = whnfHead;
	Cell vf = snd(snd(wp));
	if (isPair(vf)) {
	    if (nonNull(snd(vf))) {
		fst(vf)    = snd(vf);
		snd(vf)    = finalizers;
		finalizers = vf;
	    }
	    fst(snd(wp)) = NIL;
	    snd(snd(wp)) = NIL;
	    snd(wp)      = NIL;
	}
	liveWeakPtrs = removeCell(wp,liveWeakPtrs);
    }
    IOReturn(nameUnit);
}

primFun(primRunFinalizer) {		/* IO ()			   */
    if (isNull(finalizers)) {
	IOReturn(nameUnit);
    } else {
	updapRoot(ap(hd(finalizers),primArg(2)),primArg(1));
	finalizers = tl(finalizers);
	return;
    }
}

primFun(primFinalizerWaiting) {		/* IO Boolean			   */
  IOBoolResult(!isNull(finalizers));
}
#endif /* GC_WEAKPTRS */


#if HSCRIPT
#if EMBEDDED
extern void* getCurrentScript(void);

primFun(primGetCurrentScript) {  /* IO Int */
    IOReturn( mkInt( (int)getCurrentScript() ) );
}

#else
 
primFun(primGetCurrentScript) {  /* IO Int */
    IOReturn( mkInt( 0 ) );
}

#endif /* EMBEDDED */
#endif /* HSCRIPT */

/*-------------------------------------------------------------------------*/
