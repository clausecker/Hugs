/* --------------------------------------------------------------------------
 * Implementation of the Hugs server API.
 *
 * The Hugs server allows you to write batch-mode programs that load
 * scripts and build/evaluate terms.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2002, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: server.c,v $
 * $Revision: 1.17 $
 * $Date: 2002/07/05 12:55:48 $
 * ------------------------------------------------------------------------*/

#define HUGS_SERVER
#include "hugs.c"
#include "server.h"

DLLEXPORT(HugsServerAPI*) initHugsServer Args((Int, String[]));
DLLEXPORT(Void) shutdownHugsServer Args((HugsServerAPI*));

static Void   setError     Args((String));
static Void   setHugsAPI   Args((Void));
static Void   startEval    Args((Void));
static Bool   SetModule    Args((String));
#ifndef NO_DYNAMIC_TYPES
static Bool   linkDynamic  Args((Void));
static Cell   getDictFor   Args((Class,Type));
#endif

/* --------------------------------------------------------------------------
 * Dynamic linking
 *
 * The simplest way to do dynamic linking is this:
 * 1) load the dll/shared object file
 * 2) get the address of an initialisation function
 *    from the dll symbol table
 * 3) call the initialisation function - which returns a "virtual
 *    function table" - a struct containing the addresses of all other
 *    functions and variables that we need to access.
 * ------------------------------------------------------------------------*/

static String ClearError      Args((Void));
static Int    GetNumScripts   Args((Void));
static Void   Reset           Args((Int));
static Void   SetOutputEnable Args((Bool));
static Void   ChangeDir       Args((String));
static Void   LoadProject     Args((String));
static Void   LoadFile        Args((String));
static Void   LoadStringF     Args((String));
static Void   SetOptions      Args((String));
static String GetOptions      Args((Void));
static HVal   CompileExpr     Args((String, String));
static Void   GarbageCollect  Args((void));
static Void   LookupName      Args((String, String));
static Void   MkInt           Args((Int));
static Void   MkAddr          Args((void*));
static Void   MkString        Args((String));
static Void   Apply           Args((Void));
static Int    EvalInt         Args((Void));
static void*  EvalAddr        Args((void));
static String EvalString      Args((Void));
static Int    DoIO            Args((Void));
static Int    DoIO_Int        Args((int*));
static Int    DoIO_Addr       Args((void**));
static HVal   PopHVal         Args((Void));
static Void   PushHVal        Args((HVal));
static Void   FreeHVal        Args((HVal));

static HugsServerAPI hugs;             /* virtual function table            */

static Void setHugsAPI() {       /* initialise virtual function table */
    hugs.clearError      = ClearError;
    hugs.setHugsArgs     = setHugsArgs;
    hugs.getNumScripts   = GetNumScripts;
    hugs.reset           = Reset;
    hugs.setOutputEnable = SetOutputEnable;
    hugs.changeDir       = ChangeDir;
    hugs.loadProject     = LoadProject;
    hugs.loadFile        = LoadFile;
    hugs.loadFromBuffer  = LoadStringF;
    hugs.setOptions      = SetOptions;
    hugs.getOptions      = GetOptions;
    hugs.compileExpr     = CompileExpr;
    hugs.garbageCollect  = GarbageCollect;
    hugs.lookupName      = LookupName;
    hugs.mkInt           = MkInt;
    hugs.mkAddr          = MkAddr;
    hugs.mkString        = MkString;
    hugs.apply           = Apply;
    hugs.evalInt         = EvalInt;
    hugs.evalAddr        = EvalAddr;
    hugs.evalString      = EvalString;
    hugs.doIO            = DoIO;
    hugs.doIO_Int        = DoIO_Int;
    hugs.doIO_Addr       = DoIO_Addr;
    hugs.popHVal         = PopHVal;
    hugs.pushHVal        = PushHVal;
    hugs.freeHVal        = FreeHVal;
}

/* --------------------------------------------------------------------------
 * Error handling
 *
 * We buffer error messages and refuse to execute commands until
 * the error is cleared.
 * ------------------------------------------------------------------------*/

#define ErrorBufferSize 10000

static char  serverErrMsg[ErrorBufferSize];   /* Buffer for error messages */
char* lastError = NULL;

static String ClearError()
{
    String err = lastError;
    lastError  = NULL;
    ClearOutputBuffer();

    if (err && (numScripts > 0)) 
    {
        everybody(RESET);        
        dropScriptsFrom(numScripts-1);  /* remove partially loaded scripts */
    }
    return err;
}

static Void setError(s)            /* Format an error message        */
String s; {
    Int    n = 0;
    String err = ClearOutputBuffer();

    if (NULL == err) {
	n = snprintf(serverErrMsg, ErrorBufferSize, "%s\n", s);
    } else {
	n = snprintf(serverErrMsg, ErrorBufferSize, "%s\n%s\n", s, err);
    }
    if (0 <= n && n <= ErrorBufferSize) {
	lastError = serverErrMsg;
    } else {
	lastError = "error buffer overflowed\n";
    }
}

/* All server entry points set CStackBase for the benefit of the (conservative)
 * GC and do error catching.  Any calls to Hugs functions should be "protected"
 * by being placed inside this macro.
 *
 *   void entryPoint(arg1, arg2, result)
 *   T1 arg1;
 *   T2 arg2;
 *   T3 *result;
 *   {
 *       protect(doNothing(),
 *           ...
 *       );
 *   }
 *
 * Macro decomposed into BEGIN_PROTECT and END_PROTECT pieces so that i
 * can be used on some compilers (Mac?) that have limits on the size of
 * macro arguments.
 */
#define BEGIN_PROTECT \
  if (NULL == lastError) { \
      Cell dummy; \
      CStackBase = &dummy;              /* Save stack base for use in gc  */ \
      consGC = TRUE;                    /* conservative GC is the default */ \
      if (!setjmp(catch_error)) {
#define END_PROTECT \
      } else { \
	setError("Error occurred"); \
	normalTerminal(); \
      }	\
  }
#define protect(s)	BEGIN_PROTECT s; END_PROTECT

/* --------------------------------------------------------------------------
 * Initialisation
 * ------------------------------------------------------------------------*/

/* I've added a special case for the server.  Probably should just add
   another entry point but what the heck.  If argc = -1 then the hugs
   server should NOT read registry or default hugs path stuff.  Instead,
   all options are in the first argument in argv.   -- jcp

*/

DLLEXPORT(HugsServerAPI*) initHugsServer(argc, argv) /*server initialisation*/
Int    argc;
String argv[]; {

    static Bool is_initialized = FALSE;

    if (!is_initialized) {
      is_initialized = TRUE;
      setHugsAPI();
      
      BEGIN_PROTECT			/* Too much text for protect()	   */
      Int i;

      lastEdit      = 0;
      setLastEdit((String)0,0);
      scriptFile    = 0;		/* Name of current script (if any) */
      numScripts    = 0;		/* Number of scripts loaded	   */
      namesUpto     = 0;		/* Number of script names set	   */
      hugsPath      = strCopy(HUGSPATH);

      if (argc == -1) {
	readOptions(argv[0],FALSE);
      } else {
#if USE_REGISTRY
	projectPath = readRegChildStrings(HKEY_LOCAL_MACHINE, ProjectRoot, "HUGSPATH", PATHSEP, "");
	readOptions(readRegString(HKEY_LOCAL_MACHINE,HugsRoot,"Options",""), TRUE);
	readOptions(readRegString(HKEY_CURRENT_USER,HugsRoot,"Options",""),TRUE);
#endif /* USE_REGISTRY */
	readOptions(fromEnv("HUGSFLAGS",""),FALSE);
	for (i=1; i<argc; ++i) {
	  if (!readOptions2(argv[i])) {
	    setError("Unrecognised option");
	    return NULL;
	  }
	}
      }
      everybody(INSTALL);
      addScriptName(STD_PRELUDE,TRUE);
#ifndef NO_DYNAMIC_TYPES
      addScriptName("HugsDynamic",TRUE);
#endif
      EnableOutput(FALSE);

      readScripts(0);
      everybody(RESET);
#ifndef NO_DYNAMIC_TYPES
      if (!linkDynamic()) {
	setError("module HugsDynamic doesn't define correct functions");
	return NULL;
      }
#endif
      END_PROTECT
   }
   return &hugs;  /* error must have occurred */
}

DLLEXPORT(Void) shutdownHugsServer(hserv) /* server shutdown */
HugsServerAPI* hserv; {
  /* The 'hserv' argument isn't actually used */
  clearStack();
  stopAnyPrinting();
  everybody(EXIT);
  shutdownHugs();
  return;
}

/* --------------------------------------------------------------------------
 * Dynamic typing
 *
 * We use dynamic typing to make the system more robust - all values pushed
 * on the stack have type "Dynamic" and we check the real type on function
 * application and evaluation.  This requires on functions and a class
 * defined in the HugsDynamic library.
 *
 * sof 2001 - interposing a Dynamic-typed layer sometimes gets in the way
 *            (missing Typeable instances, extra loading of modules), as
 *            has proven the case when using the HugsServerAPI by HaskellScript,
 *            Lambada, and mod_haskell. So, by defining NO_DYNAMIC_TYPES,
 *            you turn this feature off.
 * ------------------------------------------------------------------------*/

#ifndef NO_DYNAMIC_TYPES
static Name  nameIntToDyn;
static Name  nameFromDynInt;
static Name  nameAddrToDyn;
static Name  nameFromDynAddr;
static Name  nameStringToDyn;
static Name  nameFromDynString;
static Name  nameRunDyn;
static Name  nameDynApp;
static Name  nameToDynamic;
static Class classTypeable;

static Bool linkDynamic()
{
    nameIntToDyn      = findName(findText("intToDyn"));
    nameFromDynInt    = findName(findText("fromDynInt"));
    nameAddrToDyn     = findName(findText("addrToDyn"));
    nameFromDynAddr   = findName(findText("fromDynAddr"));
    nameStringToDyn   = findName(findText("strToDyn"));
    nameFromDynString = findName(findText("fromDynStr"));
    nameRunDyn        = findName(findText("runDyn"));
    nameDynApp        = findName(findText("dynApp"));
    nameToDynamic     = findName(findText("toDynamic"));
    classTypeable     = findClass(findText("Typeable"));

    return (   nonNull(nameIntToDyn      )
	    && nonNull(nameFromDynInt    )
	    && nonNull(nameAddrToDyn     )
	    && nonNull(nameFromDynAddr   )
	    && nonNull(nameStringToDyn   )
	    && nonNull(nameFromDynString )
	    && nonNull(nameRunDyn        )
	    && nonNull(nameDynApp        )
	    && nonNull(nameToDynamic     )
	    && nonNull(classTypeable     ));
}
#endif

/* --------------------------------------------------------------------------
 * Miscellaneous:
 * ------------------------------------------------------------------------*/

static Int GetNumScripts()      /* Get number of scripts in system  */
{
    protect(return numScripts);
    return 0;
}

static Void Reset(scripts) /* Restore number of scripts to old level */
Int scripts; {
    protect(
	ClearOutputBuffer();
	clearProject();
	forgetScriptsFrom(scripts);
	everybody(RESET);
	);
}

static Void SetOutputEnable(f)    /* enable/disable compiler output  */
Bool f;
{
    protect(EnableOutput(f));
}

static Void ChangeDir(s)          /* change current directory        */
String s;
{
    protect(
	if (chdir(s)) {
	    setError("changeDir: invalid directory");
	    return;
	}
	);
}

static Void LoadProject(fn)       /* load a project into the system  */
String fn;
{
    protect(
	loadProject(strCopy(fn));
	readScripts(numScripts);
	everybody(RESET);
	);
}

static Void LoadFile(fn)          /* load a module (from a file) into the system   */
String fn;
{
  /*
   * The meaning of load file 
   */
    protect(
	addScriptName(fn,TRUE);
	readScripts(numScripts);
	everybody(RESET);
	);
}

static Void LoadStringF(mod)      /* load a module (from a string) into the system   */
String mod;
{
    protect(
	addScriptName(mod,TRUE);
	readScripts(numScripts);
	everybody(RESET);
	);
}

static Void SetOptions(opt)
String opt; {
  readOptions2(opt);
  return;
}

static String GetOptions() {
  return strCopy(optionsToStr());
}


static Bool SetModule(m)
String m; {
    Module mod   = findModule(findText(m));
    if (isNull(mod)) {
	return FALSE;
    }
    setCurrModule(mod);
    return TRUE;
}

static HVal CompileExpr(m,e)    /* compile expression e wrt module m */
String m;
String e; {
    BEGIN_PROTECT
	Type type = NIL;
	Cell d    = NIL;
	HVal r    = 0;

	if (!SetModule(m)) {
	    setError("compileExpr: invalid module");
	    return 0;
	}
	scriptFile = 0;
	stringInput(e);                /* Put expression into input buffer */
				
	startNewScript(0);             /* Enables recovery of storage      */
				       /* allocated during evaluation      */
	parseExp();
	checkExp();
	defaultDefns = evalDefaults;
	type         = typeCheckExp(TRUE);
#ifndef NO_DYNAMIC_TYPES
	d = getDictFor(classTypeable,type);
	if (isNull(d)) {
	    setError("compileExpr: can't create Typeable instance");
	    return 0;
	}
	inputExpr = ap(ap(nameToDynamic,d),inputExpr);
#endif
	compileExp();
	run(inputCode,sp);  /* Build graph for redex */

	r = mkStablePtr(pop());
	if (0 == r) {
	    setError("compileExpr: no free stable pointers");
	    return 0;
	}
	return r;
    END_PROTECT
    return 0;
}

static Void GarbageCollect()
{
    garbageCollect();
}

/* --------------------------------------------------------------------------
 * Building expressions on the stack
 * ------------------------------------------------------------------------*/

static Void LookupName(m,v) /*Push value of qualified name onto stack*/
String m,v;
{
    BEGIN_PROTECT
	Name   n;
	Cell   d;
        char   errbuf[256];

	if (!SetModule(m)) {
	    snprintf(errbuf, 255, "lookupName: invalid module '%s'", m);
	    setError(errbuf);
	    return;
	}
	if (isNull(n = findName(findText(v)))) {
            snprintf( errbuf, 255, "lookupName: invalid name '%s.%s'", m, v );
	    setError(errbuf);
	    return;
	}
#ifndef NO_DYNAMIC_TYPES
	d = getDictFor(classTypeable,name(n).type);
	if (isNull(d)) {
	    setError("lookupName: can't create Typeable instance");
	    return;
	}
	push(ap(ap(nameToDynamic,d),n));
#else
	push(n);
#endif
    END_PROTECT
}

static Void MkInt(i)              /* Push an Int onto the stack      */
Int i;
{
#ifndef NO_DYNAMIC_TYPES
    protect(push(ap(nameIntToDyn,mkInt(i))));
#else
    protect(push(mkInt(i)));
#endif
}

static Void MkAddr(a)              /* Push an Addr onto the stack      */
void* a;
{
#ifndef NO_DYNAMIC_TYPES
    protect(push(ap(nameAddrToDyn,mkPtr(a))));
#else
    protect(push(mkPtr(a)));
#endif
}

static Void MkString(s)           /* Push a String onto the stack    */
String s;
{
    BEGIN_PROTECT
	Cell   r = NIL;
	String t = s;
	push(nameNil);
	while (*t) ++t;
	while (t-- != s) {
	    Cell ss = pop();
	    push(ap(consChar(*t),ss));
	}
#ifndef NO_DYNAMIC_TYPES
	r = pop();
	push(ap(nameStringToDyn,r));
#endif
    END_PROTECT
}

static Void Apply()              /* Apply stack[sp-1] to stack[sp]   */
{
    BEGIN_PROTECT
	Cell x = pop();
	Cell f = pop();
#ifndef NO_DYNAMIC_TYPES
	push(ap(ap(nameDynApp,f),x));
#else
	push(ap(f,x));
#endif
    END_PROTECT
}

/* --------------------------------------------------------------------------
 * Evaluate top of stack
 * ------------------------------------------------------------------------*/

static Void startEval()
{
    numCells      = 0;
    numReductions = 0;
    numGcs        = 0;
    printing      = TRUE;
    noechoTerminal();
    consGC = FALSE;
    noechoTerminal();
}

static void evalError(Cell e)
{
#define MAXLEN 255
  char message[MAXLEN] = ""; /* "error called: "; */
  int  len;
  len = strlen(message);

  push( arg(e) );

  while(1) {
    if (nonNull(evalWithNoError(pop())))  break; /* error in error ! */
    
    if (whnfHead!=nameCons) break;  /* end of string */
    if (nonNull(evalWithNoError(pop())))  break; /* error in error ! */

    message[len] = charOf(whnfHead);
    len++;
    if (len >= MAXLEN)      break;
  }

  message[len] = '\0';
  setError(message);
}

static Bool tryEval(Cell c)
{
    Cell temp = evalWithNoError(c);
    if (nonNull(temp))
    {
        evalError(temp);
        return FALSE;
    }
    else return TRUE;
}


static Bool safeEval(Cell c)
{
        Bool ok;
        startEval();
        ok = tryEval(c);
        normalTerminal();
        return ok;
}


static Int EvalInt()            /* Evaluate a cell (:: Int)         */
{
    BEGIN_PROTECT
	startEval();
#ifndef NO_DYNAMIC_TYPES
	safeEval(ap(nameFromDynInt,pop()));
#else
	safeEval(pop());
#endif
	normalTerminal();
	return whnfInt;
    END_PROTECT
    return 0;
}

static void* EvalAddr()          /* Evaluate a cell (:: Addr)         */
{
    BEGIN_PROTECT
	startEval();
#ifndef NO_DYNAMIC_TYPES
	safeEval(ap(nameFromDynAddr,pop()));
#else
	safeEval(pop());
#endif
	normalTerminal();
	return ptrOf(whnfHead);
    END_PROTECT
    return 0;
}

static String EvalString()      /* Evaluate a cell (:: String)      */
{
    BEGIN_PROTECT
	Int      len = 0;
	String   s;
	Bool     ok;
	StackPtr oldsp = sp;

	startEval();

	/* Evaluate spine of list onto stack */
#ifndef NO_DYNAMIC_TYPES
	ok = tryEval(ap(nameFromDynString,pop()));
#else
	ok = tryEval(pop());
#endif
        if (!ok) { sp = oldsp-1; return NULL; }

	while (whnfHead==nameCons && whnfArgs==2) {
	    Cell e  = pop();
	    Cell es = pop();
	    len++;
	    push(e);
	    ok = tryEval(es);
            if (!ok) { sp = oldsp-1; return NULL; }
	}
	normalTerminal();

	if (whnfHead != nameNil) {
	    setError("evalString: nil expected");
	    return NULL;
	}
	if (sp != oldsp-1+len) {
	    setError("evalString: unbalanced stack1");
	    return NULL;
	}

	/* Pull characters off stack into array */
	if (!(s = malloc(len+1))) {
	    setError("Malloc failed in mkString");
	    return NULL;
	}
	s[len] = '\0';
	while (--len >= 0) {
	   ok = tryEval(pop());
	   if (!ok) { sp  = oldsp; free(s); return NULL; }
	   s[len] = charOf(whnfHead);
	}
	if (sp+1 != oldsp) {
	    setError("evalString: unbalanced stack2");
	    return NULL;
	}
	return s;
    END_PROTECT
    return NULL;
}

static Int DoIO()        /* Evaluate a cell (:: IO ()) return exit status */
{
    BEGIN_PROTECT
	Int exitCode = 0;
        Bool ok;
	StackPtr oldsp = sp;
	startEval();
#ifndef NO_DYNAMIC_TYPES
	ok = safeEval(ap(nameIORun,ap(nameRunDyn,pop())));
#else
	ok = safeEval(ap(nameIORun,pop()));
#endif
        if (!ok)
        {
            sp = oldsp-1;   
            exitCode = 1;
        } else if (whnfHead == nameLeft) { /* Left exitCode -> return exitCode */
	    safeEval(pop());
	    exitCode = whnfInt;
	} else {                    /* Right void    -> return 0        */
	    drop();
	    exitCode = 0; /* implicit exit code is 0 */
	}
	normalTerminal();
	if (sp != oldsp-1) {
	    setError("doIO: unbalanced stack");
	    return 1;
	}
	return exitCode;
    END_PROTECT
    return -1; /* error code */
}

/* 
 * Evaluate a cell (:: IO Int) return exit status
 */
static Int DoIO_Int(int* phval)
{
    BEGIN_PROTECT
        Int exitCode = 0;
        Bool ok = TRUE;
        StackPtr oldsp = sp;
        startEval();
#ifndef NO_DYNAMIC_TYPES
        ok = safeEval(ap(nameIORun,ap(nameRunDyn,pop())));
#else
        ok = safeEval(ap(nameIORun,pop()));
#endif
        if (!ok)
        {
            sp = oldsp-1;   
            exitCode = 1;
        } else if (whnfHead == nameLeft) { 
            safeEval(pop());
            exitCode = whnfInt;
        } else {   
	    if (phval) {
	      safeEval(pop());
	      *phval = whnfInt;
	    } else {
	      drop();
	    }
            exitCode = 0; 
        }
        normalTerminal();
        if (sp != oldsp-1) {
            setError("doIO: unbalanced stack");
            return 1;
        }
        return exitCode;
    END_PROTECT;
    return -1; /* error code */
}

/* 
 * Evaluate a cell (:: IO Addr) return exit status
 */
static Int DoIO_Addr(void** phval)
{
    BEGIN_PROTECT
        Int exitCode = 0;
        Bool ok;
        StackPtr oldsp = sp;
        startEval();
#ifndef NO_DYNAMIC_TYPES
        ok = safeEval(ap(nameIORun,ap(nameRunDyn,pop())));
#else
        ok = safeEval(ap(nameIORun,pop()));
#endif
        if (!ok)
        {
            sp = oldsp-1;   
            exitCode = 1;
	} else if (whnfHead == nameLeft) { 
            safeEval(pop());
            exitCode = whnfInt;
        } else {   
	    if (phval) {
	      safeEval(pop());
	      *phval = (void*)ptrOf(whnfHead);
	    } else {
	      drop();
	    }
            exitCode = 0; 
        }
        normalTerminal();
        if (sp != oldsp-1) {
            setError("doIO: unbalanced stack");
            return 1;
        }
        return exitCode;
    END_PROTECT;
    return -1; /* error code */
}

/* --------------------------------------------------------------------------
 * Stable pointers
 *
 * If a value is popped off the stack, it is made into a stable pointer
 * which must be explicitly freed.
 * ------------------------------------------------------------------------*/

static HVal PopHVal()           /* Get a value off the stack         */
{
    protect(
	HVal r = mkStablePtr(pop());
	if (0 == r) {
	    setError("popHVal: no free stable pointers");
	    return 0;
	}
	return r;
	);
    return 0;
}

static Void PushHVal(hval)      /* Put a value back on the stack     */
HVal hval;
{
    protect(
	if (hval == 0) {
	    setError("pushHVal: invalid HVal");
	    return;
	}
	push(derefStablePtr(hval))
	);
}

static Void FreeHVal(hval)      /* Free a Haskell value              */
HVal   hval;
{
    protect(freeStablePtr(hval));
}
#ifndef NO_DYNAMIC_TYPES
/* --------------------------------------------------------------------------
 * Testing for class membership:
 * ------------------------------------------------------------------------*/

static Cell getDictFor(c,t) /* Find a dictionary for instance t of c, or   */
Class c;                    /* NIL if none found                           */
Type  t; {
    Kinds ks = NIL;
    if (isPolyType(t)) {
	ks = polySigOf(t);
	t  = monotypeOf(t);
    }
    switch (whatIs(t)) {
	case QUAL  :
	case RANK2 :
	case EXIST :
	case CDICTS: return NIL;
    }
    return provePred(ks,NIL,ap(c,t));
}
#endif

/* ----------------------------------------------------------------------- */
