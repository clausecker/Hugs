/* --------------------------------------------------------------------------
 * Command interpreter
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: hugs.c,v $
 * $Revision: 1.35 $
 * $Date: 2001/02/14 12:15:05 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "command.h"
#include "connect.h"
#include "errors.h"
#include "version.h"
#include <setjmp.h>
#include <ctype.h>

#include <stdio.h>

#if HAVE_WINDOWS_H
#include <windows.h>
#endif

#if HUGS_FOR_WINDOWS
#include "winhugs\WinHugs.h"
#include "winhugs\WinUtils.h"
#endif

#if !HASKELL_98_ONLY
Bool haskell98 = TRUE;			/* TRUE => Haskell 98 compatibility*/
#endif

#if EXPLAIN_INSTANCE_RESOLUTION
Bool showInstRes = FALSE;
#endif
#if MULTI_INST
Bool multiInstRes = FALSE;
#endif

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void   local initialize        Args((Int,String []));
static Void   local promptForInput    Args((String));
static Void   local interpreter       Args((Int,String []));
static Void   local menu              Args((Void));
static Void   local guidance          Args((Void));
static Void   local forHelp           Args((Void));
static Void   local set               Args((Void));
static Void   local changeDir         Args((Void));
static Void   local load              Args((Void));
static Void   local project           Args((Void));
static Void   local readScripts       Args((Int));
static Void   local whatScripts       Args((Void));
static Void   local editor            Args((Void));
static Void   local find              Args((Void));
static Bool   local startEdit         Args((Int,String));
static Void   local runEditor         Args((Void));
#if IGNORE_MODULES
#define findEvalModule() doNothing()
#else
static Void   local setModule         Args((Void));
static Module local findEvalModule    Args((Void));
#endif
static Void   local evaluator         Args((Void));
static Void   local stopAnyPrinting   Args((Void));
static Void   local showtype          Args((Void));
static String local objToStr          Args((Module, Cell));
static Void   local info              Args((Void));
static Void   local printSyntax       Args((Name));
static Void   local showInst          Args((Inst));
static Void   local describe          Args((Text));
static Void   local listNames         Args((Void));
#if HUGS_FOR_WINDOWS
static Void   local autoReloadFiles   Args((Void));
#endif

static Void   local toggleSet         Args((Char,Bool));
static Void   local togglesIn         Args((Bool));
static Void   local optionInfo        Args((Void));
#if USE_REGISTRY || HUGS_FOR_WINDOWS
static String local optionsToStr      Args((Void));
#endif
static Void   local readOptions       Args((String));
static Bool   local processOption     Args((String));
static Void   local setHeapSize       Args((String));
static Int    local argToInt          Args((String));

static Void   local loadProject       Args((String));
static Void   local clearProject      Args((Void));
static Void   local addScriptName     Args((String,Bool));
static Bool   local addScript         Args((String,Long));
static Void   local forgetScriptsFrom Args((Script));
static Void   local setLastEdit       Args((String,Int));
static Void   local failed            Args((Void));
static String local strCopy           Args((String));
static Void   local browseit	      Args((Module,String,Bool));
static Void   local browse	          Args((Void));

#if USE_PREFERENCES_FILE
static Void    readPrefsFile          Args((FILE *));
typedef char GVarname[2000];
static GVarname hugsFlags = "";
int  iniArgc;
char iniArgv[10][33];
#endif


/* --------------------------------------------------------------------------
 * Machine dependent code for Hugs interpreter:
 * ------------------------------------------------------------------------*/

#include "machdep.c"
#ifdef WANT_TIMER
#include "timer.c"
#endif

/* --------------------------------------------------------------------------
 * Local data areas:
 * ------------------------------------------------------------------------*/

static Bool   printing     = FALSE;     /* TRUE => currently printing value*/
static Bool   showStats    = FALSE;     /* TRUE => print stats after eval  */
static Bool   listScripts  = TRUE;      /* TRUE => list scripts after loading*/
static Bool   addType      = FALSE;     /* TRUE => print type with value   */
static Bool   useShow      = TRUE;      /* TRUE => use Text/show printer   */
static Bool   chaseImports = TRUE;      /* TRUE => chase imports on load   */
static Bool   useDots      = RISCOS;    /* TRUE => use dots in progress    */
static Bool   quiet        = FALSE;     /* TRUE => don't show progress     */
#if HUGS_FOR_WINDOWS
static Bool autoLoadFiles  = TRUE;	/* TRUE => reload files before eval*/
static Bool InAutoReloadFiles = FALSE;	/* TRUE =>loading files before eval*/
#endif

static String scriptName[NUM_SCRIPTS];  /* Script file names               */
static String scriptReal[NUM_SCRIPTS];  /* Full path to canonical name     */
static Time   lastChange[NUM_SCRIPTS];  /* Time of last change to script   */
static Bool   postponed[NUM_SCRIPTS];   /* Indicates postponed load        */
static Int    numScripts;               /* Number of scripts loaded        */
static Int    namesUpto;                /* Number of script names set      */
static Bool   needsImports;             /* set to TRUE if imports required */
       String scriptFile;               /* Name of current script (if any) */

static Text   evalModule  = 0;          /* Name of module we eval exprs in */
static String currProject = 0;          /* Name of current project file    */
static Bool   projectLoaded = FALSE;    /* TRUE => project file loaded     */

static String lastEdit   = 0;           /* Name of script to edit (if any) */
static Int    lastLine   = 0;           /* Editor line number (if possible)*/
static String prompt     = 0;           /* Prompt string                   */
static Int    hpSize     = DEFAULTHEAP; /* Desired heap size               */
String hugsEdit		 = 0;		/* String for editor command       */
String hugsPath		 = 0;		/* String for file search path     */
String projectPath	 = 0;		/* String for project search path  */
Bool   preludeLoaded	 = FALSE;

#if __MWERKS__ && macintosh
#include <SIOUX.h>
#endif

#if REDIRECT_OUTPUT
static Bool disableOutput = FALSE;      /* redirect output to buffer?      */
#endif

/* --------------------------------------------------------------------------
 * Hugs entry point:
 * ------------------------------------------------------------------------*/

#ifndef NO_MAIN /* we omit main when building the "Hugs server" */
 
Main main Args((Int, String []));       /* now every func has a prototype  */

Main main(argc,argv)
int  argc;
char *argv[]; {

#if __MWERKS__ && macintosh
    strcpy(macHugsDir,currentDir());
    SIOUXSettings.autocloseonquit   = true;
    SIOUXSettings.asktosaveonclose  = false;
    SIOUXSettings.columns           = 80;
    SIOUXSettings.rows              = 40; 
    SIOUXSettings.tabspaces         = 8;
    SIOUXSettings.enabledraganddrop = true;
    SIOUXSetTitle("\pHugs 98");
#endif

    CStackBase = &argc;                 /* Save stack base for use in gc   */

#if HUGS_FOR_WINDOWS
    { 
    INT svColor;
    svColor = SetForeColor(BLUE);    Printf( "__   __ __  __  ____   ___");
                                     Printf("      _______________________________________________\n");
    SetForeColor(svColor);
    svColor = SetForeColor(RED);     Printf("||   || ||  || ||  || ||__ ");
    SetForeColor(svColor);           Printf("     Hugs 98: Based on the Haskell 98 standard\n");
    svColor = SetForeColor(BLUE);    Printf("||___|| ||__|| ||__||  __||");
    SetForeColor(svColor);           Printf("     Copyright (c) 1994-2001\n");
    svColor = SetForeColor(RED);     Printf("||---||         ___||      ");
    SetForeColor(svColor);           Printf("     World Wide Web: http://haskell.org/hugs\n");
    svColor = SetForeColor(BLUE);    Printf("||   ||                    ");
    SetForeColor(svColor);           Printf("     Report bugs to: hugs-bugs@haskell.org\n");
    svColor = SetForeColor(RED);     Printf("||   || ");
    SetForeColor(svColor);           Printf("Version: %s",HUGS_VERSION);
    svColor = SetForeColor(BLUE);    Printf(" _______________________________________________\n\n");
    SetForeColor(svColor);
    }
#else
    Printf("__   __ __  __  ____   ___      _________________________________________\n");
    Printf("||   || ||  || ||  || ||__      Hugs 98: Based on the Haskell 98 standard\n");
    Printf("||___|| ||__|| ||__||  __||     Copyright (c) 1994-2001\n");
    Printf("||---||         ___||           World Wide Web: http://haskell.org/hugs\n");
    Printf("||   ||                         Report bugs to: hugs-bugs@haskell.org\n");
    Printf("||   || Version: %-14s _________________________________________\n\n",HUGS_VERSION);
#endif

    FlushStdout();
    interpreter(argc,argv);
    Printf("[Leaving Hugs]\n");
#if HUGS_FOR_WINDOWS
    SaveGUIOptions();
#endif
    everybody(EXIT);
#if HUGS_FOR_WINDOWS
    return 0; /* return to Winmain */
#endif
    exit(0);
    MainDone();
}

#endif

/* --------------------------------------------------------------------------
 * Initialization, interpret command line args and read prelude:
 * ------------------------------------------------------------------------*/

static Void local initialize(argc,argv)/* Interpreter initialization       */
Int    argc;
String argv[]; {
    Script i;
    String proj = 0;
#if USE_PREFERENCES_FILE
    FILE *f;
    FileName hugsPrefsFile = "\0";
#endif

    setLastEdit((String)0,0);
    lastEdit      = 0;
    scriptFile    = 0;
    numScripts    = 0;
    namesUpto     = 1;

#if HUGS_FOR_WINDOWS || HAVE_WINDOWS_H
#define DEFAULT_EDITOR "\\notepad.exe"
    /*
     * Check first to see if the user has explicitly defined
     * an editor via the environment variable EDITOR..
     */
    hugsEdit      = strCopy(fromEnv("EDITOR",NULL));
    if (hugsEdit == NULL) {
      UINT rc;
      int notePadLen = strlen(DEFAULT_EDITOR);
      char* notePadLoc;
      /*
       * Nope, the default editor is used instead. In our case
       * this is 'notepad', which we assume is always residing
       * in the windows directory, so locate it first..
       */
      
      notePadLoc = _alloca(sizeof(char)*(MAX_PATH + notePadLen + 1));
      rc = GetWindowsDirectory(notePadLoc, MAX_PATH);
      if ( !(rc == 0 || rc > MAX_PATH) ) {
	strcat(notePadLoc, DEFAULT_EDITOR);
	hugsEdit = strCopy(notePadLoc);
      }
    }
#elif __MWERKS__ && macintosh
    hugsEdit      = "";
#else
    hugsEdit      = strCopy(fromEnv("EDITOR",NULL));
#endif
    hugsPath      = strCopy(HUGSPATH);
    readOptions("-p\"%s> \" -r$$");
#if USE_REGISTRY
    projectPath   = strCopy(readRegChildStrings(HKEY_LOCAL_MACHINE,ProjectRoot,
					        "HUGSPATH", PATHSEP, ""));
    readOptions(readRegString(HKEY_LOCAL_MACHINE,HugsRoot,"Options",""));
    readOptions(readRegString(HKEY_CURRENT_USER, HugsRoot,"Options",""));
#endif /* USE_REGISTRY */
#if USE_PREFERENCES_FILE
    if (f=fopen(PREFS_FILE_NAME,"r")) { /* is preferences file in the {Current} folder? */
	  readPrefsFile(f);
	} else {                               /* is preferences file in the {Hugs} folder? */
        strcpy(hugsPrefsFile,macHugsDir);
        strcat(hugsPrefsFile,":");
        strcat(hugsPrefsFile,PREFS_FILE_NAME);
        if (f=fopen(hugsPrefsFile,"r"))
          readPrefsFile(f);
	  }                                              /* else: take default preferences */
    readOptions(hugsFlags);
    if (iniArgc > 0)            /* load additional files found in the preferences file */
	  for (i=0; i<iniArgc; i++) {
	    addScriptName(iniArgv[i],TRUE);
      }
#else
# if HUGS_FOR_WINDOWS
    ReadGUIOptions();
# endif
    readOptions(fromEnv("HUGSFLAGS",""));
#endif

    for (i=1; i<argc; ++i) {            /* process command line arguments  */
	if (strcmp(argv[i],"+")==0 && i+1<argc) {
	    if (proj) {
		ERRMSG(0) "Multiple project filenames on command line"
		EEND;
	    } else {
		proj = argv[++i];
	    }
	} else if (argv[i] && argv[i][0]/* workaround for /bin/sh silliness*/
		 && !processOption(argv[i])) {
#if HUGS_FOR_WINDOWS
	    {addScriptName(argv[i],TRUE); SetWorkingDir(argv[i]); }
#else
	    addScriptName(argv[i],TRUE);
#endif
	}
    }
    scriptName[0] = strCopy(findMPathname(NULL,STD_PRELUDE,hugsPath));
    if (!scriptName[0]) {
	Printf("Prelude not found on current path: \"%s\"\n",
	       hugsPath ? hugsPath : "");
	fatal("Unable to load prelude");
    }
    scriptReal[0] = strCopy(RealPath(scriptName[0]));

#if !HASKELL_98_ONLY
    if (haskell98) {
	Printf("Haskell 98 mode: Restart with command line option -98 to enable extensions\n\n");
    } else {
	Printf("Hugs mode: Restart with command line option +98 for Haskell 98 mode\n\n");
    }
#endif

    everybody(INSTALL);
    evalModule = findText("");      /* evaluate wrt last module by default */
    if (proj) {
	if (namesUpto>1) {
	    fprintf(stderr,
		    "\nUsing project file, ignoring additional filenames\n");
	}
	loadProject(strCopy(proj));
    }
    readScripts(0);
}

#if USE_PREFERENCES_FILE
static Void readPrefsFile(FILE *f)
{ GVarname line  = "";
  int      linep = 0;

  char c;
      
  while ( (c=fgetc(f)) != EOF && c != '\n') {     /* read HUGSFLAGS          */
    if ((c != '\t') && (c != '\r')) {             /* skip some control chars */
      line[linep++] = c;
      line[linep]   = '\0';
    }
  }
  strcpy(hugsFlags,line);
    
  iniArgc = 0;
  do  {                                  /* read input command line files   */
    while ((c == '\n') || (c == '\t') || (c == ' '))  /* skip blank spaces  */
      c=fgetc(f);    
    if (c == '"') {                      /* filename found                  */
      linep = 0;
      iniArgv[iniArgc][0] = '\0';
      while ((c=fgetc(f)) != EOF && c != '"') {
        if (linep <= 32) {              /* filename limit on a mac 32 chars */
          iniArgv[iniArgc][linep++] = c;
          iniArgv[iniArgc][linep]   = '\0';
        }
      }
      if (c == EOF) {
        ERRMSG(0) "Incorrect name specification in preferences file"
		EEND;
      } else {
		  iniArgc++;
	    }
    }
  } while ( (c = fgetc(f)) != EOF );
}
#endif

/* --------------------------------------------------------------------------
 * Command line options:
 * ------------------------------------------------------------------------*/

struct options {                        /* command line option toggles     */
    char   c;                           /* table defined in main app.      */
#if !HASKELL_98_ONLY
    int    h98;                         /* set in Haskell'98 mode?         */
#endif
    String description;
    Bool   *flag;
};
extern struct options toggle[];

static Void local toggleSet(c,state)    /* Set command line toggle         */
Char c;
Bool state; {
    Int i;
    for (i=0; toggle[i].c; ++i)
	if (toggle[i].c == c) {
	    *toggle[i].flag = state;
	    return;
	}
    ERRMSG(0) "Unknown toggle `%c'", c
    EEND;
}

static Void local togglesIn(state)      /* Print current list of toggles in*/
Bool state; {                           /* given state                     */
    Int count = 0;
    Int i;
    for (i=0; toggle[i].c; ++i)
#if HASKELL_98_ONLY
	if (*toggle[i].flag == state) {
#else
	if (*toggle[i].flag == state && (!haskell98 || toggle[i].h98)) {
#endif        
	    if (count==0)
		Putchar((char)(state ? '+' : '-'));
	    Putchar(toggle[i].c);
	    count++;
	}
    if (count>0)
	Putchar(' ');
}

static Void local optionInfo() {        /* Print information about command */
    static String fmts = "%-5s%s\n";    /* line settings                   */
    static String fmtc = "%-5c%s\n";
    Int    i;

    Printf("TOGGLES: groups begin with +/- to turn options on/off resp.\n");
    for (i=0; toggle[i].c; ++i) {
#if !HASKELL_98_ONLY
	if (!haskell98 || toggle[i].h98) {
#endif
	    Printf(fmtc,toggle[i].c,toggle[i].description);
#if !HASKELL_98_ONLY
	}
#endif
    }

    Printf("\nOTHER OPTIONS: (leading + or - makes no difference)\n");
    Printf(fmts,"hnum","Set heap size (cannot be changed within Hugs)");
    Printf(fmts,"pstr","Set prompt string to str");
    Printf(fmts,"rstr","Set repeat last expression string to str");
    Printf(fmts,"Pstr","Set search path for modules to str");
    Printf(fmts,"Estr","Use editor setting given by str");
    Printf(fmts,"cnum","Set constraint cutoff limit");
#if USE_PREPROCESSOR  && (defined(HAVE_POPEN) || defined(HAVE__POPEN))
    Printf(fmts,"Fstr","Set preprocessor filter to str");
#endif
#if PROFILING
    Printf(fmts,"dnum","Gather profiling statistics every <num> reductions\n");
#endif

    Printf("\nCurrent settings: ");
    togglesIn(TRUE);
    togglesIn(FALSE);
    Printf("-h%d",heapSize);
    Printf(" -p");
    printString(prompt);
    Printf(" -r");
    printString(repeatStr);
    Printf(" -c%d",cutoff);
    Printf("\nSearch path     : -P");
    printString(hugsPath);
#if __MWERKS__ && macintosh
    Printf("\n{Hugs}          : %s",hugsdir());
    Printf("\n{Current}       : %s",currentDir());
#endif
    if (projectPath!=NULL) {
	Printf("\nProject Path    : %s",projectPath);
    }
    Printf("\nEditor setting  : -E");
    printString(hugsEdit);
#if USE_PREPROCESSOR  && (defined(HAVE_POPEN) || defined(HAVE__POPEN))
    Printf("\nPreprocessor    : -F");
    printString(preprocessor);
#endif
#if PROFILING
    Printf("\nProfile interval: -d%d", profiling ? profInterval : 0);
#endif
#if HASKELL_98_ONLY
    Printf("\nCompatibility   : Haskell 98");
#else
    Printf("\nCompatibility   : %s", haskell98 ? "Haskell 98 (+98)"
					       : "Hugs Extensions (-98)");
#endif
    Putchar('\n');
}

#if USE_REGISTRY || HUGS_FOR_WINDOWS
#define PUTC(c)                         \
    *next++=(c)

#define PUTS(s)                         \
    strcpy(next,s);                     \
    next+=strlen(next)

#define PUTInt(optc,i)                  \
    sprintf(next,"-%c%d",optc,i);       \
    next+=strlen(next)

#define PUTStr(c,s)                     \
    next=PUTStr_aux(next,c,s)

static String local PUTStr_aux Args((String,Char, String));

static String local PUTStr_aux(next,c,s)
String next;
Char   c;
String s; {
    if (s) { 
	String t = 0;
	sprintf(next,"-%c\"",c); 
	next+=strlen(next);      
	for(t=s; *t; ++t) {
	    PUTS(unlexChar(*t,'"'));
	}
	next+=strlen(next);      
	PUTS("\" ");
    }
    return next;
}

static String local optionsToStr() {          /* convert options to string */
    static char buffer[2000];
    String next = buffer;

    Int i;
    for (i=0; toggle[i].c; ++i) {
	PUTC(*toggle[i].flag ? '+' : '-');
	PUTC(toggle[i].c);
	PUTC(' ');
    }
#if !HASKELL_98_ONLY
    PUTS(haskell98 ? "+98 " : "-98 ");
#endif
    PUTInt('h',hpSize);  PUTC(' ');
    PUTStr('p',prompt);
    PUTStr('r',repeatStr);
    PUTStr('P',hugsPath);
    PUTStr('E',hugsEdit);
    PUTInt('c',cutoff);  PUTC(' ');
#if USE_PREPROCESSOR  && (defined(HAVE_POPEN) || defined(HAVE__POPEN))
    PUTStr('F',preprocessor);
#endif
#if PROFILING
    PUTInt('d',profiling ? profInterval : 0);
#endif
    PUTC('\0');
    return buffer;
}
#endif /* USE_REGISTRY */

#undef PUTC
#undef PUTS
#undef PUTInt
#undef PUTStr

static Void local readOptions(options)         /* read options from string */
String options; {
    String s;
    if (options) {
	stringInput(options);
	while ((s=readFilename())!=0) {
	    if (*s && !processOption(s)) {
		ERRMSG(0) "Option string must begin with `+' or `-'"
		EEND;
	    }
	}
    }
}

static Bool local processOption(s)      /* process string s for options,   */
String s; {                             /* return FALSE if none found.     */
    Bool state;

    if (s[0]=='-')
	state = FALSE;
    else if (s[0]=='+')
	state = TRUE;
    else
	return FALSE;

    while (*++s)
	switch (*s) {
	    case 'p' : if (s[1]) {
			   if (prompt) free(prompt);
			   prompt = strCopy(s+1);
		       }
		       return TRUE;

	    case 'r' : if (s[1]) {
			   if (repeatStr) free(repeatStr);
			   repeatStr = strCopy(s+1);
		       }
		       return TRUE;

#if PROFILING
	    case 'd' : {                /* random choice of letter - ADR   */
			   Int i = argToInt(s+1);
			   if (i > 0) {
			       profiling = TRUE;
			       profInterval = i;
			   } else {
			       profiling = FALSE;
			       /* To keep the profiling test efficient(?)
				* we dont actually disable the gathering
				* of profiling statistics - we just gather
				* them very infrequently. ADR
				*/
			       profInterval = MAXPOSINT;
			   }
		       }
		       return TRUE;
#endif

	    case 'P' : {
			   String p = substPath(s+1,hugsPath ? hugsPath : "");
			   if (hugsPath) free(hugsPath);
			   hugsPath = p;
			   return TRUE;
		       }

	    case 'E' : if (hugsEdit) free(hugsEdit);
		       hugsEdit = strCopy(s+1);
		       return TRUE;

#if USE_PREPROCESSOR  && (defined(HAVE_POPEN) || defined(HAVE__POPEN))
	    case 'F' : if (preprocessor) free(preprocessor);
		       preprocessor = strCopy(s+1);
		       return TRUE;
#endif

	    case 'h' : setHeapSize(s+1);
		       return TRUE;

	    case 'c' : {   Int cutcand = argToInt(s+1);
			   if (cutcand>=1 && cutcand<=1024)
			       cutoff = cutcand;
		       }
		       return TRUE;
        default  :
#if !HASKELL_98_ONLY
	           if (strcmp("98",s)==0) {
			   if (heapBuilt() && ((state && !haskell98) ||
					       (!state && haskell98))) {
			       FPrintf(stderr,"Haskell 98 compatibility cannot be changed while the interpreter is running\n");
			   } else {
			       haskell98 = state;
			   }
			   return TRUE;
		       } else {
#endif
			   toggleSet(*s,state);
#if !HASKELL_98_ONLY
               }
#endif
		       break;
	}
    return TRUE;
}

static Void local setHeapSize(s) 
String s; {
    if (s) {
	hpSize = argToInt(s);
	if (hpSize < MINIMUMHEAP)
	    hpSize = MINIMUMHEAP;
	else if (MAXIMUMHEAP && hpSize > MAXIMUMHEAP)
	    hpSize = MAXIMUMHEAP;
	if (heapBuilt() && hpSize != heapSize) {
#if HUGS_FOR_WINDOWS
            MessageBox(hWndMain, "Change to heap size will not take effect until you rerun Hugs", appName, MB_ICONHAND | MB_OK);	    
#endif            
#if USE_REGISTRY
	    FPrintf(stderr,"Change to heap size will not take effect until you rerun Hugs\n");
#else
	    FPrintf(stderr,"Cannot change heap size\n");
#endif
	} else {
	    heapSize = hpSize;
	}
    }
}

static Int local argToInt(s)            /* read integer from argument str  */
String s; {
    Int    n = 0;
    String t = s;

    if (*s=='\0' || !isascii(*s) || !isdigit(*s)) {
	ERRMSG(0) "Missing integer in option setting \"%s\"", t
	EEND;
    }

    do {
	Int d = (*s++) - '0';
	if (n > ((MAXPOSINT - d)/10)) {
	    ERRMSG(0) "Option setting \"%s\" is too large", t
	    EEND;
	}
	n     = 10*n + d;
    } while (isascii(*s) && isdigit(*s));

    if (*s=='K' || *s=='k') {
	if (n > (MAXPOSINT/1000)) {
	    ERRMSG(0) "Option setting \"%s\" is too large", t
	    EEND;
	}
	n *= 1000;
	s++;
    }

#if MAXPOSINT > 1000000                 /* waste of time on 16 bit systems */
    if (*s=='M' || *s=='m') {
	if (n > (MAXPOSINT/1000000)) {
	    ERRMSG(0) "Option setting \"%s\" is too large", t
	    EEND;
	}
	n *= 1000000;
	s++;
    }
#endif

#if MAXPOSINT > 1000000000
    if (*s=='G' || *s=='g') {
	if (n > (MAXPOSINT/1000000000)) {
	    ERRMSG(0) "Option setting \"%s\" is too large", t
	    EEND;
	}
	n *= 1000000000;
	s++;
    }
#endif

    if (*s!='\0') {
	ERRMSG(0) "Unwanted characters after option setting \"%s\"", t
	EEND;
    }

    return n;
}

/* --------------------------------------------------------------------------
 * Print Menu of list of commands:
 * ------------------------------------------------------------------------*/

static struct cmd cmds[] = {
 {":?",      HELP},   {":cd",   CHGDIR},  {":also",    ALSO},
 {":type",   TYPEOF}, {":!",    SYSTEM},  {":load",    LOAD},
 {":reload", RELOAD}, {":gc",   COLLECT}, {":edit",    EDIT},
 {":quit",   QUIT},   {":set",  SET},     {":find",    FIND},
 {":names",  NAMES},  {":info", INFO},    {":project", PROJECT},
#if !IGNORE_MODULES
 {":module",SETMODULE}, 
#endif
 {":browse", BROWSE},
#if EXPLAIN_INSTANCE_RESOLUTION
 {":xplain", XPLAIN},
#endif
 {":version", PNTVER},
#ifdef __SYMBIAN32__
 {":Pwd",PRNDIR},
#endif
 {"",      EVAL},
 {0,0}
};

static Void local menu() {
    Printf("LIST OF COMMANDS:  Any command may be abbreviated to :c where\n");
    Printf("c is the first character in the full name.\n\n");
    Printf(":load <filenames>   load modules from specified files\n");
    Printf(":load               clear all files except prelude\n");
    Printf(":also <filenames>   read additional modules\n");
    Printf(":reload             repeat last load command\n");
    Printf(":project <filename> use project file\n");
    Printf(":edit <filename>    edit file\n");
    Printf(":edit               edit last module\n");
#if !IGNORE_MODULES
    Printf(":module <module>    set module for evaluating expressions\n");
#endif
    Printf("<expr>              evaluate expression\n");
    Printf(":type <expr>        print type of expression\n");
    Printf(":?                  display this list of commands\n");
    Printf(":set <options>      set command line options\n");
    Printf(":set                help on command line options\n");
    Printf(":names [pat]        list names currently in scope\n");
    Printf(":info <names>       describe named objects\n");
    Printf(":browse <modules>   browse names defined in <modules>\n");
#if EXPLAIN_INSTANCE_RESOLUTION
    Printf(":xplain <context>   explain instance resolution for <context>\n");
#endif
    Printf(":find <name>        edit module containing definition of name\n");
    Printf(":!command           shell escape\n");
    Printf(":cd dir             change directory\n");
    Printf(":gc                 force garbage collection\n");
#ifdef __SYMBIAN32__
    Printf(":Pwd                print working directory\n");
#endif
    Printf(":version            print Hugs version\n");
    Printf(":quit               exit Hugs interpreter\n");
}

static Void local guidance() {
    Printf("Command not recognised.  ");
    forHelp();
}

static Void local forHelp() {
    Printf("Type :? for help\n");
}

/* --------------------------------------------------------------------------
 * Setting of command line options:
 * ------------------------------------------------------------------------*/

struct options toggle[] = {             /* List of command line toggles    */
    {'s',
#if !HASKELL_98_ONLY
             1,
#endif
             "Print no. reductions/cells after eval", &showStats},
    {'t',
#if !HASKELL_98_ONLY
             1,
#endif
             "Print type after evaluation",           &addType},
    {'f',
#if !HASKELL_98_ONLY
             1,
#endif
             "Terminate evaluation on first error",   &failOnError},
    {'g',
#if !HASKELL_98_ONLY
             1,
#endif
             "Print no. cells recovered after gc",    &gcMessages},
    {'G', 
#if !HASKELL_98_ONLY
             0, 
#endif
             "Generate FFI code for foreign import",  &generate_ffi},
    {'l',
#if !HASKELL_98_ONLY
             1, 
#endif
             "Literate modules as default",           &literateScripts},
    {'e',
#if !HASKELL_98_ONLY
             1,
#endif
             "Warn about errors in literate modules", &literateErrors},
    {'.',
#if !HASKELL_98_ONLY
             1,
#endif
             "Print dots to show progress",           &useDots},
    {'q',
#if !HASKELL_98_ONLY
             1,
#endif
             "Print nothing to show progress",        &quiet},
    {'Q',
#if !HASKELL_98_ONLY
             1,
#endif
	     "Qualify names when printing",           &useQualifiedNames},
    {'w',
#if !HASKELL_98_ONLY
             1,
#endif
             "Always show which modules are loaded",  &listScripts},
    {'k',
#if !HASKELL_98_ONLY
             1,
#endif
             "Show kind errors in full",              &kindExpert},
    {'o',
#if !HASKELL_98_ONLY
             0,
#endif
             "Allow overlapping instances",           &allowOverlap},
    {'u',
#if !HASKELL_98_ONLY
             1,
#endif
             "Use \"show\" to display results",       &useShow},
    {'i',
#if !HASKELL_98_ONLY
             1,
#endif
             "Chase imports while loading modules",   &chaseImports},
#if HUGS_FOR_WINDOWS
    {'A', 
#if !HASKELL_98_ONLY
             1, 
#endif
             "Auto load files",		   	      &autoLoadFiles},
#endif
#if EXPLAIN_INSTANCE_RESOLUTION
    {'x',   
#if !HASKELL_98_ONLY
             1,
#endif
             "Explain instance resolution",           &showInstRes},
#endif
#if MULTI_INST
    {'m', 0, "Use multi instance resolution",         &multiInstRes},
#endif
#if DEBUG_CODE
    {'D',
#if !HASKELL_98_ONLY
          1,
#endif
          "Debug: show generated G code",          &debugCode},
#endif
#if DEBUG_SHOWSC
    {'S',
#if !HASKELL_98_ONLY
          1,
#endif
          "Debug: show generated SC code",         &debugSC},
#endif
#if OBSERVATIONS
    {'R',
#if !HASKELL_98_ONLY
          1,
#endif
          "Enable root optimisation",         &rootOpt},
#endif
    {0,   
#if !HASKELL_98_ONLY
          0,
#endif
          0}
};

static Void local set() {               /* change command line options from*/
    String s;                           /* Hugs command line               */

    if ((s=readFilename())!=0) {
	do {
	    if (!processOption(s)) {
		ERRMSG(0) "Option string must begin with `+' or `-'"
		EEND;
	    }
	} while ((s=readFilename())!=0);
#if USE_REGISTRY
	writeRegString("Options", optionsToStr());
#endif
    }
    else
	optionInfo();
}

/* --------------------------------------------------------------------------
 * Change directory command:
 * ------------------------------------------------------------------------*/

static Void local changeDir() {         /* change directory                */
    String s = readFilename();
    if (s && chdir(s)) {
	ERRMSG(0) "Unable to change to directory \"%s\"", s
	EEND;
    }
}

#ifdef __SYMBIAN32__
/* --------------------------------------------------------------------------
 * Print working directory command:
 * ------------------------------------------------------------------------*/

static Void local printDir() {         /* print directory                */
    char s[256];
    printf("%s\n",getcwd(s,255));
}
#endif

/* --------------------------------------------------------------------------
 * Loading project and script files:
 * ------------------------------------------------------------------------*/

static Void local loadProject(s)        /* Load project file               */
String s; {
    clearProject();
    currProject = s;
    projInput(currProject);
    scriptFile = currProject;
    forgetScriptsFrom(1);
    while ((s=readFilename())!=0)
	addScriptName(s,TRUE);
    if (namesUpto<=1) {
	ERRMSG(0) "Empty project file"
	EEND;
    }
    scriptFile    = 0;
    projectLoaded = TRUE;
}

static Void local clearProject() {      /* clear name for current project  */
    if (currProject)
	free(currProject);
    currProject   = 0;
    projectLoaded = FALSE;
#if HUGS_FOR_WINDOWS
    setLastEdit((String)0,0);
#endif
}

static Void local addScriptName(s,sch)  /* Add script to list of scripts   */
String s;                               /* to be read in ...               */
Bool   sch; {                           /* TRUE => requires pathname search*/
    if (namesUpto>=NUM_SCRIPTS) {
	ERRMSG(0) "Too many module files (maximum of %d allowed)",
		  NUM_SCRIPTS
	EEND;
    }
    else
	scriptName[namesUpto] = strCopy(sch ? findPathname(NULL,s) : s);
	scriptReal[namesUpto] = strCopy(RealPath(scriptName[namesUpto]));
	namesUpto++;
}

static Bool local addScript(fname,len)  /* read single script file
*/
String fname;                           /* name of script file
*/
Long   len; {                           /* length of script file
*/
    scriptFile = fname;

#if HUGS_FOR_WINDOWS         /* Set clock cursor while loading   */
    allowBreak();
    SetCursor(LoadCursor(NULL, IDC_WAIT));
    AddFileToFileNamesMenu(&FilesMenu, RealPath(fname));
#endif

    Printf("Reading file \"%s\":\n",fname);
    setLastEdit(fname,0);

    needsImports = FALSE;
    parseScript(fname,len);             /* process script file
*/
    if (needsImports)
 return FALSE;
    checkDefns();
    typeCheckDefns();
    compileDefns();
    scriptFile    = 0;
    preludeLoaded = TRUE;
    return TRUE;
}

Bool chase(imps)                        /* Process list of import requests */
List imps; {
    if (chaseImports) {
	Int    origPos  = numScripts;   /* keep track of original position */
	String origName = scriptName[origPos];
	for (; nonNull(imps); imps=tl(imps)) {
	    String iname = findPathname(origName,textToStr(textOf(hd(imps))));
	    String rname = RealPath(iname);
	    Int    i     = 0;
	    for (; i<namesUpto; i++)
		if (filenamecmp(scriptReal[i],rname)==0)
		    break;
	    if (i>=origPos) {           /* Neither loaded or queued        */
		String theName;
		String theReal;
		Time   theTime;
		Bool   thePost;

		postponed[origPos] = TRUE;
		needsImports       = TRUE;

		if (i>=namesUpto)       /* Name not found (i==namesUpto)   */
		    addScriptName(iname,FALSE);
		else if (postponed[i]) {/* Check for recursive dependency  */
		    ERRMSG(0)
		      "Recursive import dependency between \"%s\" and \"%s\"",
		      scriptName[origPos], iname
		    EEND;
		}
		/* Right rotate section of tables between numScripts and i so
		 * that i ends up with other imports in front of orig. script
		 */
		theName = scriptName[i];
		theReal = scriptReal[i];
		thePost = postponed[i];
		timeSet(theTime,lastChange[i]);
		for (; i>numScripts; i--) {
		    scriptName[i] = scriptName[i-1];
		    scriptReal[i] = scriptReal[i-1];
		    postponed[i]  = postponed[i-1];
		    timeSet(lastChange[i],lastChange[i-1]);
		}
		scriptName[numScripts] = theName;
		scriptReal[numScripts] = theReal;
		postponed[numScripts]  = thePost;
		timeSet(lastChange[numScripts],theTime);
		origPos++;
	    }
	}
	return needsImports;
    }
    return FALSE;
}

static Void local forgetScriptsFrom(scno)/* remove scripts from system     */
Script scno; {
    Script i;
    for (i=scno; i<namesUpto; ++i)
	if (scriptName[i])
	    free(scriptName[i]);
    dropScriptsFrom(scno-1);             /* don't count prelude as script  */
    namesUpto = scno;
    if (numScripts>namesUpto)
	numScripts = scno;
}

/* --------------------------------------------------------------------------
 * Commands for loading and removing script files:
 * ------------------------------------------------------------------------*/

static Void local load() {           /* read filenames from command line   */
    String s;                        /* and add to list of scripts waiting */
				     /* to be read                         */
    while ((s=readFilename())!=0)
	addScriptName(s,TRUE);
    readScripts(1);
}

static Void local project() {          /* read list of script names from   */
    String s;                          /* project file                     */

    if ((s=readFilename()) || currProject) {
	if (!s)
	    s = strCopy(currProject);
	else if (readFilename()) {
	    ERRMSG(0) "Too many project files"
	    EEND;
	}
	else
	    s = strCopy(s);
    }
    else {
	ERRMSG(0) "No project filename specified"
	EEND;
    }
    loadProject(s);
    readScripts(1);
}

static Void local readScripts(n)        /* Reread current list of scripts, */
Int n; {                                /* loading everything after and    */
    Time timeStamp;                     /* including the first script which*/
    Long fileSize;                      /* has been either changed or added*/

#if HUGS_FOR_WINDOWS
    SetCursor(LoadCursor(NULL, IDC_WAIT));
#endif

    for (; n<numScripts; n++) {         /* Scan previously loaded scripts  */
	getFileInfo(scriptName[n], &timeStamp, &fileSize);
	if (timeChanged(timeStamp,lastChange[n])) {
	    dropScriptsFrom(n-1);
	    numScripts = n;
	    break;
	}
    }
    for (; n<NUM_SCRIPTS; n++)          /* No scripts have been postponed  */
	postponed[n] = FALSE;           /* at this stage                   */

    while (numScripts<namesUpto) {      /* Process any remaining scripts   */
	getFileInfo(scriptName[numScripts], &timeStamp, &fileSize);
	timeSet(lastChange[numScripts],timeStamp);
	if (numScripts>0)               /* no new script for prelude       */
	    startNewScript(scriptName[numScripts]);
	if (addScript(scriptName[numScripts],fileSize))
	    numScripts++;
	else
	    dropScriptsFrom(numScripts-1);
    }

    if (listScripts)
	whatScripts();
    if (numScripts<=1)
	setLastEdit((String)0, 0);
}

static Void local whatScripts() {       /* list scripts in current session */
    int i;
#if HUGS_FOR_WINDOWS
    if (!InAutoReloadFiles) {
#endif
    Printf("\nHugs session for:");
    if (projectLoaded)
	Printf(" (project: %s)",currProject);
    for (i=0; i<numScripts; ++i)
	Printf("\n%s",scriptName[i]);
    Putchar('\n');
#if HUGS_FOR_WINDOWS
    }
#endif
}

/* --------------------------------------------------------------------------
 * Access to external editor:
 * ------------------------------------------------------------------------*/

static Void local editor() {            /* interpreter-editor interface    */
    String newFile  = readFilename();
    if (newFile) {
	setLastEdit(newFile,0);
	if (readFilename()) {
	    ERRMSG(0) "Multiple filenames not permitted"
	    EEND;
	}
    }
    runEditor();
}

static Void local find() {              /* edit file containing definition */
    String nm = readFilename();         /* of specified name               */
    if (!nm) {
	ERRMSG(0) "No name specified"
	EEND;
    }
    else if (readFilename()) {
	ERRMSG(0) "Multiple names not permitted"
	EEND;
    }
    else {
	Text t;
	Cell c;
	setCurrModule(findEvalModule());
	startNewScript(0);
	if (nonNull(c=findTycon(t=findText(nm)))) {
	    if (startEdit(tycon(c).line,scriptName[scriptThisTycon(c)])) {
		readScripts(1);
	    }
	} else if (nonNull(c=findName(t))) {
	    if (startEdit(name(c).line,scriptName[scriptThisName(c)])) {
		readScripts(1);
	    }
	} else {
	    ERRMSG(0) "No current definition for name \"%s\"", nm
	    EEND;
	}
    }
}

static Void local runEditor() {         /* run editor on script lastEdit   */
    if (startEdit(lastLine,lastEdit))   /* at line lastLine                */
	readScripts(1);
}

static Void local setLastEdit(fname,line)/* keep name of last file to edit
*/
String fname;
Int    line; {
    if (lastEdit)
 free(lastEdit);
    lastEdit = strCopy(fname);
    lastLine = line;
#if HUGS_FOR_WINDOWS
    /* Add file to Edit menu */
    if (lastEdit)
      AddFileToFileNamesMenu(&EditMenu, RealPath(lastEdit));
#endif
}

/* --------------------------------------------------------------------------
 * Read and evaluate an expression:
 * ------------------------------------------------------------------------*/

#if !IGNORE_MODULES
static Void local setModule(){/*set module in which to evaluate expressions*/
    String s = readFilename();
    if (s!=0) {			/* Locate named module			   */
	Text    t = findText(s);
	Module  m = findModule(t);
	if (isNull(m)) {
	    ERRMSG(0) "Cannot find module \"%s\"", s
	    EEND;
	}
	else {
	    evalModule = t;
	    setLastEdit(fileOfModule(m),0);
	}
    }
    else {			/* :m clears the current module selection */
	evalModule = findText("");
	setLastEdit(fileOfModule(lastModule()),0);
    }
}

static Module local findEvalModule() { /*Module in which to eval expressions*/
    Module m = findModule(evalModule); 
    if (isNull(m))
	m = lastModule();
    return m;
}
#endif

static Void local evaluator() {        /* evaluate expr and print value    */
    Type  type, bd, t;
    Kinds ks   = NIL;
    Cell  temp = NIL;

    setCurrModule(findEvalModule());
    scriptFile = 0;
    startNewScript(0);                 /* Enables recovery of storage      */
				       /* allocated during evaluation      */
    parseExp();
    checkExp();
    defaultDefns = evalDefaults;
    type         = typeCheckExp(TRUE);
    if (isPolyType(type)) {
	ks = polySigOf(type);
	bd = monotypeOf(type);
    }
    else
	bd = type;

    if (whatIs(bd)==QUAL) {
	ERRMSG(0) "Unresolved overloading" ETHEN
	ERRTEXT   "\n*** Type       : "    ETHEN ERRTYPE(type);
	ERRTEXT   "\n*** Expression : "    ETHEN ERREXPR(inputExpr);
	ERRTEXT   "\n"
	EEND;
    }
  
#if PROFILING
    if (profiling)
	profilerLog("profile.hp");
    numReductions = 0;
    garbageCollect();
#endif

#ifdef WANT_TIMER
    updateTimers();
#endif
#if IO_MONAD
    if (t = getProgType(ks,type)) {
	Cell printer = namePrint;
	if (useShow) {
	    Cell d = resolvePred(ks,ap(classShow,t));
	    if (isNull(d)) {
		printing = FALSE;
		ERRMSG(0) "Cannot find \"show\" function for IO result:" ETHEN
		ERRTEXT   "\n*** Expression : "   ETHEN ERREXPR(inputExpr);
		ERRTEXT   "\n*** Of type    : "   ETHEN ERRTYPE(type);
		ERRTEXT   "\n"
		EEND;
	    }
	    printer = ap(nameShowsPrec,d);
	}
	printer = ap(ap(nameFlip,ap(printer,mkInt(MIN_PREC))),nameNil);
	printer = ap(ap(nameComp,namePutStr),printer);
	if (t != typeUnit) {
	  inputExpr = ap(ap(nameIOBind,inputExpr),printer);
	}
    }
    else
#endif
    {   Cell printer = namePrint;
	if (useShow) {
	    Cell d = resolvePred(ks,ap(classShow,bd));
	    if (isNull(d)) {
		printing = FALSE;
		ERRMSG(0) "Cannot find \"show\" function for:" ETHEN
		ERRTEXT   "\n*** Expression : "   ETHEN ERREXPR(inputExpr);
		ERRTEXT   "\n*** Of type    : "   ETHEN ERRTYPE(type);
		ERRTEXT   "\n"
		EEND;
	    }
	    printer = ap(nameShowsPrec,d);
	}
	inputExpr = ap(ap(ap(printer,mkInt(MIN_PREC)),inputExpr),nameNil);
	inputExpr = ap(namePutStr,inputExpr);
    }
    inputExpr = ap(nameIORun,inputExpr);
    compileExp();                       
    clearStack();
    run(inputCode,sp);  /* Build graph for redex */
#if DEBUG_CODE
    if (debugCode) {
	Printf("evaluator() builds: ");
	printExp(stdout,top());
	Putchar('\n');
    }
#endif
    numCells      = 0;
    numReductions = 0;
    numGcs        = 0;
    printing      = TRUE;
#if OBSERVATIONS
    appNum        = 0;
    obsCount      = 0;
    clearAllBreak();
    clearObserve();
#endif
#if 1 /* Arguably not Haskell 1.4 compliant */
    noechoTerminal();
#endif
    consGC = FALSE;
    if (nonNull(type) && addType) {
	onto(NIL);
	pushed(0) = pushed(1);
	pushed(1) = type;
	if (nonNull(temp = evalWithNoError(pop()))) {
	    abandon("Program execution",temp);
	}
	drop();
#if HUGS_FOR_WINDOWS
	{ INT svColor = SetForeColor(BLUE);
#endif
	Printf(" :: ");
	printType(stdout,pop());
#if HUGS_FOR_WINDOWS
	SetForeColor(svColor); }
#endif
    }
    else {
	if (nonNull(temp = evalWithNoError(pop()))) {
	    abandon("Program execution",temp);
	}
    }
    stopAnyPrinting();
}

static Void local stopAnyPrinting() {  /* terminate printing of expression,*/
    if (printing) {                    /* after successful termination or  */
	printing = FALSE;              /* runtime error (e.g. interrupt)   */
	Putchar('\n');
	if (showStats) {
#define plural(v)   v, (v==1?"":"s")
#if HUGS_FOR_WINDOWS
	    { INT svColor = SetForeColor(BLUE);
#endif
	    Printf("(%lu reduction%s, ",plural(numReductions));
	    Printf("%lu cell%s",plural(numCells));
	    if (numGcs>0)
		Printf(", %u garbage collection%s",plural(numGcs));
	    Printf(")\n");
#if HUGS_FOR_WINDOWS
	    SetForeColor(svColor); }
#endif
#undef plural
	}
#if OBSERVATIONS
        printObserve(ALLTAGS);
        if (obsCount) {
            ERRMSG(0) "Internal: observation sanity counter > 0\n"
	    EEND;
        }
        if (showStats){
            Int n = countObserve();
            if (n > 0)
                Printf("%d observations recorded\n", n);
        }
#endif
	FlushStdout();
	garbageCollect();
    }
}

/* --------------------------------------------------------------------------
 * Print type of input expression:
 * ------------------------------------------------------------------------*/

static Void local showtype() {         /* print type of expression (if any)*/
    Cell type;

    setCurrModule(findEvalModule());
    startNewScript(0);                 /* Enables recovery of storage      */
				       /* allocated during evaluation      */
    parseExp();
    checkExp();
    defaultDefns = evalDefaults;
    type = typeCheckExp(FALSE);
    printExp(stdout,inputExpr);
#if HUGS_FOR_WINDOWS
    { INT svColor = SetForeColor(BLUE);
#endif
    Printf(" :: ");
    printType(stdout,type);
#if HUGS_FOR_WINDOWS
    SetForeColor(svColor); }
#endif
    Putchar('\n');
}

static Void local browseit(mod,t,all)
Module mod; 
String t;
Bool all; {
    if (nonNull(mod)) {
	Cell cs;
	if (nonNull(t))
	    Printf("module %s where\n",textToStr(module(mod).text));
	for (cs = module(mod).names; nonNull(cs); cs=tl(cs)) {
	    Name nm = hd(cs);
	    /* only look at things defined in this module,
	       unless `all' flag is set */
	    if (all || name(nm).mod == mod) {
		/* unwanted artifacts, like lambda lifted values,
		   are in the list of names, but have no types */
		if (nonNull(name(nm).type)) {
		    printExp(stdout,nm);
		    Printf(" :: ");
		    printType(stdout,name(nm).type);
		    if (isCfun(nm)) {
			Printf("  -- data constructor");
		    } else if (isMfun(nm)) {
			Printf("  -- class member");
		    } else if (isSfun(nm)) {
			Printf("  -- selector function");
		    }
		    if (name(nm).primDef) {
			Printf("   -- primitive");
		    }
		    Printf("\n");
		}
	    }
	}
    } else {
      if (isNull(mod)) {
	Printf("Unknown module %s\n",t);
      }
    }
}

static Void local browse() {            /* browse modules                  */
    Int    count = 0;                   /* or give menu of commands        */
    String s;
    Bool all = FALSE;

    setCurrModule(findEvalModule());
    startNewScript(0);                  /* for recovery of storage         */
    for (; (s=readFilename())!=0; count++)
	if (strcmp(s,"all") == 0) {
	    all = TRUE;
	    --count;
	} else
	    browseit(findModule(findText(s)),s,all);
    if (count == 0)
	browseit(findEvalModule(),NULL,all);
}

#if EXPLAIN_INSTANCE_RESOLUTION
static Void local xplain() {         /* print type of expression (if any)*/
    Cell d;
    Bool sir = showInstRes;

    setCurrModule(findEvalModule());
    startNewScript(0);                 /* Enables recovery of storage      */
				       /* allocated during evaluation      */
    parseContext();
    checkContext();
    showInstRes = TRUE;
    d = provePred(NIL,NIL,hd(inputContext));
    if (isNull(d)) {
	fprintf(stdout, "not Sat\n");
    } else {
	fprintf(stdout, "Sat\n");
    }
    showInstRes = sir;
}
#endif

/* --------------------------------------------------------------------------
 * Enhanced help system:  print current list of scripts or give information
 * about an object.
 * ------------------------------------------------------------------------*/

static String local objToStr(m,c)
Module m;
Cell   c; {
#if 1 || DISPLAY_QUANTIFIERS
    static char newVar[60];
    switch (whatIs(c)) {
	case NAME  : if (m == name(c).mod) {
			 sprintf(newVar,"%s", textToStr(name(c).text));
		     } else {
			 sprintf(newVar,"%s.%s",
					textToStr(module(name(c).mod).text),
					textToStr(name(c).text));
		     }
		     break;

	case TYCON : if (m == tycon(c).mod) {
			 sprintf(newVar,"%s", textToStr(tycon(c).text));
		     } else {
			 sprintf(newVar,"%s.%s",
					textToStr(module(tycon(c).mod).text),
					textToStr(tycon(c).text));
		     }
		     break;

	case CLASS : if (m == cclass(c).mod) {
			 sprintf(newVar,"%s", textToStr(cclass(c).text));
		     } else {
			 sprintf(newVar,"%s.%s",
					textToStr(module(cclass(c).mod).text),
					textToStr(cclass(c).text));
		     }
		     break;

	default    : internal("objToStr");
    }
    return newVar;
#else
    static char newVar[33];
    switch (whatIs(c)) {
	case NAME  : sprintf(newVar,"%s", textToStr(name(c).text));
		     break;

	case TYCON : sprintf(newVar,"%s", textToStr(tycon(c).text));
		     break;

	case CLASS : sprintf(newVar,"%s", textToStr(cclass(c).text));
		     break;

	default    : internal("objToStr");
    }
    return newVar;
#endif
}

static Void local info() {              /* describe objects                */
    Int    count = 0;                   /* or give menu of commands        */
    String s;

    setCurrModule(findEvalModule());
    startNewScript(0);                  /* for recovery of storage         */
    for (; (s=readFilename())!=0; count++) {
	describe(findText(s));
    }
    if (count == 0) {
	whatScripts();
    }
}

static Void local describe(t)           /* describe an object              */
Text t; {
    Tycon  tc  = findTycon(t);
    Class  cl  = findClass(t);
    Name   nm  = findName(t);

    if (nonNull(tc)) {                  /* as a type constructor           */
	Type t = tc;
	Int  i;
	Inst in;
	for (i=0; i<tycon(tc).arity; ++i) {
	    t = ap(t,mkOffset(i));
	}
	Printf("-- type constructor");
	if (kindExpert) {
	    Printf(" with kind ");
	    printKind(stdout,tycon(tc).kind);
	}
	Putchar('\n');
	switch (tycon(tc).what) {
	    case SYNONYM      : Printf("type ");
				printType(stdout,t);
				Printf(" = ");
				printType(stdout,tycon(tc).defn);
				break;

	    case NEWTYPE      :
	    case DATATYPE     : {   List cs = tycon(tc).defn;
				    if (tycon(tc).what==DATATYPE) {
					Printf("data ");
				    } else {
					Printf("newtype ");
				    }
				    printType(stdout,t);
				    Putchar('\n');
				    mapProc(printSyntax,cs);
				    if (hasCfun(cs)) {
					Printf("\n-- constructors:");
				    }
				    for (; hasCfun(cs); cs=tl(cs)) {
					Putchar('\n');
					printExp(stdout,hd(cs));
					Printf(" :: ");
					printType(stdout,name(hd(cs)).type);
				    }
				    if (nonNull(cs)) {
					Printf("\n-- selectors:");
				    }
				    for (; nonNull(cs); cs=tl(cs)) {
					Putchar('\n');
					printExp(stdout,hd(cs));
					Printf(" :: ");
					printType(stdout,name(hd(cs)).type);
				    }
				}
				break;

	    case RESTRICTSYN  : Printf("type ");
				printType(stdout,t);
				Printf(" = <restricted>");
				break;
	}
	Putchar('\n');
	if (nonNull(in=findFirstInst(tc))) {
	    Printf("\n-- instances:\n");
	    do {
		showInst(in);
		in = findNextInst(tc,in);
	    } while (nonNull(in));
	}
	Putchar('\n');
    }

    if (nonNull(cl)) {                  /* as a class                      */
	List  ins = cclass(cl).instances;
	Kinds ks  = cclass(cl).kinds;
	if (nonNull(ks) && isNull(tl(ks)) && hd(ks)==STAR) {
	    Printf("-- type class");
	} else {
	    Printf("-- constructor class");
	    if (kindExpert) {
		Printf(" with arity ");
		printKinds(stdout,ks);
	    }
	}
	Putchar('\n');
	mapProc(printSyntax,cclass(cl).members);
	Printf("class ");
	if (nonNull(cclass(cl).supers)) {
	    printContext(stdout,cclass(cl).supers);
	    Printf(" => ");
	}
	printPred(stdout,cclass(cl).head);
	if (nonNull(cclass(cl).fds)) {
	    List   fds = cclass(cl).fds;
	    String pre = " | ";
	    for (; nonNull(fds); fds=tl(fds)) {
		Printf(pre);
		printFD(stdout,hd(fds));
		pre = ", ";
	    }
	}
	if (nonNull(cclass(cl).members)) {
	    List ms = cclass(cl).members;
	    Printf(" where");
	    do {
		Type t = name(hd(ms)).type;
                if (isPolyType(t)) {
		    t = monotypeOf(t);
		}
		Printf("\n  ");
		printExp(stdout,hd(ms));
		Printf(" :: ");
		if (isNull(tl(fst(snd(t))))) {
		    t = snd(snd(t));
		} else {
		    t = ap(QUAL,pair(tl(fst(snd(t))),snd(snd(t))));
		}
		printType(stdout,t);
		ms = tl(ms);
	    } while (nonNull(ms));
	}
	Putchar('\n');
	if (nonNull(ins)) {
	    Printf("\n-- instances:\n");
	    do {
		showInst(hd(ins));
		ins = tl(ins);
	    } while (nonNull(ins));
	}
	Putchar('\n');
    }

    if (nonNull(nm)) {                  /* as a function/name              */
	printSyntax(nm);
	printExp(stdout,nm);
	Printf(" :: ");
	if (nonNull(name(nm).type)) {
	    printType(stdout,name(nm).type);
	} else {
	    Printf("<unknown type>");
	}

	if (isCfun(nm)) {
	    Printf("  -- data constructor");
	} else if (isMfun(nm)) {
	    Printf("  -- class member");
	} else if (isSfun(nm)) {
	    Printf("  -- selector function");
	}
	if (name(nm).primDef) {
	    Printf("   -- primitive");
	}
	Printf("\n\n");
    }

    if (isNull(tc) && isNull(cl) && isNull(nm)) {
	Printf("Unknown reference `%s'\n",textToStr(t));
    }
}

static Void local printSyntax(nm)
Name nm; {
    Syntax sy = syntaxOf(nm);
    Text   t  = name(nm).text;
    String s  = textToStr(t);
    if (sy != defaultSyntax(t)) {
	Printf("infix");
	switch (assocOf(sy)) {
	    case LEFT_ASS  : Putchar('l'); break;
	    case RIGHT_ASS : Putchar('r'); break;
	    case NON_ASS   : break;
	}
	Printf(" %i ",precOf(sy));
	if (isascii(*s) && isalpha(*s)) {
	    Printf("`%s`",s);
	} else {
	    Printf("%s",s);
	}
	Putchar('\n');
    }
}

static Void local showInst(in)          /* Display instance decl header    */
Inst in; {
    Printf("instance ");
    if (nonNull(inst(in).specifics)) {
	printContext(stdout,inst(in).specifics);
	Printf(" => ");
    }
    printPred(stdout,inst(in).head);
    Putchar('\n');
}

/* --------------------------------------------------------------------------
 * List all names currently in scope:
 * ------------------------------------------------------------------------*/

static Void local listNames() {         /* list names matching optional pat*/
    String pat   = readFilename();
    List   names = NIL;
    Int    width = getTerminalWidth() - 1;
    Int    count = 0;
    Int    termPos;
    Module mod   = findEvalModule();

    if (pat) {                          /* First gather names to list      */
	do {
	    names = addNamesMatching(pat,names);
	} while ((pat=readFilename())!=0);
    } else {
	names = addNamesMatching((String)0,names);
    }
    if (isNull(names)) {                /* Then print them out             */
	ERRMSG(0) "No names selected"
	EEND;
    }
    for (termPos=0; nonNull(names); names=tl(names)) {
	String s = objToStr(mod,hd(names));
	Int    l = strlen(s);
	if (termPos+1+l>width) { 
	    Putchar('\n');       
	    termPos = 0;         
	} else if (termPos>0) {  
	    Putchar(' ');        
	    termPos++;           
	}
	Printf("%s",s);
	termPos += l;
	count++;
    }
    Printf("\n(%d names listed)\n", count);
}

/* --------------------------------------------------------------------------
 * print a prompt and read a line of input:
 * ------------------------------------------------------------------------*/

static Void local promptForInput(moduleName)
String moduleName; {
    char promptBuffer[1000];
#if 1
    /* This is portable but could overflow buffer */
    sprintf(promptBuffer,prompt,moduleName);
#else
    /* Works on ANSI C - but pre-ANSI compilers return a pointer to
     * promptBuffer instead.
     */
    if (sprintf(promptBuffer,prompt,moduleName) >= 1000) {
	/* Reset prompt to a safe default to avoid an infinite loop */
	free(prompt);
	prompt = strCopy("? ");
	internal("Combined prompt and evaluation module name too long");
    }
#endif
    consoleInput(promptBuffer);
}

#if HUGS_FOR_WINDOWS
static Void local autoReloadFiles() {
    if (autoLoadFiles) {
      InAutoReloadFiles = TRUE;
      saveInputState();
      readScripts(1);
      restoreInputState();
      InAutoReloadFiles = FALSE;
    }
}
#endif

#if USE_THREADS
static Void  local loopInBackground 	Args((Void));
static Void  local stopEvaluatorThread	Args((Void));
static DWORD local evaluatorThreadBody 	Args((LPDWORD));
static Void  local startEvaluatorThread	Args((Void));

static HANDLE evaluatorThread;
static DWORD  evaluatorThreadId;
static Bool   evaluatorThreadRunning = FALSE;

static Void local stopEvaluatorThread(Void) {
    evaluatorThreadRunning = FALSE;        
    TerminateThread(evaluatorThread, 1000); 
}

static DWORD local evaluatorThreadBody(LPDWORD notUsed) {
    evaluator();
    stopEvaluatorThread();
    return 0;
}

static Void local startEvaluatorThread(Void) {

    evaluatorThread = CreateThread(NULL,
                                   0,
                                   (LPTHREAD_START_ROUTINE) evaluatorThreadBody, 
                                   NULL, 
                                   CREATE_SUSPENDED, 
                                   &evaluatorThreadId);
    //SetThreadPriority(evaluatorThread, THREAD_PRIORITY_HIGHEST); 
    evaluatorThreadRunning = TRUE;                        
    ResumeThread(evaluatorThread);
} 

#endif /* USE_THREADS */

/* --------------------------------------------------------------------------
 * main read-eval-print loop, with error trapping:
 * ------------------------------------------------------------------------*/

static jmp_buf catch_error;             /* jump buffer for error trapping  */

static Void local interpreter(argc,argv)/* main interpreter loop           */
Int    argc;
String argv[]; {
    Int errorNumber = setjmp(catch_error);

    breakOn(TRUE);                      /* enable break trapping           */
    if (numScripts==0) {                /* only succeeds on first time,    */
	if (errorNumber)                /* before prelude has been loaded  */
	    fatal("Unable to load prelude");
	initialize(argc,argv);
	forHelp();
    }

    for (;;) {
	Command cmd;
	everybody(RESET);               /* reset to sensible initial state */
	dropScriptsFrom(numScripts-1);  /* remove partially loaded scripts */
					/* not counting prelude as a script*/

#if IGNORE_MODULES
	promptForInput("");
#else
	promptForInput(textToStr(module(findEvalModule()).text));
#endif
#if HUGS_FOR_WINDOWS
        InAutoReloadFiles = FALSE;
#endif

	cmd = readCommand(cmds, (Char)':', (Char)'!');
#ifdef WANT_TIMER
	updateTimers();
#endif
	switch (cmd) {
	    case EDIT   : editor();
			  break;
	    case FIND   : 
#if HUGS_FOR_WINDOWS
			  autoReloadFiles();
#endif
                          find();
			  break;
	    case LOAD   : clearProject();
			  forgetScriptsFrom(1);
			  load();
			  break;
	    case ALSO   : clearProject();
			  forgetScriptsFrom(numScripts);
			  load();
			  break;
	    case RELOAD : readScripts(1);
			  break;
	    case PROJECT: project();
			  break;
#if !IGNORE_MODULES
	    case SETMODULE :
			  setModule();
			  break;
#endif
	    case EVAL   : 
#if HUGS_FOR_WINDOWS
			  autoReloadFiles();
#endif
#if USE_THREADS
                          startEvaluatorThread();
			  loopInBackground();
#else
                          evaluator();
#endif
			  break;
	    case TYPEOF : 
#if HUGS_FOR_WINDOWS
			  autoReloadFiles();
#endif
                          showtype();
			  break;
	    case BROWSE : browse();
			  break;
#if EXPLAIN_INSTANCE_RESOLUTION
	    case XPLAIN : xplain();
			  break;
#endif
	    case NAMES  : 
#if HUGS_FOR_WINDOWS
			  autoReloadFiles();
#endif
                          listNames();
			  break;
	    case HELP   : menu();
			  break;
	    case BADCMD : guidance();
			  break;
	    case SET    : set();
			  break;
	    case SYSTEM : if (shellEsc(readLine()))
			      Printf("Warning: Shell escape terminated abnormally\n");
			  break;
	    case CHGDIR : changeDir();
			  break;
	    case INFO   : 
#if HUGS_FOR_WINDOWS
			  autoReloadFiles();
#endif
                          info();
			  break;
	    case PNTVER: Printf("-- Hugs Version %s\n",
				 HUGS_VERSION);
			  break;
	    case QUIT   : breakOn(FALSE);
			  return;
	    case COLLECT: consGC = FALSE;
			  garbageCollect();
			  consGC = TRUE;
			  Printf("Garbage collection recovered %d cells\n",
				 cellsRecovered);
			  break;
	    case NOCMD  : break;
#ifdef __SYMBIAN32__
        case PRNDIR : printDir();
              break;
#endif
	}
#ifdef WANT_TIMER
	updateTimers();
	Printf("Elapsed time (ms): %ld (user), %ld (system)\n",
	       millisecs(userElapsed), millisecs(systElapsed));
#endif
    }
    breakOn(FALSE);
}

/* --------------------------------------------------------------------------
 * Display progress towards goal:
 * ------------------------------------------------------------------------*/

static Target currTarget;
static Bool   aiming = FALSE;
static Int    currPos;
static Int    maxPos;
static Int    charCount;

Void setGoal(what, t)                  /* Set goal for what to be t        */
String what;
Target t; {
    if (quiet
#if EXPLAIN_INSTANCE_RESOLUTION
	      || showInstRes
#endif
			    ) return;
    currTarget = (t?t:1);
    aiming     = TRUE;
    if (useDots) {
	currPos = strlen(what);
	maxPos  = getTerminalWidth() - 1;
	Printf("%s",what);
    }
    else
	for (charCount=0; *what; charCount++)
	    Putchar(*what++);
    FlushStdout();
}

Void soFar(t)                          /* Indicate progress towards goal   */
Target t; {                            /* has now reached t                */
    if (quiet
#if EXPLAIN_INSTANCE_RESOLUTION
	      || showInstRes
#endif
			    ) return;
    if (useDots) {
	Int newPos = (Int)((maxPos * ((long)t))/currTarget);

	if (newPos>maxPos)
	    newPos = maxPos;

	if (newPos>currPos) {
	    do
		Putchar('.');
	    while (newPos>++currPos);
	    FlushStdout();
	}
	FlushStdout();
    }
}

Void done() {                          /* Goal has now been achieved       */
    if (quiet
#if EXPLAIN_INSTANCE_RESOLUTION
	      || showInstRes
#endif
			    ) return;
    if (useDots) {
	while (maxPos>currPos++)
	    Putchar('.');
	Putchar('\n');
    }
    else
	for (; charCount>0; charCount--) {
	    Putchar('\b');
#if !(__MWERKS__ && macintosh)
	    Putchar(' ');
	    Putchar('\b');
#endif
	}
    aiming = FALSE;
    FlushStdout();
}

static Void local failed() {           /* Goal cannot be reached due to    */
    if (aiming) {                      /* errors                           */
	aiming = FALSE;
	Putchar('\n');
	FlushStdout();
    }
}

/* --------------------------------------------------------------------------
 * Error handling:
 * ------------------------------------------------------------------------*/

Void errHead(l)                        /* print start of error message     */
Int l; {
    failed();                          /* failed to reach target ...       */
    stopAnyPrinting();
    FPrintf(errorStream,"ERROR");

    if (scriptFile) {
 	FPrintf(errorStream," %s",scriptFile);
	setLastEdit(scriptFile,l);
 	if (l) FPrintf(errorStream,":%d",l);
	scriptFile = 0;
    }
    FPrintf(errorStream," - ");
    FFlush(errorStream);
}

Void errFail() {                        /* terminate error message and     */
    Putc('\n',errorStream);             /* produce exception to return to  */
    FFlush(errorStream);                /* main command loop               */
#if USE_THREADS
    stopEvaluatorThread();
#endif /* USE_THREADS */
    longjmp(catch_error,1);
}

Void errAbort() {                       /* altern. form of error handling  */
    failed();                           /* used when suitable error message*/
    stopAnyPrinting();                  /* has already been printed        */
    errFail();
}

Void internal(msg)                      /* handle internal error           */
String msg; {
#if HUGS_FOR_WINDOWS
    char buf[300];
    wsprintf(buf,"INTERNAL ERROR: %s",msg);
    MessageBox(hWndMain, buf, appName, MB_ICONHAND | MB_OK);
#endif
    failed();
    stopAnyPrinting();
    Printf("INTERNAL ERROR: %s\n",msg);
    FlushStdout();
#if USE_THREADS
    stopEvaluatorThread();
#endif /* USE_THREADS */
    longjmp(catch_error,1);
}

Void fatal(msg)                         /* handle fatal error              */
String msg; {
#if HUGS_FOR_WINDOWS
    char buf[300];
    wsprintf(buf,"FATAL ERROR: %s",msg);
    MessageBox(hWndMain, buf, appName, MB_ICONHAND | MB_OK);
#endif
    FlushStdout();
    Printf("\nFATAL ERROR: %s\n",msg);
    everybody(EXIT);
    exit(1);
}

sigHandler(breakHandler) {              /* respond to break interrupt      */
#if HUGS_FOR_WINDOWS
#if USE_THREADS
    MessageBox(hWndMain, "Interrupted!", appName, MB_ICONSTOP | MB_OK);
#else
    MessageBox(GetFocus(), "Interrupted!", appName, MB_ICONSTOP | MB_OK);
#endif
#endif
#if HUGS_FOR_WINDOWS
    FPrintf(errorStream,"{Interrupted!}\n");
#else
    Hilite();
    Printf("{Interrupted!}\n");
    Lolite();
#endif
    breakOn(TRUE);  /* reinstall signal handler - redundant on BSD systems */
		    /* but essential on POSIX (and other?) systems         */
    everybody(BREAK);
    failed();
    stopAnyPrinting();
    FlushStdout();
    clearerr(stdin);
#if USE_THREADS
    stopEvaluatorThread();
#endif /* USE_THREADS */
    longjmp(catch_error,1);
    sigResume;/*NOTREACHED*/
}

/* --------------------------------------------------------------------------
 * Read value from environment variable or registry:
 * ------------------------------------------------------------------------*/

String fromEnv(var,def)         /* return value of:                        */
String var;                     /*     environment variable named by var   */
String def; {                   /* or: default value given by def          */
    String s = getenv(var);     
    return (s ? s : def);
}

/* --------------------------------------------------------------------------
 * String manipulation routines:
 * ------------------------------------------------------------------------*/

static String local strCopy(s)         /* make malloced copy of a string   */
String s; {
    if (s && *s) {
	char *t, *r;
	if ((t=(char *)malloc(strlen(s)+1))==0) {
	    ERRMSG(0) "String storage space exhausted"
	    EEND;
	}
	for (r=t; (*r++ = *s++)!=0; ) {
	}
	return t;
    }
    return NULL;
}

/* --------------------------------------------------------------------------
 * Compiler output
 * We can redirect compiler output (prompts, error messages, etc) by
 * tweaking these functions.
 * ------------------------------------------------------------------------*/

#if REDIRECT_OUTPUT && !HUGS_FOR_WINDOWS

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#else
#include <varargs.h>
#endif

/* ----------------------------------------------------------------------- */

#define BufferSize 10000	      /* size of redirected output buffer  */

typedef struct _HugsStream {
    char buffer[BufferSize];          /* buffer for redirected output      */
    Int  next;                        /* next space in buffer              */
} HugsStream;

static Void   local vBufferedPrintf  Args((HugsStream*, const char*, va_list));
static Void   local bufferedPutchar  Args((HugsStream*, Char));
static String local bufferClear      Args((HugsStream *stream));

static Void local vBufferedPrintf(stream, fmt, ap)
HugsStream* stream;
const char* fmt;
va_list     ap; {
    Int spaceLeft = BufferSize - stream->next;
    char* p = &stream->buffer[stream->next];
    Int charsAdded = vsnprintf(p, spaceLeft, fmt, ap);
    if (0 <= charsAdded && charsAdded < spaceLeft) 
	stream->next += charsAdded;
#if 1 /* we can either buffer the first n chars or buffer the last n chars */
    else
	stream->next = 0;
#endif
}

static Void local bufferedPutchar(stream, c)
HugsStream *stream;
Char        c; {
    if (BufferSize - stream->next >= 2) {
	stream->buffer[stream->next++] = c;
	stream->buffer[stream->next] = '\0';
    }
}    

static String local bufferClear(stream)
HugsStream *stream; {
    if (stream->next == 0) {
	return "";
    } else {
	stream->next = 0;
	return stream->buffer;
    }
}

/* ----------------------------------------------------------------------- */

static HugsStream outputStream;
/* ADR note: 
 * We rely on standard C semantics to initialise outputStream.next to 0.
 */

Void hugsEnableOutput(f) 
Bool f; {
    disableOutput = !f;
}

String hugsClearOutputBuffer() {
    return bufferClear(&outputStream);
}

#ifdef HAVE_STDARG_H
Void hugsPrintf(const char *fmt, ...) {
    va_list ap;                    /* pointer into argument list           */
    va_start(ap, fmt);             /* make ap point to first arg after fmt */
    if (!disableOutput) {
	vprintf(fmt, ap);
    } else {
	vBufferedPrintf(&outputStream, fmt, ap);
    }
    va_end(ap);                    /* clean up                             */
}
#else
Void hugsPrintf(fmt, va_alist) 
const char *fmt;
va_dcl {
    va_list ap;                    /* pointer into argument list           */
    va_start(ap);                  /* make ap point to first arg after fmt */
    if (!disableOutput) {
	vprintf(fmt, ap);
    } else {
	vBufferedPrintf(&outputStream, fmt, ap);
    }
    va_end(ap);                    /* clean up                             */
}
#endif

Void hugsPutchar(c)
int c; {
    if (!disableOutput) {
	putchar(c);
    } else {
	bufferedPutchar(&outputStream, c);
    }
}

Void hugsFlushStdout() {
    if (!disableOutput) {
	fflush(stdout);
    }
}

Void hugsFFlush(fp)
FILE* fp; {
    if (!disableOutput) {
	fflush(fp);
    }
}

#ifdef HAVE_STDARG_H
Void hugsFPrintf(FILE *fp, const char* fmt, ...) {
    va_list ap;             
    va_start(ap, fmt);      
    if (!disableOutput) {
	vfprintf(fp, fmt, ap);
    } else {
	vBufferedPrintf(&outputStream, fmt, ap);
    }
    va_end(ap);             
}
#else
Void hugsFPrintf(FILE *fp, const char* fmt, va_list)
FILE* fp;
const char* fmt;
va_dcl {
    va_list ap;             
    va_start(ap);      
    if (!disableOutput) {
	vfprintf(fp, fmt, ap);
    } else {
	vBufferedPrintf(&outputStream, fmt, ap);
    }
    va_end(ap);             
}
#endif

Void hugsPutc(c, fp)
int   c;
FILE* fp; {
    if (!disableOutput) {
	putc(c,fp);
    } else {
	bufferedPutchar(&outputStream, c);
    }
}
    
#endif /* REDIRECT_OUTPUT && !HUGS_FOR_WINDOWS */

/* --------------------------------------------------------------------------
 * Break dialogue code
 * ------------------------------------------------------------------------*/
#if OBSERVATIONS
static struct cmd brkCmds[] =
    { {"p",   BRK_DISPLAY}
    , {"c",   BRK_CONTINUE}
    , {"s",   BRK_SET}
    , {"r",   BRK_RESET}
    , {0,0}
    };

Void breakDialogue(s)
String s;{
    String arg;
    Int n;
    char cmdstr[80];
    Command cmd;

    normalTerminal();
    do {
	strcpy(cmdstr,"Break @ ");
	promptForInput(strcat(cmdstr,s));
	cmd = readCommand(brkCmds, (Char)0, (Char)'!');
	switch (cmd){
	    case BRK_DISPLAY:	
	       			if ((arg=readFilename())!=0)
				    printObserve(arg);
				else
				    printObserve(ALLTAGS);
	    			break;
	    case BRK_CONTINUE:	
	    			if ((arg=readFilename())!=0){
				    n = atoi(arg);
				    if (n>0) n--;
				    setBreakCount(s,n);
				}
	    			break;
	    case BRK_SET:	if ((arg=readFilename())!=0)
	    			    setBreakpt(arg,TRUE);
				break;
	    case BRK_RESET:	if ((arg=readFilename())==0)
	    			    setBreakpt(s,FALSE);
				else
	    			    setBreakpt(arg,FALSE);
				break;
	}
    } while (cmd!=BRK_CONTINUE);
    noechoTerminal();
}
#endif

/* --------------------------------------------------------------------------
 * Send message to each component of system:
 * ------------------------------------------------------------------------*/

Void everybody(what)            /* send command `what' to each component of*/
Int what; {                     /* system to respond as appropriate ...    */
    machdep(what);              /* The order of calling each component is  */
    storage(what);              /* important for the INSTALL command       */
    substitution(what);
    input(what);
    staticAnalysis(what);
    typeChecker(what);
    compiler(what);
    machine(what);
    builtIn(what);
    controlFuns(what);
    plugins(what);
    ffi(what);
}

/* --------------------------------------------------------------------------
 * Hugs for Windows code (WinMain and related functions)
 * ------------------------------------------------------------------------*/

#if HUGS_FOR_WINDOWS
#include "winhugs\winhugs.c"
#if USE_THREADS
static Void local loopInBackground (Void) { 
    MSG msg;

   //WaitForSingleObject(evaluatorThread, INFINITE);
    while ( evaluatorThreadRunning && GetMessage(&msg, NULL, 0, 0) ) {
      if (!TranslateAccelerator(hWndMain, hAccelTable, &msg)) {
         TranslateMessage(&msg);
         DispatchMessage(&msg);
      }
    }
}
#endif /* USE_THREADS */
#endif

/*-------------------------------------------------------------------------*/

