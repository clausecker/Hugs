/* --------------------------------------------------------------------------
 * Standalone hugs system
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2002, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: runhugs.c,v $
 * $Revision: 1.15 $
 * $Date: 2003/03/07 00:52:13 $
 * ------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include "prelude.h"
#include "server.h"

#if defined(_MSC_VER) && !defined(_MANAGED)
#include <windows.h>
#endif

extern int  main      Args((int, char**));
static void check     Args((void));
static void loadHugs  Args((int, char**));

static HugsServerAPI* hugs = 0;

/* An optional nicety to call initSystem(); not required. */
extern int            initSystem  Args((void));

static void check() {
    char* err = hugs->clearError();
    if (err) {
	fprintf(stderr,"runhugs: %s\n",err);
	fflush(stderr);
	exit(1);
    }
}

static void loadHugs(argc,argv)
int    argc;
char* argv[]; {
    hugs = initHugsServer(argc,argv);
    if (NULL == hugs) {
	fprintf(stderr,"runhugs: Unable to initialise Hugs (%s)\n", lastError);
	fflush(stderr);
	exit(1);
    }
    hugs->setOutputEnable(0);
    check();
}

/* --------------------------------------------------------------------------
 * main
 * ------------------------------------------------------------------------*/

int main(argc,argv)
int    argc;
char* argv[]; {
    int    exitCode = 0;
    char** hugs_argv;
    int    hugs_argc;

    if (!initSystem()) {
      fprintf(stderr,"%s: failed to initialize, exiting\n", (argv ? argv[0] : ""));
      fflush(stderr);
      exit(1);
    }
#if __MWERKS__ && macintosh
    argc = ccommand(&argv);
#endif

    /* skip over any option flags before the program name */
    {
	int i = 1; /* ignore first arg - name of this program */
	while (i < argc 
	       && argv[i] /* paranoia */
	       && (argv[i][0] == '+' || argv[i][0] == '-')
	       ) {
	    ++i;
	}
	hugs_argv = argv;
	hugs_argc = i;

	argv += i;
	argc -= i;
    }

    if (argc < 1) {
	fprintf(stderr,"runhugs: missing file argument\n");
	fflush(stderr);
	exit(1);
    }

#if defined(_MSC_VER) && !defined(_MANAGED)
    __try {
#endif

    loadHugs(hugs_argc, hugs_argv);

    hugs->loadFile(argv[0]);
    check();

    /* For 'ffihugs', we're done once the module has been loaded.  */
#if !defined(FFI_COMPILER)
    hugs->setHugsArgs(argc,argv);
    hugs->pushHVal(hugs->compileExpr("Main","main >> return ()"));
    exitCode = hugs->doIO();
    check();
#endif

#if defined(_MSC_VER) && !defined(_MANAGED)
    } __except ( ((GetExceptionCode() == EXCEPTION_STACK_OVERFLOW) ? EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH) ) {
      fatal("C stack overflow");
    }
#endif

    shutdownHugsServer(hugs);
    
    exit(exitCode);
    return 0;/*NOTUSED*/
}
