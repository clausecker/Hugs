/* --------------------------------------------------------------------------
 * Standalone hugs system
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2002, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: runhugs.c,v $
 * $Revision: 1.10 $
 * $Date: 2002/04/11 23:20:21 $
 * ------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include "config.h"
#include "options.h"
#include "server.h"

/* In case the server API couldn't
   be initialised, look at 'lastError'. */
extern char* lastError;

#if defined(_MSC_VER) && !defined(_MANAGED)
#include <windows.h>
#endif

extern int  main      Args((int, char**));
static void check     Args((void));
static void loadHugs  Args((int, char**));

extern void           shutdownHugsServer Args((HugsServerAPI*));
extern HugsServerAPI* initHugsServer Args((int,char**));
static HugsServerAPI* hugs = 0;

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
    int    exitCode;
    char** hugs_argv;
    int    hugs_argc;

    if (!initSystem()) {
      fprintf(stderr,"%0: failed to initialize, exiting\n", (argv ? argv[0] : ""));
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

    hugs->setHugsArgs(argc,argv);

    hugs->pushHVal(hugs->compileExpr("Main","main >> return ()"));
    exitCode = hugs->doIO();
    check();
    
#if defined(_MSC_VER) && !defined(_MANAGED)
    } __except ( ((GetExceptionCode() == EXCEPTION_STACK_OVERFLOW) ? EXCEPTION_EXECUTE_HANDLER : EXCEPTION_CONTINUE_SEARCH) ) {
      fatal("C stack overflow");
    }
#endif

    shutdownHugsServer(hugs);
    
    exit(exitCode);
    return 0;/*NOTUSED*/
}
