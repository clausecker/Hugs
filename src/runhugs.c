/* --------------------------------------------------------------------------
 * Standalone hugs system
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: runhugs.c,v $
 * $Revision: 1.3 $
 * $Date: 1999/09/13 11:01:06 $
 * ------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include "config.h"
#include "options.h"
#include "server.h"

#if HAVE_CONSOLE_H
# include <console.h>
#endif

extern int  main      Args((int, char**));
static void check     Args((void));
static void loadHugs  Args((int, char**));

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
	fprintf(stderr,"runhugs: Unable to initialise Hugs\n");
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

#ifdef HAVE_CONSOLE_H /* Macintosh port */
    _ftype = 'TEXT';
    _fcreator = 'R*ch';		/* // 'KAHL';	//'*TEX';	//'ttxt'; */

    console_options.top = 40;
    console_options.left = 6;

    /* Example of combinations (nrows, ncols, txFont, txSize):
	(35, 100, 22, 10)
	(38, 120, 22, 9)
    */
    console_options.nrows = 34;
    console_options.ncols = 80;

    console_options.pause_atexit = 1;
    console_options.title = "\pHugs" HUGS_VERSION;

    console_options.txFont = 22;	/* 22 = Courier			   */
    console_options.txSize = 10;

    /* console_options.procID = 5; */
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

    loadHugs(hugs_argc, hugs_argv);

    hugs->loadFile(argv[0]);
    check();

    hugs->setHugsArgs(argc,argv);

    hugs->lookupName("Main","main");
    exitCode = hugs->doIO();
    check();

    exit(exitCode);
    return 0;/*NOTUSED*/
}
