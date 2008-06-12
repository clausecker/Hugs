/* --------------------------------------------------------------------------
 * Core generator: loads all sources and outputs intermediate Core,
 * then terminates not starting REPL.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2005, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "command.h"
#include "connect.h"
#include "errors.h"
#include "script.h"
#include "opts.h"
#include "strutil.h"
#include "evaluator.h"
#include "machdep.h"
#include "output.h"
#include "module.h"
#include <setjmp.h>
#include <ctype.h>

#include <stdio.h>

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void   local clearEvalModule   Args((Void));
static Void   local interpreter       Args((Int,String []));

/* --------------------------------------------------------------------------
 * Optional timer hooks:
 * ------------------------------------------------------------------------*/
#if WANT_TIMER
#include "timer.c"
#endif

/* --------------------------------------------------------------------------
 * Local data areas:
 * ------------------------------------------------------------------------*/
static Text    evalModule = 0;  /* Name of module we eval exprs in */
static String  defaultArgv[] = { "Corehugs" };  /* program name */

/* --------------------------------------------------------------------------
 * UI interpreter initalization:
 * ------------------------------------------------------------------------*/
static Void local initialize(argc,argv)
Int    argc;
String argv[]; {
    startEvaluator();

    /* force +C to be among the default options to generate Core */

    readOptions("-p\"%s> \" -r$$ +C",FALSE);
    readOptionSettings();
    processOptionVector(argc,argv);

    /* Figure out what Prelude module we're using + hoist it in. */
    loadPrelude();

    /* Add an empty module as the default, to avoid being inside the Prelude */
    addScriptName(STD_EMPTY_MODULE, TRUE);

    /* We record the number of scripts that loading the Prelude
     * brought about, so that when the user comes to clear the module
     * stack (e.g., ":l<ENTER>"), only modules later than the Prelude
     * ones are scratched.
     */
    setScriptStableMark();

    addScriptsFromArgs(argc,argv);
    setHugsArgs(1, defaultArgv);

    clearEvalModule();		/* evaluate wrt last module by default */
    readScripts(0);
}

/* --------------------------------------------------------------------------
 * Hugs entry point:
 * ------------------------------------------------------------------------*/

int main Args((Int, String []));       /* now every func has a prototype  */

int main(argc,argv)
int  argc;
char *argv[]; {

#ifndef DEBUG_SHOWSC

    Printf("%0: Hugs was not compiled with external core enabled\n" (argv ? argv[0] : ""));
    Printf("Please run configure --enable-extcore and rebuild\n", "");
    return 1;

#endif

    CStackBase = &argc;                 /* Save stack base for use in gc   */

    if (!initSystem()) {
	Printf("%0: failed to initialize, exiting\n", (argv ? argv[0] : ""));
	return 1;
    }
    errorCount = 0;
    interpreter(argc,argv);
    everybody(EXIT);
    shutdownHugs();

    return (errorCount > 0);
}

/* --------------------------------------------------------------------------
 * Shutdown interpreter.
 * ------------------------------------------------------------------------*/
Void shutdownHugs() {
    /* Let go of dynamic storage */
    if (hugsEdit) { free(hugsEdit);  hugsEdit=0; }
    stopEvaluator();
}

static Void local clearEvalModule() {
    evalModule = findText("");
}

/* --------------------------------------------------------------------------
 * print a prompt and read a line of input:
 * ------------------------------------------------------------------------*/

/* Size of (expanded) prompt buffer, should be more than enough.... */
#define MAX_PROMPT_SIZE 1000

Void promptForInput(moduleName)
String moduleName; {
    char promptBuffer[MAX_PROMPT_SIZE];
    char* fromPtr;
    char* toPtr;
    int modLen = strlen(moduleName);
    int roomLeft = MAX_PROMPT_SIZE - 1;

    toPtr = promptBuffer;
    fromPtr = prompt;

    /* Carefully substituting occurrences of %s in the
       prompt string with the module name.
    */
    while (*fromPtr != '\0' && roomLeft > 0) {
	if (*fromPtr == '%' && *(fromPtr+1) == 's') {
	    /* Substitute module name */
	    if (modLen > roomLeft) {
		/* Running out of room; copy what we can */
		fromPtr = moduleName;
		while (roomLeft-- > 0)
		    *toPtr++ = *fromPtr++;
		break;
	    } else {
		strcpy(toPtr,moduleName);
		toPtr += modLen;
		roomLeft -= modLen;
		fromPtr +=2;
	    }
	} else {
	    *toPtr++ = *fromPtr++;
	    roomLeft--;
	}
    }
    *toPtr = '\0';

    consoleInput(promptBuffer);
}

/* --------------------------------------------------------------------------
 * main read-eval-print loop, with error trapping:
 * ------------------------------------------------------------------------*/

static Void local interpreter(argc,argv)/* main interpreter loop           */
Int    argc;
String argv[]; {
#if HAVE_SIGSEGV_H && HAVE_STACK_OVERFLOW_RECOVERY
    char extra_stack[16384];
#endif

    if (setjmp(catch_error)) {
	if (numLoadedScripts()==0)
	    fatal("Unable to load Prelude");
	everybody(RESET);
	garbageCollect();		/* gc after stack has unwound      */
    }
    breakOn(TRUE);                      /* enable break trapping           */
#if HAVE_SIGSEGV_H && HAVE_STACK_OVERFLOW_RECOVERY
    stackoverflow_install_handler(stackOverflow, extra_stack, sizeof(extra_stack));
#endif
    if (numLoadedScripts()==0) {	/* only succeeds on first time,    */
	initialize(argc,argv);		/* before Prelude has been loaded  */
    }
}

/*-------------------------------------------------------------------------*/
