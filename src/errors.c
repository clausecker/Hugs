/* --------------------------------------------------------------------------
 * Error handling support functions
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2002, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * ------------------------------------------------------------------------*/
#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "output.h"
#include "opts.h"
#include "goal.h"
#include "evaluator.h" /* everybody() proto only */
#include <setjmp.h>

jmp_buf catch_error;          /* jump buffer for error trapping  */

/* --------------------------------------------------------------------------
 * Error handling:
 * ------------------------------------------------------------------------*/

Void stopAnyPrinting() {  /* terminate printing of expression,*/
    if (printing) {       /* after successful termination or  */
	printing = FALSE; /* runtime error (e.g. interrupt)   */
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

Void errHead(l)                        /* print start of error message     */
Int l; {
    failed();                          /* failed to reach target ...       */
    stopAnyPrinting();
    FPrintf(errorStream,"ERROR");

    /*
     * Encapsulating the filename portion inside of d-quotes makes it
     * a tad easier for an Emacs-mode to decipher the location of the error.
     * -- sof 9/01.
     */
    if (scriptFile) {
 	FPrintf(errorStream," \"%s\"",scriptFile);
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

/* --------------------------------------------------------------------------
 * Break interrupt handler:
 * ------------------------------------------------------------------------*/
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
