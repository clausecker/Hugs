/*
 * The Hugs evaluator / command interpreter + support functions.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2002, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 */
#ifndef __EVALUATOR_H__
#define __EVALUATOR_H__

extern Void startEvaluator    Args((Void));
extern Void stopEvaluator     Args((Void));
extern Void evaluator         Args((Module));
extern Void everybody         Args((Int));
extern Void loadPrelude       Args((Void));

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

#endif /* __EVALUATOR_H__ */
