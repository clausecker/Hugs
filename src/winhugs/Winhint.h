/* --------------------------------------------------------------------------
 * WinHint.h:	José Enrique Gallardo Ruiz, Feb 1999
 *
 * Hugs for Windows is Copyright (c) José Enrique Gallardo, Mark P Jones,
 * Alastair Reid and the Yale Haskell Group 1994-99, and is distributed as
 * Open Source software under the Artistic License; see the file "Artistic"
 * that is included in the distribution for details.
 *
 * This file contains the Header file for a hint window definition
 * ------------------------------------------------------------------------*/

#define __WINHINT_H

#define MAXLNG	64




typedef struct tagHINT {
  CHAR		HintStr[MAXLNG];	/* The message on the hint */
} HINT;

typedef HINT *HHINT;


/* Functions defined in WinTip.c that are exported */
HWND 	       HintCreateWindow	   	(HINSTANCE, HWND);
BOOL 	       HintRegisterClass   	(HINSTANCE);

