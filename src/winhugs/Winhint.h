/* --------------------------------------------------------------------------
 * WinHint.h:	José Enrique Gallardo Ruiz, Feb 1999
 *
 * Hugs for Windows is Copyright (c) José Enrique Gallardo, Mark P Jones,
 * Alastair Reid, the Yale Haskell Group, and the Oregon Graduate Institute
 * of Science and Technology, 1994-1999, All rights reserved.  It is
 * distributed as free software under the license in the file "License",
 * which is included in the distribution.
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

