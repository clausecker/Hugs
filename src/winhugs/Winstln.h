/* --------------------------------------------------------------------------
 * WinSTLN.h:	José Enrique Gallardo Ruiz, Feb 1999
 *
 * Hugs for Windows is Copyright (c) José Enrique Gallardo, Mark P Jones,
 * Alastair Reid and the Yale Haskell Group 1994-99, and is distributed as
 * Open Source software under the Artistic License; see the file "Artistic"
 * that is included in the distribution for details.
 *
 * This file contains the Header file for a status line definition
 * ------------------------------------------------------------------------*/

#define __WINSTLN_H

typedef struct TagSTLN {
   CHAR 	LeftText[256];
   CHAR 	ResizeBitmap[128];
   HINSTANCE 	hInstance;
} STLN;


typedef STLN 	*HSTLN;


/* Functions defined in WinSTLN.c that are exported */
HWND 	       STLNCreateWindow	   	(HINSTANCE, HWND, LPCSTR);
BOOL 	       STLNRegisterClass   	(HINSTANCE);

