/* --------------------------------------------------------------------------
 * WinSTLN.h:	José Enrique Gallardo Ruiz, Feb 1999
 *
 * Hugs for Windows is Copyright (c) José Enrique Gallardo, Mark P Jones,
 * Alastair Reid, the Yale Haskell Group, and the Oregon Graduate Institute
 * of Science and Technology, 1994-1999, All rights reserved.  It is
 * distributed as free software under the license in the file "License",
 * which is included in the distribution.
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

