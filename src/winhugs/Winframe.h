/* --------------------------------------------------------------------------
 * WinFrame.h:	José Enrique Gallardo Ruiz, Feb 1999
 *
 * Hugs for Windows is Copyright (c) José Enrique Gallardo, Mark P Jones,
 * Alastair Reid and the Yale Haskell Group 1994-99, and is distributed as
 * Open Source software under the Artistic License; see the file "Artistic"
 * that is included in the distribution for details.
 *
 * This file contains the Header file for a frame window definition
 * ------------------------------------------------------------------------*/


#define __WINFRAME_H

typedef struct TagFRAME {
   HWND		hWndSTLN;
   HWND		hWndTB;
   HWND		hWndChild;
   HINSTANCE	hInstance;
} FRAME;


typedef FRAME 	*HFRAME;


/* Functions defined in WinFrame.c that are exported */
HWND 	       FRAMECreateWindow	(HINSTANCE, LPCSTR, WNDPROC, WNDPROC*, HWND, LPCSTR, LPCSTR, LPCSTR, LPCSTR);
BOOL 	       FRAMERegisterClass	(HINSTANCE);
HWND	       FRAMEGetTB		(HWND);
HWND	       FRAMEGetSTLN		(HWND);
INT 	       FRAMEGetRightBorderSize	(HWND);
VOID	       FRAMESetChild		(HWND, HWND);
BOOL	       FRAMESuperclass 		(HINSTANCE, LPCSTR, LPCSTR, LPCSTR);

