/* --------------------------------------------------------------------------
 * WinUtils.h:	José Enrique Gallardo Ruiz, Feb 1999
  *
 * Hugs for Windows is Copyright (c) José Enrique Gallardo, Mark P Jones,
 * Alastair Reid and the Yale Haskell Group 1994-99, and is distributed as
 * Open Source software under the Artistic License; see the file "Artistic"
 * that is included in the distribution for details.
 *
 * This file contains the Header file for some common functions
 * ------------------------------------------------------------------------*/

#define __WINUTILS_H

VOID 	CenterDialogInParent 	(HWND);
VOID	SetDialogFont		(HWND, HFONT);
BOOL 	CheckExt 		(LPCSTR, LPCSTR);
VOID 	DrawTransparentBitmap	(HDC, HBITMAP, UINT, UINT, COLORREF);
VOID 	DrawBitmap 		(HDC, HBITMAP, UINT, UINT);
VOID 	ExecDialog 		(HINSTANCE, WORD, WNDPROC);
VOID	FullPath		(LPSTR, LPCSTR);
VOID	MapBitmap 		(HBITMAP, COLORREF, COLORREF);
HBITMAP LoadMappedBitmap	(HINSTANCE, LPCSTR);
HBITMAP ResizeBitmap            (HBITMAP, UINT, UINT);
INT 	SetWorkingDir 		(LPCSTR);
VOID 	StrReplace		(CHAR*, CHAR*, CHAR*, CHAR*);
VOID    ShortFileName           (CHAR *, CHAR *);