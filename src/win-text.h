/* --------------------------------------------------------------------------
 * Win-Text.h:	José Enrique Gallardo Ruiz, April 1995
 *		With very minor modifications by mpj/adr for Hugs, 1995-97
 *
 * Hugs 98 for Windows is Copyright (c) José Enrique Gallardo, Mark P
 * Jones, Alastair Reid, the Yale Haskell Group, and the Oregon
 * Graduate Institute of Science and Technology, 1994-1999, All rights
 * reserved.  It is distributed as free software under the license in
 * the file "License", which is included in the distribution.
 *
 * This file contains the interface to win-text.c.
 * ------------------------------------------------------------------------*/

#define __WIN_TEXT_H

#ifndef __STDIO_H
#include <stdio.h>
#endif

/* --------------------------------------------------------------------------
 * Colour definitions to make output prettier:
 * ------------------------------------------------------------------------*/

#define BLACK           0
#define BLUE            1
#define GREEN           2
#define CYAN            3
#define RED             4
#define MAGENTA         5
#define YELLOW          6
#define WHITE           7

/* --------------------------------------------------------------------------
 * Definitions of some types:
 * ------------------------------------------------------------------------*/

typedef UCHAR HUGE*	FPOINTER;
typedef UCHAR* 		POINTER;

/* --------------------------------------------------------------------------
 * Functions exported from win-text.c:
 * ------------------------------------------------------------------------*/

HWND 	  CreateTextWindow  (HINSTANCE, HWND, INT, INT, UINT, UINT, HFONT, HACCEL);
BOOL 	  RegisterTextClass (HINSTANCE);
INT cdecl TextWinPrintf     (const CHAR *, ...);
INT cdecl TextWinFprintf    (FILE *, const CHAR *, ...);
BOOL 	  WinAllowBreak	    (HWND hWnd);
VOID      WinTextcolor      (HWND, INT);
VOID      WinTextbackground (HWND, INT);
VOID      WinTextbright     (HWND, BOOL);
VOID      WinGotoxy         (HWND, UINT, UINT);
UINT      WinWherex         (HWND);
UINT      WinWherey         (HWND);
BOOL      WinSetinsert      (HWND, BOOL);
BOOL      WinSetcursor      (HWND, BOOL);
INT       WinPuts           (HWND, CHAR *);
INT 	  WinPutchar	    (HWND, CHAR);
INT	  WinPutc	    (HWND, CHAR, FILE*);
BOOL      WinKbhit          (HWND);
TCHAR     WinGetch          (HWND);
VOID      WinClrscr         (HWND);
VOID      WinClreol         (HWND);
CHAR 	  *WinGets          (HWND, CHAR *);
INT       WinGetc           (HWND, FILE*);
INT cdecl WinPrintf         (HWND, const CHAR *, ...);
INT cdecl WinFprintf        (HWND, FILE *, const CHAR *, ...);
CHAR      *GetSelectedText  (HWND);

/* ---------------------------------------------------------------------------
 * Now we map C standard I/O functions to the ones defined in win-text.c:
 * ------------------------------------------------------------------------ */

#if !DOS
extern INT WinSystem(const char*);
#endif

#define printf     TextWinPrintf
#define fprintf    TextWinFprintf
#undef  getc
#define getc(x)    WinGetc(hWndText,(x))
#undef  putchar
#define putchar(x) WinPutchar(hWndText,(x))
#undef  putc
#define putc(c,s)  WinPutc(hWndText,c,s)
#define kbhit()    WinKbhit(hWndText)
#define system     WinSystem
#undef  getchar()
#define getchar()  WinGetc(hWndText, stdin)
#define getch()    WinGetch(hWndText)

extern FILE *stdstr;		/* fprintf(stdstr, ...) used to direct	   */
extern char stdstrbuff[];	/* output to string stdstrbuff		   */

#define WM_CANCOPY 		WM_USER+1
#define WM_CANCUT 		WM_USER+2
#define WM_CANCLEAR 		WM_USER+3
#define WM_CANPASTE 		WM_USER+4

/*-------------------------------------------------------------------------*/

