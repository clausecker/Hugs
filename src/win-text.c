/*---------------------------------------------------------------------------
 * Win-text.c:	José Enrique Gallardo Ruiz,  April 1995
 *		With very minor modifications by mpj/adr for Hugs, 1995-98
 *
 * Hugs 98 for Windows is Copyright (c) José Enrique Gallardo, Mark P
 * Jones, Alastair Reid, the Yale Haskell Group, and the Oregon
 * Graduate Institute of Science and Technology, 1994-1999, All rights
 * reserved.  It is distributed as free software under the license in
 * the file "License", which is included in the distribution.
 *
 * Functions to define a windows class that emulates MS-DOS text
 * windows. These windows support ANSI control.
 *-------------------------------------------------------------------------*/

#define STRICT
#include "prelude.h"
#include <alloc.h>
#include <mem.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdarg.h>

/* --------------------------------------------------------------------------
 * Local functions protoypes:
 * ------------------------------------------------------------------------*/

LRESULT FAR APIENTRY  _export TextWndProc (HWND, UINT, WPARAM, LPARAM);

static VOID 		MoveCursor 		(HWND, INT);
static VOID		UnSelect		(HWND);
static VOID		MyMemMove		(FPOINTER, FPOINTER, ULONG);
static VOID		MyMemSet		(FPOINTER, CHAR, ULONG);
static VOID             PutTextSelectedRegion	(HWND, RECT);
static VOID 		PutTextRect 		(BOOL, HWND, RECT);
static VOID 		PutTextRegion 		(BOOL, HWND, RECT);
static CHAR 	       *Ansi			(HWND, CHAR*);

static VOID 		TextBeginPaint 		(BOOL, HWND, HDC*, PAINTSTRUCT*, HFONT*);
static VOID 		TextEndPaint 		(BOOL, HWND, HDC*, PAINTSTRUCT*, HFONT*);
static VOID 		TextOutput 		(HWND, HDC, UINT, UINT, LPSTR, INT, UCHAR);

static INT 		DoCreate		(HWND, UINT, WPARAM, LPARAM);
static VOID 		DoDestroy		(HWND, UINT, WPARAM, LPARAM);
static VOID 		DoPaint         	(HWND, UINT, WPARAM, LPARAM);
static VOID 		DoSetFocus 		(HWND, UINT, WPARAM, LPARAM);
static VOID 		DoKillFocus		(HWND, UINT, WPARAM, LPARAM);
static INT 		DoKeyDown		(HWND, UINT, WPARAM, LPARAM);
static INT 		DoKeyUp 		(HWND, UINT, WPARAM, LPARAM);
static INT 		DoChar			(HWND, UINT, WPARAM, LPARAM);
static VOID 		DoSize			(HWND, UINT, WPARAM, LPARAM);
static VOID 		DoHScroll		(HWND, UINT, WPARAM, LPARAM);
static VOID 		DoVScroll		(HWND, UINT, WPARAM, LPARAM);
static VOID 		DoLButtonDown		(HWND, UINT, WPARAM, LPARAM);
static VOID 		DoMouseMove 		(HWND, UINT, WPARAM, LPARAM);
static VOID 		DoLButtonUp 		(HWND, UINT, WPARAM, LPARAM);
static VOID 		DoRButtonDown 		(HWND, UINT, WPARAM, LPARAM);
static VOID 		DoCopy			(HWND, UINT, WPARAM, LPARAM);
static VOID 		DoPaste 		(HWND, UINT, WPARAM, LPARAM);
static VOID 		DoCutClear 		(HWND, UINT, WPARAM, LPARAM);
static BOOL 		DoCanCut 		(HWND, UINT, WPARAM, LPARAM);
static BOOL 		DoCanCopy 		(HWND, UINT, WPARAM, LPARAM);
static BOOL 		DoCanPaste 		(HWND, UINT, WPARAM, LPARAM);
static BOOL 		DoCanClear 		(HWND, UINT, WPARAM, LPARAM);

/* --------------------------------------------------------------------------
 * Text Window Structure:
 * ------------------------------------------------------------------------*/

typedef struct TagTextWindowInfo {
  INT      Left, Top; 			/* Position in parent Window              */
  HFONT	   hFont;		      	/* Text Font used            	          */
  UINT 	   CharWidth, CharHeight;  	/* Width and Height of used font          */
  UINT	   Rows, Cols;			/* Number of rows and columns of window   */
  UINT 	   RowsShowed, ColsShowed;	/* Nº of cols and rows currently showed   */
  UINT 	   PosX, PosY;			/* Cursor current postion  	          */
  INT      TextAttr, TextBackAttr;	/* Current attributes  	           	  */
  BOOL     Bright;			/* Bright ON/OFF               		  */
  HGLOBAL  ScrBuffer;			/* Screen buffer		          */
  BOOL     CursorStatus;		/* Cursor ON/OFF		          */
  BOOL     InsertStatus;		/* Edit capability Insert(ON/OFF)         */
  BOOL	   IsFocused;			/* TRUE if window has Focus               */
  HGLOBAL  KbdBuffer;			/* Keyboard buffer   		          */
  INT      KbdInx;			/* Next free position in keyboard buffer  */
  HACCEL   hAccelTable;			/* Accelerators to use in Window          */
  HGLOBAL  hBufferEd;			/* Edit buffer for edit capability        */
  HGLOBAL  hBufferEdPrev;           	/* Previous edit buffers   	          */
  INT      IoInx;	                /* Points to last char readed with WinGetc*/
  CHAR     AnsiStr[200];		/* Used to implement ANSI support	  */
  INT      AnsiPtr;
  BOOL     InAnsi;
  UINT 	   AnsiSavex, AnsiSavey;
  UINT 	   VScroll, HScroll;		/* Offsets for scroll implementation       */
  BOOL	   Selecting;			/* TRUE if selecting text with mouse       */
  BOOL     Selected;			/* TRUE if there is selected text 	   */
  RECT     rSelected;			/* Selected Rectangle 			   */
  POINT    pBaseSel;
  BOOL     InEdit;			/* TRUE if executing buffered edit function*/
  UINT 	   EdLeft, EdTop;	       	/* Pos where buffered edit begins          */
  FPOINTER EdStr;			/* Points to string being edited           */
  INT	   EdPos;		       	/* Cursor offset in edited string          */
  UINT 	   EdLength;			/* Current Length of edited string	   */
  BOOL	   Control, Shift;		/* TRUE if those keys are pressed	   */
  BOOL     LButtonDown;			/* TRUE if left button is Down 		   */
} TEXTWINDOWINFO ;

/* --------------------------------------------------------------------------
 * Some defined values:
 * ------------------------------------------------------------------------*/

#define MAX_LONG_BUFFER_ED     	 80*2  	/* Buffer edit max length           */
#define NUM_BUFFER_ED		   15   /* Number of Previous saved buffers */
#define KBDBUFFERSIZE	         1024  	/* Keyboard buffer size             */

#define PAGES		       	    2	/* Number of pages of the Window    */

#define FIRSTROW		    1   /* rows are in [1..twi->Rows]	    */
#define FIRSTCOL		    1   /* cols are in [1..twi->Cols]	    */

/* --------------------------------------------------------------------------
 * The color palette:
 * -------------------------------------------------------------------------*/


#define BACKGROUND      16
#define BRIGHT	       128
#define MAXCOLORS        8

static COLORREF thePalette[MAXCOLORS*2] = {
	 RGB(0,0,0),
	 RGB(0,0,175),
	 RGB(0,175,0),
	 RGB(0,175,175),
	 RGB(175,0,0),
	 RGB(175,0,175),
	 RGB(175,175,0),
	 RGB(255,255,255),
	 RGB(0,0,0),
	 RGB(0,0,255),
	 RGB(0,255,0),
	 RGB(0,255,255),
	 RGB(255,0,0),
	 RGB(255,0,255),
	 RGB(255,255,0),
	 RGB(255,255,255)
};

/*---------------------------------------------------------------------------
 * Register text window class:
 *-------------------------------------------------------------------------*/

BOOL RegisterTextClass(HINSTANCE hInstance) {
  WNDCLASS  wc;

  wc.style = CS_HREDRAW | CS_VREDRAW;
  wc.lpfnWndProc = TextWndProc;
  wc.cbWndExtra = (INT) sizeof(TEXTWINDOWINFO *);
  wc.cbClsExtra	= 0;
  wc.hInstance = hInstance;
  wc.hIcon = NULL;
  wc.hCursor = LoadCursor(NULL, IDC_IBEAM);
  wc.hbrBackground = GetStockObject(WHITE_BRUSH);
  wc.lpszMenuName = NULL;
  wc.lpszClassName = "TextWindow";

  return RegisterClass(&wc);
}

/*---------------------------------------------------------------------------
 * Screen structure in memory:
 *
 *  The screen is represented by an array of Rows*Columns where the ASCII
 * codes of every screen position are saved. There is another array to save
 * chars attributes. This represenatation makes possible a faster way
 * of painting the screen.
 *
 * How PAINT is done (See WM_PAINT):
 *
 *  First obtain invalidated rect. Then for every row to paint look count
 * consecutives chars with the same attribute. Then output them all together
 * with TextOut function. This makes the PAINT function faster.
 *-------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------
 * Create a text window:
 *-------------------------------------------------------------------------*/

HWND CreateTextWindow(HINSTANCE hInstance, HWND hParent, INT Left, INT Top,
		      UINT Columns, UINT Rows, HFONT hFont, HACCEL hAccelTable)
{
  HWND 		 hWnd;
  HDC		 hDC;
  HFONT		 hSaveFont;
  TEXTMETRIC 	 tm;
  INT		 CharHeight, CharWidth;
  TEXTWINDOWINFO *twi;
  HGLOBAL	 hScreen, hKbd, hBufferEd, hBufferEdPrev;
  FPOINTER	 Screen;

  hWnd = CreateWindow(
		"TextWindow",
		NULL,
		WS_CHILD | WS_VISIBLE | WS_HSCROLL | WS_VSCROLL,
		CW_USEDEFAULT,
		CW_USEDEFAULT,
		CW_USEDEFAULT,
		CW_USEDEFAULT,
		hParent,
		NULL,
		hInstance,
		NULL
  );

  Rows *= PAGES;

  if(!hWnd)
    return NULL;

  /* Get memory for text window structure */
  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  /* Get memory for screen buffer */
  hScreen = GlobalAlloc(GMEM_MOVEABLE, (DWORD) Rows*Columns*2);

  if(!hScreen)
    return NULL;

  /* Fill with spaces */
  Screen = (FPOINTER) GlobalLock(hScreen);
  MyMemSet (Screen, ' ', (ULONG)Rows*Columns);
  MyMemSet (Screen+(ULONG)Rows*Columns, BACKGROUND*WHITE+BLACK, (ULONG)Rows*Columns);
  GlobalUnlock(hScreen);

  /* Get memory for keyboard buffer */
  hKbd = GlobalAlloc(GMEM_MOVEABLE, (DWORD) (KBDBUFFERSIZE*sizeof(INT)));

  if(!hKbd)
    return NULL;

  /* Get memory for buffered Input */
  hBufferEd = GlobalAlloc(GMEM_MOVEABLE|GMEM_ZEROINIT,
			 (DWORD) (sizeof(CHAR)*(MAX_LONG_BUFFER_ED+1)));
  hBufferEdPrev = GlobalAlloc(GMEM_MOVEABLE|GMEM_ZEROINIT,
			     (DWORD) (sizeof(CHAR)*(MAX_LONG_BUFFER_ED+1)*NUM_BUFFER_ED));

  if (!hBufferEd || !hBufferEdPrev)
    return NULL;

  /* Get font dimmensions */
  hDC = GetDC(hParent);
  hSaveFont = SelectObject(hDC, hFont);

  GetTextMetrics(hDC, &tm);

  CharHeight = tm.tmHeight+tm.tmExternalLeading;
  CharWidth  = tm.tmAveCharWidth;

  SelectObject(hDC, hSaveFont);
  ReleaseDC(hParent, hDC);

  /* Fill in window structure */
  twi->Left         	= Left;
  twi->Top          	= Top;
  twi->CharWidth    	= CharWidth;
  twi->CharHeight   	= CharHeight;
  twi->Rows         	= Rows;
  twi->Cols         	= Columns;
  twi->RowsShowed	= 0;  /* Computed in WM_MOVE */
  twi->ColsShowed	= 0;  /* Computed in WM_MOVE */
  twi->PosX	    	= FIRSTCOL;
  twi->PosY	    	= FIRSTROW;
  twi->ScrBuffer    	= hScreen;
  twi->hFont	    	= hFont;
  twi->CursorStatus 	= TRUE;
  twi->InsertStatus 	= TRUE;
  twi->IsFocused    	= FALSE;
  twi->TextAttr      	= BLACK;
  twi->TextBackAttr 	= WHITE;
  twi->Bright	      	= FALSE;
  twi->KbdBuffer     	= hKbd;
  twi->KbdInx	      	= 0;
  twi->hAccelTable   	= hAccelTable;
  twi->hBufferEd     	= hBufferEd;
  twi->hBufferEdPrev 	= hBufferEdPrev;
  twi->IoInx	      	= -1;
  twi->AnsiPtr	      	= 0;
  twi->InAnsi	      	= FALSE;
  twi->VScroll 	   	= 0;
  twi->HScroll       	= 0;
  twi->Selecting     	= FALSE;
  twi->Selected	   	= FALSE;
  twi->rSelected.top 	= twi->rSelected.left
			= twi->rSelected.bottom
			= twi->rSelected.right
			= twi->pBaseSel.x
			= twi->pBaseSel.y = 0;
  twi->InEdit	      	= FALSE;
  twi->EdStr	      	= NULL;
  twi->Control 		= FALSE;
  twi->Shift 		= FALSE;
  twi->LButtonDown	= FALSE;

  /* Resize window */
  #define HINDENT	2
  #define VINDENT	2

  MoveWindow(hWnd, Left, Top,
		  CharWidth*Columns+2*HINDENT-1,
		  CharHeight*Rows/PAGES+2*VINDENT-1,
		  FALSE);
  ShowWindow(hWnd, SW_SHOW);
  UpdateWindow(hWnd);

  return hWnd;
}


/*---------------------------------------------------------------------------
 * Functions to get pointers to screen buffer:
 *-------------------------------------------------------------------------*/

#define GetScreenPos(ptr, col, row) GetScreenPos1(twi, ptr, col, row)
#define GetAttrPos(ptr, col, row)   GetAttrPos1(twi, ptr, col, row)

static FPOINTER GetScreenPos1(TEXTWINDOWINFO *twi, FPOINTER ptr, INT col, INT row)
{
  return ((ptr)+(ULONG)((ULONG)(twi->Cols)*(ULONG)(row-1)+((ULONG)(col-1))));
}

static FPOINTER GetAttrPos1(TEXTWINDOWINFO *twi, FPOINTER ptr, INT col, INT row)
{
  return (((ptr)+(ULONG)((ULONG)(twi->Cols)*(ULONG)(row-1)+((ULONG)(col-1))))+
	 ((ULONG)twi->Cols*(ULONG)twi->Rows));
}

/*---------------------------------------------------------------------------
 * Functions to ouput strings on the window:
 *-------------------------------------------------------------------------*/

#define IN_WM_PAINT 		1

/* Get DC and set font */
static VOID TextBeginPaint (BOOL InPaint, HWND hWnd, HDC *hDC, PAINTSTRUCT *ps, HFONT *hSaveFont)
{
  TEXTWINDOWINFO  *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  if (InPaint)
    *hDC = BeginPaint(hWnd, ps);
  else
    *hDC = GetDC (hWnd);

  *hSaveFont = SelectObject(*hDC, twi->hFont);
}

/* Restore old font and release DC */
static VOID TextEndPaint (BOOL InPaint, HWND hWnd, HDC *hDC, PAINTSTRUCT *ps, HFONT *hSaveFont)
{
  SelectObject(*hDC, *hSaveFont);

  if (InPaint)
    EndPaint(hWnd, ps);
  else
    ReleaseDC (hWnd, *hDC);
}

/* Outputs a string of length n with atribute Attr on the Window */
static VOID TextOutput (HWND hWnd, HDC hDC, UINT Col, UINT Row, LPSTR Str, INT n, UCHAR Attr)
{
  TEXTWINDOWINFO  *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  /* Set attributes */
  if (Attr & BRIGHT) {
    SetTextColor(hDC, thePalette[(Attr&0x07)+MAXCOLORS]);
    SetBkColor(hDC, thePalette[((Attr>>4)&0x07)+MAXCOLORS]);
  }
  else {
    SetTextColor(hDC, thePalette[Attr&0x07]);
    SetBkColor(hDC, thePalette[(Attr>>4)&0x07]);
  }

  TextOut(hDC,
	  twi->CharWidth*(Col-FIRSTCOL)+HINDENT,
	  twi->CharHeight*(Row-FIRSTROW)+VINDENT,
	  (LPCSTR) Str, n);
}

/* Print a region in the selected color. A region is a set of lines, specified
   with a RECT, and correspondes to a set of lines that is not really a rectangle */
VOID PutTextSelectedRegion (HWND hWnd, RECT aRect)
{
  HDC hDC;
  INT i;
  TEXTWINDOWINFO  *twi;
  FPOINTER BaseScreen;
  HFONT hSaveFont;
  PAINTSTRUCT ps;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  /* Check for limits */
  aRect.top    = max (FIRSTROW, aRect.top);
  aRect.left   = max (FIRSTCOL, aRect.left);
  aRect.bottom = min (twi->Rows, aRect.bottom);
  aRect.right  = min (twi->Cols, aRect.right);

  TextBeginPaint (!IN_WM_PAINT, hWnd, &hDC, &ps, &hSaveFont);

  BaseScreen = (FPOINTER) GlobalLock(twi->ScrBuffer);

  #define PRINT(Col, Row, Str, n)   TextOutput(hWnd, hDC,      \
				    Col-twi->HScroll,          \
				    Row-twi->VScroll,          \
				    ((LPSTR) (Str)),  	       \
				    (n),		       \
				    BACKGROUND*BLACK+WHITE+BRIGHT);

  /* Print first line of region */
  PRINT(aRect.left, aRect.top,
	GetScreenPos(BaseScreen, aRect.left, aRect.top),
	(aRect.bottom > aRect.top) ? twi->Cols-aRect.left+FIRSTCOL : aRect.right-aRect.left+FIRSTCOL);

  /* Print middle lines */
  for (i=aRect.top+1; i<aRect.bottom; i++) {

     PRINT(FIRSTCOL, i,
	   (LPCSTR)GetScreenPos(BaseScreen, FIRSTCOL, i),
	   twi->Cols);
  }

  /* Print last line */
  if (aRect.bottom > aRect.top) {
    PRINT(FIRSTCOL, aRect.bottom,
	  (LPCSTR)GetScreenPos(BaseScreen, FIRSTCOL, aRect.bottom),
	  aRect.right);
  }

  GlobalUnlock (twi->ScrBuffer);

  TextEndPaint (!IN_WM_PAINT, hWnd, &hDC, &ps, &hSaveFont);
}

/* Print a rectangle using the colors in the screen buffer */
static VOID PutTextRect (BOOL In_WM_PAINT, HWND hWnd, RECT aRect)
{
  TEXTWINDOWINFO *twi;
  HDC hDC;
  HFONT hSaveFont;
  PAINTSTRUCT ps;
  FPOINTER	  Screen, BaseScreen, ScreenAttr;
  UINT	          i, nChars;
  UINT	          currX, currCol;
  UCHAR	          Attr;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  /* Check for limits */
  aRect.top    = max (FIRSTROW, aRect.top);
  aRect.left   = max (FIRSTCOL, aRect.left);
  aRect.bottom = min (twi->Rows, aRect.bottom);
  aRect.right  = min (twi->Cols, aRect.right);

  TextBeginPaint (In_WM_PAINT, hWnd, &hDC, &ps, &hSaveFont);

  BaseScreen = (FPOINTER) GlobalLock(twi->ScrBuffer);

  for(i=aRect.top; i<=aRect.bottom; i++) {

    currCol = aRect.left;
    ScreenAttr = GetAttrPos(BaseScreen, aRect.left, i);
    Screen = GetScreenPos(BaseScreen, aRect.left, i);

    while (currCol <= aRect.right) {

	currX = currCol;

	Attr = *ScreenAttr;

	/* Count consecutive chars with same attribute */
	for(nChars=0; (currCol<=aRect.right)&&(*ScreenAttr==Attr); nChars++, currCol++, ScreenAttr++);

	TextOutput (hWnd, hDC, currX-twi->HScroll, i-twi->VScroll, (LPSTR) Screen, nChars, Attr);

	/* Go to next sequence of consecutive attributes chars */
	Screen += nChars;
    }
  }
  GlobalUnlock(twi->ScrBuffer);

  TextEndPaint (In_WM_PAINT, hWnd, &hDC, &ps, &hSaveFont);
}

/* Print a region using the colors in the screen buffer */
static VOID PutTextRegion (BOOL In_WM_PAINT, HWND hWnd, RECT aRect)
{
  RECT theRect;
  TEXTWINDOWINFO *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  /* Print first line */
  theRect.top    = aRect.top;
  theRect.bottom = aRect.top;
  theRect.left = aRect.left;
  if (aRect.bottom > aRect.top) {
    theRect.right = twi->Cols;
  }
  else {
    theRect.right = aRect.right;
  }
  PutTextRect (In_WM_PAINT, hWnd, theRect);

  /* Print middle lines */
  if (aRect.bottom > aRect.top) {
    theRect.top    = aRect.top+1;
    theRect.bottom = aRect.bottom-1;
    theRect.left = FIRSTCOL;
    theRect.right = twi->Cols;
    PutTextRect (In_WM_PAINT, hWnd, theRect);

    /* Print last line */
    theRect.top    = aRect.bottom;
    theRect.bottom = aRect.bottom;
    theRect.left = FIRSTCOL;
    theRect.right = aRect.right;
    PutTextRect (In_WM_PAINT, hWnd, theRect);

  }
}


/*---------------------------------------------------------------------------
 * Text class window WinProc:
 *-------------------------------------------------------------------------*/

#pragma argsused
static INT DoCreate (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  TEXTWINDOWINFO *twi;

  /* Get window structure */
  twi = (TEXTWINDOWINFO *) malloc(sizeof(TEXTWINDOWINFO));
  if (!twi)
    return -1;

  memset(twi, 0, sizeof(TEXTWINDOWINFO));
  SetWindowLong(hWnd, 0, (LONG) twi);

  return 1;
}

#pragma argsused
static VOID DoDestroy (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  TEXTWINDOWINFO *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  /* Free screen buffer */
  GlobalFree(twi->ScrBuffer);

  /* Free keyboard buffer */
  GlobalFree(twi->KbdBuffer);

  /* Free buffered input buffers */
  GlobalFree(twi->hBufferEd);
  GlobalFree(twi->hBufferEdPrev);

  /* Free window information structure */
  free(twi);
}

#pragma argsused
static VOID DoPaint (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  HDC	          hDC;
  PAINTSTRUCT     ps;
  TEXTWINDOWINFO  *twi;
  HFONT		  hSaveFont;
  RECT            aRect;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  TextBeginPaint (IN_WM_PAINT, hWnd, &hDC, &ps, &hSaveFont);

  /* Get rectangle to repaint */
  aRect.top = min((UINT)((ps.rcPaint.top-VINDENT)/(INT)twi->CharHeight)+FIRSTROW+(INT)twi->VScroll,
			   twi->Rows);
  aRect.bottom  = min((UINT)((ps.rcPaint.bottom-VINDENT-1)/(INT)twi->CharHeight)+FIRSTROW+(INT)twi->VScroll,
			  twi->Rows);

  aRect.left    = min((UINT)((ps.rcPaint.left-HINDENT)/(INT)twi->CharWidth)+FIRSTCOL+(INT)twi->HScroll,
			  twi->Cols);
  aRect.right   = min((UINT)((ps.rcPaint.right-HINDENT-1)/(INT)twi->CharWidth)+FIRSTCOL+(INT)twi->HScroll,
			  twi->Cols);

  PutTextRect (!IN_WM_PAINT, hWnd, aRect);

  TextEndPaint (IN_WM_PAINT, hWnd, &hDC, &ps, &hSaveFont);
}

#pragma argsused
static VOID DoSetFocus (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  TEXTWINDOWINFO  *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);
  twi->IsFocused = TRUE;
  WinSetcursor(hWnd, twi->CursorStatus);

  UnSelect(hWnd);
}

#pragma argsused
static VOID DoKillFocus (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  TEXTWINDOWINFO  *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);
  twi->IsFocused = FALSE;
  WinSetcursor(hWnd, twi->CursorStatus);

  UnSelect(hWnd);
}

#pragma argsused
static INT DoKeyDown (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  UINT		  i;
  static TCHAR 	  ControlKeys[] = {VK_UP, VK_DOWN,  VK_LEFT, VK_RIGHT,
				   VK_DELETE, VK_INSERT, VK_HOME, VK_END, 0};
  INT              VirtKey;
  TEXTWINDOWINFO  *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);
  VirtKey = (INT) wParam;

  if (VirtKey == VK_CANCEL && lParam & 0x1000000L) {
    PostMessage(GetParent(hWnd), WM_COMMAND, ID_STOP, 0L);
    twi->Control = FALSE;
    MessageBeep(-1);
    return 0;
  }
  else if (VirtKey == VK_CONTROL) {
    twi->Control = TRUE;
    return 0;
  }
  else if (VirtKey == VK_SHIFT) {
     twi->Shift = TRUE;
     return 0;
  }
  for(i=0; ControlKeys[i]; i++)
    if (VirtKey == (INT) ControlKeys[i] && lParam & 0x1000000L) {
      SendMessage(hWnd, WM_CHAR, wParam, lParam);
      return 0;
    }

  return 1;
}

#pragma argsused
static INT DoKeyUp (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) {
    TEXTWINDOWINFO* twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);
    twi->Control        = FALSE;
    twi->Shift          = FALSE;
    return 0;
}

#pragma argsused
static INT DoChar (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) {
  TEXTWINDOWINFO  *twi;
  INT far         *Kbd;
  TCHAR		   CharCode;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  CharCode = (TCHAR) wParam;

  /* Hot Keys to cut, paste ... */
  if (CharCode == VK_INSERT && twi->Control) {
    SendMessage(hWnd, WM_COPY, 0, 0L);
    return 0;
  }
  else if (CharCode == VK_INSERT && twi->Shift) {
    SendMessage(hWnd, WM_PASTE, 0, 0L);
    return 0;
  }
  else if (CharCode == VK_DELETE && twi->Control) {
    SendMessage(hWnd, WM_CLEAR, 0, 0L);
    return 0;
  }
  else if (CharCode == VK_DELETE && twi->Shift) {
    SendMessage(hWnd, WM_CUT, 0, 0L);
    return 0;
  }

  UnSelect(hWnd);

  if (twi->KbdInx >= KBDBUFFERSIZE-1)
    MessageBox(GetFocus(), "Keyboard buffer full", NULL, MB_ICONEXCLAMATION | MB_OK);
  else {
    if (lParam & 0x1000000L)  /* Control keys are negated */
      wParam = -wParam;
    Kbd = (INT *) GlobalLock(twi->KbdBuffer);
    Kbd[(twi->KbdInx)++] = (INT) wParam;
    GlobalUnlock(twi->KbdBuffer);
  }

  return 1;
}

#pragma argsused
static VOID DoSize (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) {
  TEXTWINDOWINFO  *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  /* Get number of rows and columns currently showed */
  if (twi->CharHeight)
    twi->RowsShowed = (HIWORD(lParam)-VINDENT)/twi->CharHeight;

  if (twi->CharWidth)
    twi->ColsShowed = (LOWORD(lParam)-HINDENT)/twi->CharWidth;

  /* Hide or show HSCROLL BAR */
  if (twi->ColsShowed < twi->Cols) {
    /* Small window */
    SetScrollRange(hWnd, SB_HORZ, 0, twi->Cols-twi->ColsShowed, TRUE);
    SetScrollPos(hWnd, SB_HORZ, twi->HScroll, TRUE);
  }
  else {
    /* Hide scroll bar */
    SetScrollRange(hWnd, SB_HORZ, 0, 0, TRUE);
    twi->HScroll = 0;
    SetScrollPos(hWnd, SB_HORZ, twi->HScroll, TRUE);
    WinGotoxy(hWnd, twi->PosX, twi->PosY);
  }

  /* Set Range for VSCROLL BAR */
  SetScrollRange(hWnd, SB_VERT, 0, twi->Rows-twi->RowsShowed, TRUE);
  if (twi->VScroll > twi->Rows-twi->RowsShowed) {
    twi->VScroll = twi->Rows-twi->RowsShowed;
    WinGotoxy(hWnd, twi->PosX, twi->PosY);
  }
  SetScrollPos(hWnd, SB_VERT, twi->VScroll, TRUE);
}

#pragma argsused
static VOID DoHScroll (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  TEXTWINDOWINFO  *twi;
  INT 		   SbMin, SbMax;


  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  switch(LOWORD(wParam)) {

    case SB_PAGEDOWN:
    case SB_LINEDOWN: {

      GetScrollRange(hWnd, SB_HORZ, &SbMin, &SbMax);

      if (twi->HScroll < SbMax) {
	twi->HScroll ++;
	SetScrollPos(hWnd, SB_HORZ, twi->HScroll, TRUE);
	ScrollWindow(hWnd, -twi->CharWidth, 0, NULL, NULL);

	UpdateWindow(hWnd);
	WinGotoxy(hWnd, twi->PosX, twi->PosY);
      }
    }
    break;

    case SB_PAGEUP:
    case SB_LINEUP: {

      RECT aRect;

      GetScrollRange(hWnd, SB_HORZ, &SbMin, &SbMax);

      if (twi->HScroll > SbMin) {
	twi->HScroll --;
	SetScrollPos(hWnd, SB_HORZ, twi->HScroll, TRUE);
	ScrollWindow(hWnd, twi->CharWidth, 0, NULL, NULL);

	GetClientRect(hWnd, &aRect);
	aRect.left = 0;
	aRect.right = twi->CharWidth+HINDENT;
	InvalidateRect(hWnd, &aRect, FALSE);

	UpdateWindow(hWnd);
	WinGotoxy(hWnd, twi->PosX, twi->PosY);

      }
    }
    break;

    case SB_THUMBPOSITION:{

      GetScrollRange(hWnd, SB_HORZ, &SbMin, &SbMax);

#if DOS
      twi->HScroll = LOWORD(lParam);
#else
      twi->HScroll = HIWORD(wParam);
#endif

      SetScrollPos(hWnd, SB_HORZ, twi->HScroll, TRUE);
      InvalidateRect(hWnd,NULL,TRUE);

      UpdateWindow(hWnd);
      WinGotoxy(hWnd, twi->PosX, twi->PosY);

    }
    break;
  }
}

#pragma argsused
static VOID DoVScroll (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  TEXTWINDOWINFO  *twi;
  INT 		   SbMin, SbMax;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  switch(LOWORD(wParam)) {

    case SB_PAGEDOWN:
    case SB_LINEDOWN: {

      GetScrollRange(hWnd, SB_VERT, &SbMin, &SbMax);

      if (twi->VScroll < SbMax) {
	twi->VScroll ++;
	SetScrollPos(hWnd, SB_VERT, twi->VScroll, TRUE);
	ScrollWindow(hWnd, 0, -twi->CharHeight, NULL, NULL);
	UpdateWindow(hWnd);
	WinGotoxy(hWnd, twi->PosX, twi->PosY);
      }
    }
    break;

    case SB_PAGEUP:
    case SB_LINEUP: {

      RECT aRect;

      GetScrollRange(hWnd, SB_VERT, &SbMin, &SbMax);

      if (twi->VScroll > SbMin) {
	twi->VScroll --;
	SetScrollPos(hWnd, SB_VERT, twi->VScroll, TRUE);
	ScrollWindow(hWnd, 0, twi->CharHeight, NULL, NULL);

	GetClientRect(hWnd, &aRect);
	aRect.top = 0;
	aRect.bottom = twi->CharHeight+VINDENT;
	InvalidateRect(hWnd, &aRect, FALSE);

	UpdateWindow(hWnd);
	WinGotoxy(hWnd, twi->PosX, twi->PosY);
      }
    }
    break;

    case SB_THUMBPOSITION:{

      GetScrollRange(hWnd, SB_VERT, &SbMin, &SbMax);

#if DOS
      twi->VScroll = LOWORD(lParam);
#else
      twi->VScroll = HIWORD(wParam);
#endif

      SetScrollPos(hWnd, SB_VERT, twi->VScroll, TRUE);
      InvalidateRect(hWnd,NULL,TRUE);
      UpdateWindow(hWnd);
      WinGotoxy(hWnd, twi->PosX, twi->PosY);
    }
    break;
  }
}

#pragma argsused
static VOID DoLButtonDown (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  INT		  xPos, yPos;
  TEXTWINDOWINFO *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  UnSelect(hWnd);

  xPos = LOWORD(lParam);
  yPos = HIWORD(lParam);

  /* Capture cursor */
  SetCapture(hWnd);

  twi->CursorStatus = WinSetcursor(hWnd, FALSE);
  twi->Selecting = TRUE;

  #define AsCol(xPos)   min((INT)twi->Cols, (((xPos)-HINDENT)/(INT)twi->CharWidth)+(INT)twi->HScroll+1)
  #define AsRow(yPos)   min((INT)twi->Rows, (((yPos)-VINDENT)/(INT)twi->CharHeight)+(INT)twi->VScroll+1)

  twi->pBaseSel.x = twi->rSelected.right = twi->rSelected.left = AsCol(xPos);
  twi->pBaseSel.y = twi->rSelected.bottom = twi->rSelected.top = AsRow(yPos);

  twi->LButtonDown = TRUE;
}

#pragma argsused
static VOID DoMouseMove (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  INT		   xPos, yPos, Row, Col;
  TEXTWINDOWINFO  *twi;
  RECT	     	   rClient, aRect;
  POINT            aPoint;
  FPOINTER         ScreenPosIni, ScreenPosEnd, BaseScreen;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  if (twi->Selecting) {

#if DOS
    xPos = LOWORD(lParam);
    yPos = HIWORD(lParam);
#else
    xPos = (INT)(SHORT)LOWORD(lParam);
    yPos = (INT)(SHORT)HIWORD(lParam);
#endif

    GetClientRect(hWnd, &rClient);

    /* Don't let xPos get out of client window */
    xPos = min((INT)rClient.right-2*HINDENT, xPos);
    xPos = max((INT)rClient.left, xPos);

    /* If yPos is out of client window Scroll selection */
    if (yPos < (INT)rClient.top) {
      yPos = 1;

      SendMessage(hWnd, WM_VSCROLL, SB_LINEUP, 0L);

      GetCursorPos(&aPoint); ScreenToClient(hWnd, &aPoint);
      if (twi->LButtonDown && (INT)aPoint.y < (INT)rClient.top) {

	PostMessage(hWnd, WM_MOUSEMOVE, MK_LBUTTON, MAKELPARAM((WORD)aPoint.x,(WORD)aPoint.y));
      }
    }
    else if (yPos > (INT)rClient.bottom-2*VINDENT) {
      yPos = (INT)rClient.bottom-2*VINDENT-1;

      SendMessage(hWnd, WM_VSCROLL, SB_LINEDOWN, 0L);

      GetCursorPos(&aPoint); ScreenToClient(hWnd, &aPoint);
      if (twi->LButtonDown && (INT)aPoint.y > (INT)rClient.bottom-2*VINDENT) {

	PostMessage(hWnd, WM_MOUSEMOVE, MK_LBUTTON, MAKELPARAM((WORD)aPoint.x,(WORD)aPoint.y));
      }
    }

    Col = AsCol(xPos);
    Row = AsRow(yPos);

    ScreenPosIni = GetScreenPos(BaseScreen, twi->pBaseSel.x, twi->pBaseSel.y);
    ScreenPosEnd = GetScreenPos(BaseScreen, Col, Row);

    if (ScreenPosIni < ScreenPosEnd) {

      ScreenPosIni = GetScreenPos(BaseScreen, twi->rSelected.right, twi->rSelected.bottom);
      ScreenPosEnd = GetScreenPos(BaseScreen, Col, Row);

      if (ScreenPosIni < ScreenPosEnd) {
	/* Selected rectangle has grown downwards */
	aRect.top    = twi->rSelected.top;
	aRect.left   = twi->rSelected.left;
	aRect.bottom = twi->pBaseSel.y;
	aRect.right  = twi->pBaseSel.x-1;
	PutTextRegion(!IN_WM_PAINT, hWnd, aRect);

	aRect.top    = twi->rSelected.bottom;
	aRect.left   = twi->rSelected.right;
	aRect.bottom = Row;
	aRect.right  = Col;
	PutTextSelectedRegion(hWnd, aRect);

	twi->rSelected.top    = twi->pBaseSel.y;
	twi->rSelected.left   = twi->pBaseSel.x;
	twi->rSelected.bottom = Row;
	twi->rSelected.right  = Col;
      }
      else if (ScreenPosIni > ScreenPosEnd) {
	/* Selected rectangle has shrinked upwards */
	aRect.top  = Row;
	aRect.left = Col;
	aRect.bottom  = twi->rSelected.bottom;
	aRect.right = twi->rSelected.right;
	PutTextRegion(!IN_WM_PAINT, hWnd, aRect);

	twi->rSelected.bottom  = Row;
	twi->rSelected.right = Col;
	twi->rSelected.top = twi->pBaseSel.y;
	twi->rSelected.left = twi->pBaseSel.x;
      }
    }
    else {
      ScreenPosIni = GetScreenPos(BaseScreen, twi->rSelected.left, twi->rSelected.top);
      ScreenPosEnd = GetScreenPos(BaseScreen, Col, Row);

      if (ScreenPosIni < ScreenPosEnd) {
	/* Selected rectangle has shrinked upwards */
	aRect.top  = twi->rSelected.top;
	aRect.left = twi->rSelected.left;
	aRect.bottom  = Row;
	aRect.right = Col;

	twi->rSelected.top  = Row;
	twi->rSelected.left = Col;
	twi->rSelected.bottom = twi->pBaseSel.y;
	twi->rSelected.right = twi->pBaseSel.x;

	PutTextRegion(!IN_WM_PAINT, hWnd, aRect);
      }
      else if (ScreenPosIni > ScreenPosEnd) {

	/* Selected rectangle has grown upwards */
	if (twi->pBaseSel.x == twi->Cols) {
	  aRect.top  = twi->pBaseSel.y+1;
	  aRect.left = 1;
	}
	else {
	  aRect.top  = twi->pBaseSel.y;
	  aRect.left = twi->pBaseSel.x+1;
	}
	aRect.bottom  = twi->rSelected.bottom;
	aRect.right = twi->rSelected.right;
	PutTextRegion (!IN_WM_PAINT, hWnd, aRect);

	aRect.top  = Row;
	aRect.left = Col;
	aRect.bottom  = twi->rSelected.top;
	aRect.right = twi->rSelected.left;
	PutTextSelectedRegion(hWnd, aRect);

	twi->rSelected.top  = Row;
	twi->rSelected.left = Col;
	twi->rSelected.bottom = twi->pBaseSel.y;
	twi->rSelected.right = twi->pBaseSel.x;
      }
    }
  }
}


#pragma argsused
static VOID DoLButtonUp (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  TEXTWINDOWINFO  *twi;
  INT		   xPos, yPos, Col, Row;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  twi->LButtonDown = FALSE;

  xPos = LOWORD(lParam);
  yPos = HIWORD(lParam);

  Col = AsCol(xPos);
  Row = AsRow(yPos);

  /* End selection */
  if (twi->Selecting) {

    twi->Selecting = FALSE;
    twi->Selected = TRUE;

    ReleaseCapture();

    /* Restore caret */
    WinSetcursor(hWnd, twi->CursorStatus);
  }

  /* Let change cursor position by clicking with cursor in Edit line */
  if (twi->InEdit) {

    FPOINTER InitPos, Pos;

    InitPos = (FPOINTER) GetScreenPos(0, twi->EdLeft, twi->EdTop);
    Pos = (FPOINTER) GetScreenPos(0, Col, Row);

    if (Pos >= InitPos && Pos-InitPos<=strlen((const char *)twi->EdStr)) {
      MoveCursor(hWnd, (INT) (Pos-InitPos-twi->EdPos));
      twi->EdPos = (INT) (Pos-InitPos);
    }
  }
}

#pragma argsused
static VOID DoRButtonDown (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  POINT		 ptCurrent;
  HMENU		 hMenu;

  ptCurrent.x = LOWORD(lParam);
  ptCurrent.y = HIWORD(lParam);
  ClientToScreen (hWnd, &ptCurrent);

  hMenu = CreatePopupMenu();
  AppendMenu (hMenu, MF_ENABLED|MF_STRING, ID_EVAL,  "&Evaluate");
  AppendMenu (hMenu, MF_ENABLED|MF_STRING, ID_INFO,  "&Info");
  AppendMenu (hMenu, MF_ENABLED|MF_STRING, ID_FIND,  "&Find");
  AppendMenu (hMenu, MF_ENABLED|MF_STRING, ID_TYPE,  "&Type");
  AppendMenu (hMenu, MF_SEPARATOR,         0, NULL);
  AppendMenu (hMenu, MF_ENABLED|MF_STRING, ID_CUT,   "Cu&t");
  AppendMenu (hMenu, MF_ENABLED|MF_STRING, ID_COPY,  "&Copy");
  AppendMenu (hMenu, MF_ENABLED|MF_STRING, ID_PASTE, "&Paste");
  AppendMenu (hMenu, MF_ENABLED|MF_STRING, ID_CLEAR, "C&lear");
  AppendMenu (hMenu, MF_SEPARATOR,         0, NULL);
  AppendMenu (hMenu, MF_ENABLED|MF_STRING, ID_OPENSELECTED, "&Load file");
  AppendMenu (hMenu, MF_ENABLED|MF_STRING, ID_EDITSELECTED, "E&dit file");

  TrackPopupMenu (hMenu, TPM_LEFTALIGN|TPM_LEFTBUTTON|TPM_RIGHTBUTTON,
		  ptCurrent.x, ptCurrent.y, 0, GetParent(hWnd), NULL);

  DestroyMenu(hMenu);
}


#pragma argsused
static VOID DoCopy (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  TEXTWINDOWINFO *twi;
  HGLOBAL	hData;
  INT		Size, i, Col;
  FPOINTER	BaseScreen, Screen, Data;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  if (twi->Selected) {
    if (!(BaseScreen = (FPOINTER)GlobalLock(twi->ScrBuffer)))
      return;

    Size = (INT) (GetScreenPos(BaseScreen, twi->rSelected.right, twi->rSelected.bottom) -
		  GetScreenPos(BaseScreen, twi->rSelected.left, twi->rSelected.top))
		  +1;

    if (!(hData = GlobalAlloc(GMEM_MOVEABLE, Size+1+2*(twi->rSelected.bottom-twi->rSelected.top)))) {
      GlobalUnlock(twi->ScrBuffer);
      return;
    }

    Screen = GetScreenPos(BaseScreen, twi->rSelected.left, twi->rSelected.top);

    if (!(Data = (FPOINTER)GlobalLock(hData))) {
      GlobalUnlock(twi->ScrBuffer);
      return;
    }

    for(i=0, Col=twi->rSelected.left; i<Size; i++, Col++) {
      if (Col > twi->Cols) {

	/* Erase final white spaces in line */
	while (Data[-1] == ' ')
	  Data--;

	*Data = '\r';
	Data++;
	*Data = '\n';
	Data++;
	Col = 1;
      }
      *Data = *Screen;
      Data++;
      Screen++;
    }
    *Data = '\0';

    GlobalUnlock(hData);
    GlobalUnlock(twi->ScrBuffer);

    if (OpenClipboard(hWnd)) {
      EmptyClipboard();
      SetClipboardData(CF_TEXT, hData);
      CloseClipboard();

      UnSelect(hWnd);
    }
  }
}

#pragma argsused
static VOID DoPaste (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  TEXTWINDOWINFO *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  if (OpenClipboard(hWnd) && twi->InEdit) {

    HGLOBAL	hClipData;
    FPOINTER    ClipData, p, BufferEd;
    INT		SizeClip, dest;
    INT		i, j;

    BufferEd = twi->EdStr;

    if (!(hClipData = GetClipboardData(CF_TEXT))) {
      CloseClipboard();
      return;
    }

    if (!(ClipData = (unsigned char HUGE *)GlobalLock(hClipData))) {
      CloseClipboard();
      return;
    }

    /* Lenght of string to insert */
    for(p=ClipData, SizeClip=0; *p; p++) {
      if (*p != '\n')
	SizeClip++;
    }

    /* Shift from cursor to end before inserting */
    dest = twi->EdPos+SizeClip;
    if (dest < MAX_LONG_BUFFER_ED) {
      i = min(twi->EdLength+SizeClip, MAX_LONG_BUFFER_ED-1);
      BufferEd[i+1] = '\0';
      while(i-SizeClip >= twi->EdPos) {
	BufferEd[i] = BufferEd[i-SizeClip];
	i--;
      }
    }
    else {
      BufferEd[MAX_LONG_BUFFER_ED] = '\0';
    }

    /* Insert string */
    for(i=twi->EdPos, j=0; i<MAX_LONG_BUFFER_ED && ClipData[j]; i++, j++) {
      if (ClipData[j] == '\r') {
	j++;
	BufferEd[i] = ' ';
      }
      else {
	BufferEd[i] = ClipData[j];
      }
    }

    /* Print edit line and place cursor */
    MoveCursor (hWnd, -twi->EdPos);
    WinPuts(hWnd, (CHAR *)BufferEd);

    twi->EdLength = strlen((const char *)BufferEd);
    MoveCursor(hWnd, -twi->EdLength+twi->EdPos);

    GlobalUnlock(hClipData);
    CloseClipboard();
  }
}


#pragma argsused
static VOID DoCutClear (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  TEXTWINDOWINFO *twi;
  FPOINTER        InitEdPos, FinalEdPos, InitSelPos, FinalSelPos;
  INT	          FirstCharToDelete, LastCharToDelete, i;
  FPOINTER        BufferEd;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  if (twi->Selected && twi->InEdit) {

    InitEdPos = (FPOINTER) GetScreenPos(0, twi->EdLeft, twi->EdTop);
    FinalEdPos = (FPOINTER) InitEdPos+twi->EdLength-1;

    InitSelPos = (FPOINTER) GetScreenPos(0, twi->rSelected.left, twi->rSelected.top);
    FinalSelPos = (FPOINTER) GetScreenPos(0, twi->rSelected.right, twi->rSelected.bottom);

    /* Check if selected text is in edit line */
    if (InitSelPos >= InitEdPos && FinalSelPos <= FinalEdPos) {

      if (message == WM_CUT) {
	SendMessage(hWnd, WM_COPY, 0, 0L);
      }

      /* Cut selected text from edit line */
      FirstCharToDelete = (INT) (InitSelPos-InitEdPos);
      LastCharToDelete = (INT) (FinalSelPos-InitEdPos);

      BufferEd = twi->EdStr;

      for(i=LastCharToDelete+1; BufferEd[i-1]; i++)
	BufferEd[i-(LastCharToDelete-FirstCharToDelete+1)] = BufferEd[i];

      MoveCursor(hWnd, -twi->EdPos);
      WinPuts(hWnd, (CHAR *)BufferEd);

      for(i=FirstCharToDelete; i<=LastCharToDelete; i++)
	WinPutchar(hWnd, ' ');

      /* Place cursor */
      if (twi->EdPos > FirstCharToDelete) {
	twi->EdPos -= (LastCharToDelete-FirstCharToDelete);
      }

      MoveCursor(hWnd, -twi->EdLength+twi->EdPos);

      twi->EdLength = strlen((const char *)BufferEd);

      UnSelect(hWnd);
    }
  }
}

#pragma argsused
static BOOL DoCanCopy (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  TEXTWINDOWINFO *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  return twi->Selected;

}

#pragma argsused
static BOOL DoCanCut (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  TEXTWINDOWINFO *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  if (twi->Selected && twi->InEdit) {
    FPOINTER  InitEdPos, FinalEdPos, InitSelPos, FinalSelPos;

    InitEdPos = (FPOINTER) GetScreenPos(0, twi->EdLeft, twi->EdTop);
    FinalEdPos = (FPOINTER) InitEdPos+twi->EdLength-1;

    InitSelPos = (FPOINTER) GetScreenPos(0, twi->rSelected.left, twi->rSelected.top);
    FinalSelPos = (FPOINTER) GetScreenPos(0, twi->rSelected.right, twi->rSelected.bottom);

    /* Check if selected text is in edit line */
    return (InitSelPos >= InitEdPos && FinalSelPos <= FinalEdPos);

  }
  else {
    return FALSE;
  }
}

#pragma argsused
static BOOL DoCanPaste (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  BOOL		 CanPaste;

  if (OpenClipboard(hWnd)) {
    CanPaste = IsClipboardFormatAvailable(CF_TEXT) ||
	       IsClipboardFormatAvailable(CF_OEMTEXT);
    CloseClipboard();
  }

  return CanPaste;
}

#pragma argsused
static BOOL DoCanClear (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  return DoCanCut (hWnd, message, wParam, lParam);
}


LRESULT FAR APIENTRY _export TextWndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
  switch (message)
  {
     case WM_CREATE:      return DoCreate (hWnd, message, wParam, lParam);

     case WM_DESTROY:     DoDestroy (hWnd, message, wParam, lParam);
			  break;

     case WM_PAINT:       DoPaint (hWnd, message, wParam, lParam);
			  break;

     case WM_SETFOCUS:    DoSetFocus (hWnd, message, wParam, lParam);
			  break;

     case WM_KILLFOCUS:   DoKillFocus (hWnd, message, wParam, lParam);
			  break;

     case WM_KEYDOWN:     return DoKeyDown (hWnd, message, wParam, lParam);

     case WM_KEYUP:       return DoKeyUp (hWnd, message, wParam, lParam);

     case WM_CHAR: 	  return DoChar (hWnd, message, wParam, lParam);

     case WM_SIZE:        DoSize (hWnd, message, wParam, lParam);
			  break;

     case WM_HSCROLL:     DoHScroll (hWnd, message, wParam, lParam);
			  break;

     case WM_VSCROLL:     DoVScroll (hWnd, message, wParam, lParam);
			  break;

     case WM_LBUTTONDOWN: DoLButtonDown (hWnd, message, wParam, lParam);
			  break;

     case WM_MOUSEMOVE:   DoMouseMove (hWnd, message, wParam, lParam);
			  break;

     case WM_LBUTTONUP:   DoLButtonUp (hWnd, message, wParam, lParam);
			  break;

     case WM_RBUTTONDOWN: DoRButtonDown (hWnd, message, wParam, lParam);
			  break;

     case WM_COPY:        DoCopy (hWnd, message, wParam, lParam);
			  break;

     case WM_PASTE:       DoPaste (hWnd, message, wParam, lParam);
			  break;

     case WM_CUT:
     case WM_CLEAR:       DoCutClear (hWnd, message, wParam, lParam);
			  break;

     case WM_CANCOPY:     return DoCanCopy (hWnd, message, wParam, lParam);

     case WM_CANPASTE:    return DoCanPaste (hWnd, message, wParam, lParam);

     case WM_CANCLEAR:    return DoCanClear (hWnd, message, wParam, lParam);

     case WM_CANCUT:      return DoCanCut (hWnd, message, wParam, lParam);

     default:		  return DefWindowProc(hWnd, message, wParam, lParam);
    }

    return(1L);
}


/* Returns a pointer to the text currently selected in the Window */
CHAR *GetSelectedText (HWND hWnd)
{
  #define  MAXSIZE	1024
  static UCHAR    Buffer[MAXSIZE];
  FPOINTER        InitSelPos, FinalSelPos, BaseScreen;
  INT             i;
  TEXTWINDOWINFO *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  if (!DoCanCopy(hWnd, 0, 0, 0L))
    return NULL;

  /* Copy selected text to buffer */
  BaseScreen = (FPOINTER) GlobalLock(twi->ScrBuffer);
  InitSelPos = (FPOINTER) GetScreenPos(BaseScreen, twi->rSelected.left, twi->rSelected.top);
  FinalSelPos = (FPOINTER) GetScreenPos(BaseScreen, twi->rSelected.right, twi->rSelected.bottom);

  for (i=0; i<MAXSIZE-1 && &InitSelPos[i]<=FinalSelPos; ++i) {
    Buffer[i] = InitSelPos[i];
  }
  Buffer[i] = (UCHAR) 0;
  GlobalUnlock(twi->ScrBuffer);

  return (CHAR *) Buffer;
}


static VOID UnSelect(HWND hWnd)
{
  TEXTWINDOWINFO  *twi;
  RECT		  rInvalidate;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  if (twi->Selected) {

   rInvalidate.top = 0;
   rInvalidate.bottom = VINDENT;
   rInvalidate.left = 0;
   rInvalidate.right = 2*HINDENT+(INT)twi->CharWidth*(INT)twi->Cols;
   InvalidateRect(hWnd, &rInvalidate, TRUE);

   rInvalidate.top = (INT)twi->CharHeight*(twi->rSelected.top-1-(INT)twi->VScroll)+VINDENT;
   rInvalidate.left = (INT)twi->CharWidth*(-(INT)twi->HScroll)+HINDENT;
   rInvalidate.bottom = (INT)twi->CharHeight*(twi->rSelected.bottom-(INT)twi->VScroll)+VINDENT;
   rInvalidate.right = (INT)twi->CharWidth*((INT)twi->Cols-(INT)twi->HScroll)+HINDENT;

   InvalidateRect(hWnd, &rInvalidate, FALSE);
   UpdateWindow(hWnd);

   twi->Selected = FALSE;

  }
}


/*---------------------------------------------------------------------------
 * Interprete an ANSI sequence:
 *-------------------------------------------------------------------------*/
#define ESCAPE  27
static CHAR *Ansi(HWND hWnd, CHAR *str)
{
  CHAR 	         *p = str;
  INT 		  nums[20], numscnt=0;
  TEXTWINDOWINFO *twi;

  /* Check if it is an ANSI sequence */
  if (*p != '[') {
    WinPutchar(hWnd, '\27');
    return str;
  }

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  p++; /* pass '[' */

  if (isdigit(*p)) {
    another:
      sscanf (p, "%d", &nums[numscnt++]);
      while(isdigit(*p)) p++; /* pass number readed */
      if (*p == ';') {
	p++;
	goto another;
      }
  }
  else {
    nums[0] = 1;
    nums[1] = 1;
  }


  switch (*p) {
    case 'A': WinGotoxy(hWnd, WinWherex(hWnd), WinWherey(hWnd)-nums[0]);
	      break;

    case 'B': WinGotoxy(hWnd, WinWherex(hWnd), WinWherey(hWnd)+nums[0]);
	      break;

    case 'C': WinGotoxy(hWnd, WinWherex(hWnd)+nums[0], WinWherey(hWnd)+nums[0]);
	      break;

    case 'D': WinGotoxy(hWnd, WinWherex(hWnd)-nums[0], WinWherey(hWnd)+nums[0]);
	      break;

    case 'f':
    case 'H': WinGotoxy(hWnd, nums[1], nums[0]);
	      break;

    case 'J': if (nums[0] == 2) {
		WinClrscr(hWnd);
		WinGotoxy(hWnd, 1, 1);
	      }
	      break;

   case 'K':  WinClreol(hWnd);
	      break;

   case 'R': WinGotoxy(hWnd, nums[1], nums[0]);
	     twi->AnsiSavex = WinWherex(hWnd);
	     twi->AnsiSavey = WinWherey(hWnd);
	     break;

   case 's': twi->AnsiSavex = WinWherex(hWnd);
	     twi->AnsiSavey = WinWherey(hWnd);
	     break;

   case 'u': WinGotoxy(hWnd, twi->AnsiSavex, twi->AnsiSavey);
	     break;

   case 'm': {
	       int i;

	       if (!numscnt) {
		 nums[0] = 0;
		 numscnt++;
	       }

	       for(i=0; i<numscnt; i++) {
		 switch (nums[i]) {
		   case 0: WinTextcolor(hWnd, BLACK);
			   WinTextbackground(hWnd, WHITE);
			   WinTextbright(hWnd, FALSE);
			   break;

		   case 1:
		   case 2: WinTextbright(hWnd, FALSE);
			   break;

		   case 5:
		   case 6: WinTextbright(hWnd, TRUE);
			   break;

		   case 7: {
			     int back, fore;

			     back = twi->TextBackAttr;
			     fore  = twi->TextAttr;

			     twi->TextBackAttr = fore;
			     twi->TextAttr = back;

			   }
			   break;

		   case 30: WinTextcolor(hWnd,BLACK);
			    break;
		   case 31: WinTextcolor(hWnd,RED);
			    break;
		   case 32: WinTextcolor(hWnd,GREEN);
			    break;
		   case 33: WinTextcolor(hWnd,YELLOW);
			    break;
		   case 34: WinTextcolor(hWnd,BLUE);
			    break;
		   case 35: WinTextcolor(hWnd,MAGENTA);
			    break;
		   case 36: WinTextcolor(hWnd,CYAN);
			    break;
		   case 37: WinTextcolor(hWnd,WHITE);
			    break;
		   case 40: WinTextbackground(hWnd,BLACK);
			    break;
		   case 41: WinTextbackground(hWnd,RED);
			    break;
		   case 42: WinTextbackground(hWnd,GREEN);
			    break;
		   case 43: WinTextbackground(hWnd,YELLOW);
			    break;
		   case 44: WinTextbackground(hWnd,BLUE);
			    break;
		   case 45: WinTextbackground(hWnd,MAGENTA);
			    break;
		   case 46: WinTextbackground(hWnd,CYAN);
			    break;
		   case 47: WinTextbackground(hWnd,WHITE);
			    break;
		 }
	       }
	       break;
	     }
  }
  return p;
}



/*---------------------------------------------------------------------------
 * Write chars or string to the window:
 *-------------------------------------------------------------------------*/

/* print a string */
INT WinPuts (HWND hWnd, CHAR *str)
{
  INT n = 0;

  for (;*str;str++, n++)
    WinPutchar (hWnd, str[0]);

  return n;
}

/* print a character to screen */
INT WinPutchar (HWND hWnd, CHAR c)
{
  TEXTWINDOWINFO  *twi;
  FPOINTER        Screen, BaseScreen, ScreenAttr;
  FPOINTER        dest, source, lastline;
  UINT 		  nLinesScrolled=0;
  UCHAR		  Attr;
  CHAR 		 *p;
  HDC		  hDC;
  HFONT		  hSaveFont;
  PAINTSTRUCT     ps;
  BOOL		  saveCurs = WinSetcursor(hWnd, FALSE);

  #define ScrollUp    { dest = BaseScreen;               				\
											\
			lastline = dest + (twi->Rows-1)*twi->Cols;      		\
											\
			source = dest + twi->Cols;                      		\
			MyMemMove(dest, source, ((ULONG)(twi->Rows-1)*twi->Cols));	\
			MyMemSet(lastline, ' ', (ULONG)twi->Cols);			\
											\
			dest = BaseScreen + (twi->Rows*twi->Cols);			\
			lastline = dest + (twi->Rows-1)*twi->Cols;			\
											\
			source = dest + twi->Cols;                      		\
			MyMemMove(dest, source, ((ULONG)(twi->Rows-1)*twi->Cols));	\
			MyMemSet(lastline, Attr, (ULONG)twi->Cols);			\
											\
			nLinesScrolled++;                               		\
			twi->EdTop--;							\
											\
			ScrollWindow (hWnd, 0, -twi->CharHeight, NULL, NULL);		\
		      }

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  /* Get current attribute for the window */
  Attr = (UCHAR) (twi->TextAttr+BACKGROUND*twi->TextBackAttr);
  if (twi->Bright)
    Attr += (UCHAR) BRIGHT;

  BaseScreen = (FPOINTER) GlobalLock(twi->ScrBuffer);

  Screen = GetScreenPos(BaseScreen, twi->PosX, twi->PosY);
  ScreenAttr = GetAttrPos(BaseScreen, twi->PosX, twi->PosY);

  if (twi->InAnsi) {
     twi->AnsiStr[twi->AnsiPtr++] = c;
     if (isalpha(c)) {
       twi->AnsiStr[twi->AnsiPtr] = (CHAR) 0;
       twi->AnsiPtr = 0;
       p = Ansi(hWnd, twi->AnsiStr);
       p++;
       twi->InAnsi = FALSE;
       if (*p)
	 WinPuts (hWnd, p);
     }
  }
  else if (c == ESCAPE) {
     twi->InAnsi = TRUE;
  }
  else if (c == '\b') {
     if(twi->PosX > 1) {
       twi->PosX--;
     }
  }
  else if (c == '\n') {
     twi->PosX = 1;
     twi->PosY++;

     if (twi->PosY-twi->VScroll > twi->RowsShowed) {
       SendMessage(hWnd, WM_VSCROLL, SB_LINEDOWN, 0L);
     }

     if (twi->PosY > twi->Rows) {
       ScrollUp;
       twi->PosY--;
       UpdateWindow(hWnd);
     }
  }
  else if (c == '\t') {
     WinPuts(hWnd, "        ");
  }
  else {
     /* Print the char */
     *ScreenAttr = Attr;
     *Screen = c;

     TextBeginPaint (!IN_WM_PAINT, hWnd, &hDC, &ps, &hSaveFont);

     TextOutput (hWnd, hDC, twi->PosX-twi->HScroll, twi->PosY-twi->VScroll, &c, 1, Attr);

     TextEndPaint (!IN_WM_PAINT, hWnd, &hDC, &ps, &hSaveFont);

     twi->PosX++;

     /* Move cursor */
     if (twi->PosX > twi->Cols) {
       twi->PosX = 1;
       twi->PosY++;

       if (twi->PosY-twi->VScroll > twi->RowsShowed) {
	 SendMessage(hWnd, WM_VSCROLL, SB_LINEDOWN, 0L);
       }

       if (twi->PosY > twi->Rows) {
	 ScrollUp;
	 twi->PosY--;
	 UpdateWindow(hWnd);
       }
     }
  }

  GlobalUnlock(twi->ScrBuffer);

  WinSetcursor(hWnd, saveCurs);
  WinGotoxy(hWnd, twi->PosX, twi->PosY);

  return c;
}


/*---------------------------------------------------------------------------
 * Change text color:
 *-------------------------------------------------------------------------*/
VOID WinTextcolor(HWND hWnd, INT Color)
{
  TEXTWINDOWINFO  *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  twi->TextAttr = Color;
}


/*---------------------------------------------------------------------------
 * Change Background color:
 *-------------------------------------------------------------------------*/
VOID WinTextbackground(HWND hWnd, INT Color)
{
  TEXTWINDOWINFO  *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  twi->TextBackAttr = Color;
}


/*---------------------------------------------------------------------------
 * Set Bright:
 *-------------------------------------------------------------------------*/
void WinTextbright(HWND hWnd, BOOL Brillo)
{
  TEXTWINDOWINFO  *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  twi->Bright = Brillo;
}


/*---------------------------------------------------------------------------
 * Move cursor:
 *-------------------------------------------------------------------------*/
VOID WinGotoxy(HWND hWnd, UINT x, UINT y)
{

 TEXTWINDOWINFO  *twi;

 twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

 #if defined __DEBUG__
 if (x < 0 || x >= twi->Cols || y < 0 || y >= twi->Rows) {
  MessageBox(GetFocus, "Error in parameters: WinGotoxy", NULL, MB_OK);
  return;
 }
 #endif

 twi->PosX = x;
 twi->PosY = y;

 if (twi->CursorStatus) {
   if (twi->IsFocused) {
     if (!twi->InsertStatus)
       SetCaretPos(twi->CharWidth*(x-1-twi->HScroll)+HINDENT, twi->CharHeight*(y-1-twi->VScroll)+(twi->CharHeight/2)+VINDENT);
     else
       SetCaretPos(twi->CharWidth*(x-1-twi->HScroll)+HINDENT, twi->CharHeight*(y-1-twi->VScroll)+VINDENT);
     ShowCaret(hWnd);
   }
 }
}

/* Get cursor position */
UINT WinWherex(HWND hWnd)
{
  TEXTWINDOWINFO  *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  return twi->PosX;
}

UINT WinWherey(HWND hWnd)
{
  TEXTWINDOWINFO  *twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  return twi->PosY;
}

/* Set cursor ON/OFF */
BOOL WinSetcursor(HWND hWnd, BOOL NewState)
{
  TEXTWINDOWINFO  *twi;
  BOOL OldState;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  OldState = twi->CursorStatus;
  twi->CursorStatus = NewState;

  if (!(twi->IsFocused)) {
   DestroyCaret();
   return OldState;
  }

  if (twi->CursorStatus) {
    if (twi->InsertStatus) {
      CreateCaret(hWnd, NULL, twi->CharWidth, twi->CharHeight);
    }
    else {
      CreateCaret(hWnd, NULL, twi->CharWidth, (twi->CharHeight)/2);
    }
    WinGotoxy(hWnd, twi->PosX, twi->PosY);
  }
  else {
    DestroyCaret();
  }

  return OldState;
}

/* Set insert ON/OFF */
BOOL WinSetinsert(HWND hWnd, BOOL NewState)
{
  TEXTWINDOWINFO  *twi;
  BOOL OldState;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  OldState = twi->InsertStatus;

  twi->InsertStatus = NewState;

  /* Set cursor shape */
  WinSetcursor(hWnd, twi->CursorStatus);

  return OldState;
}



/*---------------------------------------------------------------------------
 * True if a key is pushed:
 *-------------------------------------------------------------------------*/
BOOL WinKbhit(HWND hWnd)
{

 MSG 		  msg;
 TEXTWINDOWINFO  *twi;

 twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

 while (PeekMessage(&msg, 0, 0, 0, PM_REMOVE)) {
   if (!TranslateAccelerator(GetParent(GetFocus()), twi->hAccelTable, &msg)) {
     TranslateMessage(&msg);
     DispatchMessage(&msg);
   }
 }

 return (twi->KbdInx > 0);
}

/* Get a key. If no one is available yields control to Windows */
TCHAR WinGetch(HWND hWnd)
{
  MSG              msg;
  INT              readKey;
  TEXTWINDOWINFO  *twi;
  INT   far       *Kbd;
  INT		   i;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  otherKey:
  while (!(twi->KbdInx)) {
    while (PeekMessage(&msg, 0, 0, 0, PM_REMOVE)) {
      if (!TranslateAccelerator(GetParent(GetFocus()), twi->hAccelTable, &msg)) {
	TranslateMessage(&msg);
	DispatchMessage(&msg);
      }
    }
  }

  Kbd = (INT *) GlobalLock(twi->KbdBuffer);

  readKey = Kbd[0];
  for (i=0; i<twi->KbdInx; i++)
    Kbd[i] = Kbd[i+1];
  twi->KbdInx--;

  GlobalUnlock(twi->KbdBuffer);

  /* Insert key changes cursor but can't be read */
  if (readKey<0 && (int)-readKey == VK_INSERT) {
    twi->InsertStatus = !(twi->InsertStatus);
    WinSetinsert(hWnd, twi->InsertStatus);
    goto otherKey;
  }

  return (TCHAR) readKey;
}

/* Allows to check for Break key */
BOOL WinAllowBreak(HWND hWnd)
{

 #define CHECK_BREAK_EVERY         500
 static INT n = CHECK_BREAK_EVERY;

 if (n <= 0) {
   WinKbhit(hWnd);
   n = CHECK_BREAK_EVERY;
 }
 else {
   n--;
 }

 return TRUE;
}


/*---------------------------------------------------------------------------
 * Clear screen with current attribute:
 *-------------------------------------------------------------------------*/
VOID WinClrscr(HWND hWnd)
{
  TEXTWINDOWINFO  	*twi;
  FPOINTER		 Screen;
  UCHAR			 Attr;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  Attr = (UCHAR)(twi->TextAttr+BACKGROUND*twi->TextBackAttr);
  if (twi->Bright)
    Attr += (UCHAR)BRIGHT;

  Screen = (FPOINTER) GlobalLock(twi->ScrBuffer);

  MyMemSet (Screen, ' ', (ULONG)(twi->Rows*twi->Cols));
  MyMemSet (Screen+(ULONG)twi->Rows*twi->Cols, BACKGROUND*WHITE+BLACK, (ULONG)(twi->Rows*twi->Cols));

  GlobalUnlock(twi->ScrBuffer);

  WinGotoxy(hWnd, 1, 1);

  InvalidateRect(hWnd, NULL, FALSE);
  UpdateWindow(hWnd);
}


/*---------------------------------------------------------------------------
 * Clear to end of current line:
 *-------------------------------------------------------------------------*/
VOID WinClreol(HWND hWnd)
{
  TEXTWINDOWINFO  	*twi;
  UINT			 i, nChars;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  nChars = twi->Cols-twi->PosX+1;
  for(i=0; i<nChars; i++)
   WinPutchar(hWnd, ' ');
}


/* Moves cursor n positions forward or backwards depending on sign of n */
static VOID MoveCursor (HWND hWnd, INT n)
{
  INT x, y, i, Width;
  TEXTWINDOWINFO  	*twi;

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  Width = twi->Cols;

  x = twi->PosX;
  y = twi->PosY;

  if (n<0) {
    for (i=-n; i; i--)
      if (x > 1)
	x--;
      else {
	if (y>1) {
	  x = Width;
	  y--;
	}
      }
  }
  else {
    for (i=0; i<n; i++) {
      if (x < Width)
	x++;
      else {
	x = 1;
	y++;
      }
    }
  }

  WinGotoxy(hWnd, x, y);
}


/* --------------------------------------------------------------------------
 * Emulates Keyboard input with Edit capability
 *
 *  Standard C buffered edition plus
 *
 *  VK_UP :    Get previous edit buffer
 *  VK_DOWN:   Get next edit buffer
 *  VK_LEFT:   Move cursor left
 *  VK_RIGHT:  Move cursor right
 *  VK_DELETE: Delete a char
 *  VK_BACK:   Delete char at left of cursor
 *  VK_INSERT: Switch Insert/Overwrite mode
 *  VK_HOME:   Move cursor to begin of edit buffer
 *  VK_END:    Move cursor to end of edit buffer
 *  VK_RETURN: Accept current buffer as input
 *  VK_ESCAPE: Clear current buffer
 * ------------------------------------------------------------------------*/

CHAR *WinGets(HWND hWnd, CHAR *s)
{
 UINT   	 j, savex, savey;
 BOOL		 iscursor;
 INT 		 PreviousBuffer = -1;
 TCHAR 		 Key;
 TEXTWINDOWINFO *twi;
 FPOINTER        BufferEdPrev;
#define bufferEdPrev(_i) (&BufferEdPrev[(_i)*(MAX_LONG_BUFFER_ED+1)])

 twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

 BufferEdPrev = (FPOINTER) GlobalLock(twi->hBufferEdPrev);
 iscursor = WinSetcursor(hWnd, TRUE);
 twi->InEdit = TRUE;
 twi->EdStr = (FPOINTER)s;
 twi->EdLeft = WinWherex(hWnd);
 twi->EdTop  = WinWherey(hWnd);

 loop:
 twi->EdPos = 0;
 twi->EdLength = strlen((const CHAR*)s);

 WinPuts(hWnd, (CHAR *)s);

 MoveCursor (hWnd, -twi->EdLength);

 for (;;) {

   Key = WinGetch(hWnd);

   #define IsControl(x)  	((x)<0)
   if (IsControl(Key))

     switch (-Key) {

       case VK_UP:    if (PreviousBuffer+1 < NUM_BUFFER_ED &&
			  bufferEdPrev(PreviousBuffer+1)[0]!='\0') {
			PreviousBuffer++;
			MoveCursor (hWnd, -twi->EdPos);
			for (j=0; j<twi->EdLength; j++) WinPutchar(hWnd, ' ');
			MoveCursor (hWnd, -twi->EdLength);
			strcpy (s, (const CHAR *)bufferEdPrev(PreviousBuffer));
			twi->EdLength = strlen (s);
			goto loop;
		      }
		      break;

       case VK_DOWN:  if (PreviousBuffer > 0) {
			PreviousBuffer--;
			MoveCursor (hWnd, -twi->EdPos);
			for (j=0; j<twi->EdLength; j++) WinPutchar(hWnd, ' ');
			MoveCursor (hWnd, -twi->EdLength);
			strcpy (s, (const CHAR *)bufferEdPrev(PreviousBuffer));
			twi->EdLength = strlen (s);
			goto loop;
		      }
		      break;


      case VK_DELETE: if (s[twi->EdPos]) {
			for (j=twi->EdPos; j<=twi->EdLength; j++)
			  s[j] = s[j+1];

			savex = WinWherex(hWnd);
			savey = WinWherey(hWnd);

			WinPuts(hWnd, &s[twi->EdPos]);
			WinPutchar(hWnd, ' ');
			WinGotoxy(hWnd, savex, savey);
			twi->EdLength--;
		      }
		      break;

      case VK_LEFT:   if (twi->EdPos > 0) {
			MoveCursor (hWnd, -1);
			twi->EdPos--;
		      }
		      break;

      case VK_RIGHT:  if (s[twi->EdPos] && twi->EdPos < twi->EdLength) {
			MoveCursor (hWnd, +1);
			twi->EdPos++;
		      }
		      break;

      case VK_HOME:  MoveCursor(hWnd, -twi->EdPos);
		     twi->EdPos=0;
		     break;

      case VK_END:   MoveCursor(hWnd, twi->EdLength-twi->EdPos);
		     twi->EdPos = twi->EdLength;
		     break;

     }
   else

     switch(Key) {

      case VK_ESCAPE: MoveCursor (hWnd, -twi->EdPos);
		      for (j=0; j<twi->EdLength; j++)
			   WinPutchar(hWnd, ' ');
		      MoveCursor (hWnd, -twi->EdLength);
		      s[0]          = '\0';
		      twi->EdLength = 0;
		      goto loop;

      case VK_BACK: if (twi->EdLength > 0 && twi->EdPos > 0) {
		      for (j=twi->EdPos; j<=twi->EdLength; j++)
			s[j-1] = s[j];
		      MoveCursor (hWnd, -1);
		      savex = WinWherex(hWnd);
		      savey = WinWherey(hWnd);

		      WinPuts(hWnd, &s[twi->EdPos-1]);
		      WinPutchar(hWnd, ' ');
		      WinGotoxy(hWnd, savex, savey);
		      twi->EdPos--;
		      twi->EdLength--;
		    }
		    break;


      case VK_RETURN:
		    WinSetcursor (hWnd, iscursor);
		    MoveCursor (hWnd, twi->EdLength-twi->EdPos+1);
		    twi->IoInx = 0;
		    GlobalUnlock(twi->hBufferEdPrev);
		    twi->InEdit = FALSE;
		    twi->EdStr = NULL;

		    return s;


      default:      if (((twi->EdLength+1 > MAX_LONG_BUFFER_ED) && twi->InsertStatus) ||
			 (twi->EdPos == MAX_LONG_BUFFER_ED)) {
		      break;
		    }

		    if (twi->InsertStatus) {
		      for (j=MAX_LONG_BUFFER_ED; j>twi->EdPos; j--)
			s[j] = s[j-1];
		      twi->EdLength++;
		    }

		    if (s[twi->EdPos]==0) {
		      s[twi->EdPos] = Key;
		      s[twi->EdPos+1] = 0;
		      if (!(twi->InsertStatus))
			twi->EdLength++;
		    }
		    else {
		      s[twi->EdPos] = Key;
		    }

		    WinPuts(hWnd, &s[twi->EdPos]);

		    MoveCursor (hWnd, -(twi->EdLength-twi->EdPos)+1);

		    twi->EdPos++;
		    break;
     }
   } /* end for */
#undef bufferEdPrev
}


/* Read a char from a file */
INT WinGetc (HWND hWnd, FILE *fp) {
  INT		   ret;
  TEXTWINDOWINFO  *twi;
  FPOINTER	   BufferEd;
  FPOINTER         BufferEdPrev;
#define bufferEdPrev(_i) (&BufferEdPrev[(_i)*(MAX_LONG_BUFFER_ED+1)])
  /* Check if stdin */
  if (fp != stdin)
    return fgetc(fp);

  twi = (TEXTWINDOWINFO*) GetWindowLong(hWnd, 0);

  BufferEd = (FPOINTER)GlobalLock(twi->hBufferEd);
  BufferEdPrev = (FPOINTER) GlobalLock(twi->hBufferEdPrev);

  while (twi->IoInx < 0) { /* If BufferEd is empty fill it */

    if (*((CHAR *)BufferEd)!='\0') {
     int i;
     for (i=NUM_BUFFER_ED-1; i > 0; i--) {
       strcpy ((CHAR *) bufferEdPrev(i), (const CHAR *)bufferEdPrev(i-1));
     }
     strcpy ((CHAR *) bufferEdPrev(0), (const CHAR *)BufferEd);
    }

    strcpy ((CHAR *)BufferEd, "");
    WinGets (hWnd, (CHAR *)BufferEd);
    WinPutchar (hWnd, '\n');
  }

  if (BufferEd[twi->IoInx] == 0) {
    twi->IoInx = -1;
    ret = '\n';
  }
  else {
    ret =  (BufferEd[(twi->IoInx)++]);
  }

  GlobalUnlock(twi->hBufferEd);
  GlobalUnlock(twi->hBufferEdPrev);

  return ret;
#undef bufferEdPrev
}


/* I use this to redirect output to stdstrbuff using             */
/* putc(stdstr, ...), fprintf(stdstr, ...) */
INT   StrInx = 0;
#define MAX_STDSTR 255
FILE *stdstr = NULL;
CHAR  stdstrbuff[MAX_STDSTR];

/* print a character to a stream */
INT WinPutc(HWND hWnd, CHAR c, FILE *fp)
{
  /* Output to window */
  if (fp == stdout || fp == stderr) {
    WinPutchar (hWnd, c);
  }
  /* Output to string */
  else if (fp == stdstr) {
    if (c=='\n') {
      stdstrbuff[StrInx] = (CHAR) 0;
      StrInx = 0;
    }
    else
      stdstrbuff[StrInx++] = c;
  }
  /* Output to stream */
  else {
    fputc(c, fp);
  }
  return c;
}

/* Like fprintf for DOS, but if fp == stdout or stderr output goes to */
/* the window. If fp == stdstr output goes to the string stdstrbuf    */
INT cdecl WinFprintf(HWND hWnd, FILE *fp, const CHAR *format, ...)
{
  CHAR 		buf[2000];
  va_list	arg_ptr;
  INT 		cnt;

  va_start (arg_ptr, format);

  /* Output to the string */
  if (fp == stdstr) {
    cnt = vsprintf (&stdstrbuff[StrInx], format, arg_ptr);
    if (stdstrbuff[StrInx+cnt-1] == '\n') {
      stdstrbuff[StrInx+cnt-1] = 0;
      StrInx = 0;
    }
    else
      StrInx += cnt;
    va_end (arg_ptr);
    return cnt;
  }

  /* Output to a stream */
  if (fp != stdout && fp != stderr) {
    cnt = vfprintf (fp, format, arg_ptr);
    va_end (arg_ptr);
    return cnt;
  }

  /* Output to the window */
  cnt = vsprintf (buf, format, arg_ptr);
  va_end (arg_ptr);

  WinPuts(hWnd, buf);

  return cnt;

}


/* Like printf for DOS but output goes to window */
INT cdecl WinPrintf(HWND hWnd, const CHAR *format, ...)
{
  CHAR 		buf[2000];
  va_list 	arg_ptr;
  INT 		cnt;

  va_start (arg_ptr, format);

  cnt = vsprintf (buf, format, arg_ptr);
  va_end (arg_ptr);

  WinPuts(hWnd, buf);

  return cnt;
}


/* Like WinFprintf but output goes to text window */
INT cdecl TextWinFprintf (FILE *fp, const CHAR * format, ...)
{
  CHAR 		buf[2000];
  va_list	arg_ptr;
  INT 		cnt;

  va_start (arg_ptr, format);

  if (fp == stdstr) {
    cnt = vsprintf (&stdstrbuff[StrInx], format, arg_ptr);
    if (stdstrbuff[StrInx+cnt-1] == '\n') {
      stdstrbuff[StrInx+cnt-1] = 0;
      StrInx = 0;
    }
    else
      StrInx += cnt;
    va_end (arg_ptr);
    return cnt;
  }

  if (fp != stdout && fp != stderr) {
    cnt = vfprintf (fp, format, arg_ptr);
    va_end (arg_ptr);
    return cnt;
  }

  cnt = vsprintf (buf, format, arg_ptr);
  va_end (arg_ptr);

  WinPuts(hWndText, buf);

  return cnt;
}

/* Like WinPrintf but output goes to text window */
INT cdecl TextWinPrintf (const CHAR * format, ...)
{
  CHAR 		buf[2000];
  va_list 	arg_ptr;
  INT 		cnt;

  va_start (arg_ptr, format);

  cnt = vsprintf (buf, format, arg_ptr);
  va_end (arg_ptr);

  WinPuts(hWndText, buf);

  return cnt;
}


/* --------------------------------------------------------------------------
 * Some static functions:
 * ------------------------------------------------------------------------*/

/* Move n bytes from src to dest */
static VOID MyMemMove (FPOINTER dest, FPOINTER src, ULONG n)
{
  ULONG BytesLeft, nBytes;

  BytesLeft = n;

  do {
    nBytes = min (BytesLeft, 32767U);
#if DOS
    _fmemmove((void far *) dest, (void far *) src, (size_t)nBytes);
#else
    memmove((void far *) dest, (void far *) src, (size_t)nBytes);
#endif
    dest += nBytes;
    src  += nBytes;
    BytesLeft -= nBytes;
  } while (BytesLeft);

}

/* Set n bytes to value at dest */
static VOID MyMemSet (FPOINTER dest, CHAR value, ULONG n)
{
  FPOINTER end;

  for (end=dest+n; dest<end; dest++)
    *dest = value;
}

