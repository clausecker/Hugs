/* --------------------------------------------------------------------------
 * WinHugs.c:	José Enrique Gallardo Ruiz, April 1995
 *		With mods by mpj/adr for Hugs 98, December 1998
 *
 * Hugs for Windows is Copyright (c) José Enrique Gallardo, Mark P Jones,
 * Alastair Reid and the Yale Haskell Group 1994-99, and is distributed as
 * Open Source software under the Artistic License; see the file "Artistic"
 * that is included in the distribution for details.
 *
 * This file contains functions for a MS-Windows GUI for Hugs
 * it is included into the end of hugs.c
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Windows includes
 * ------------------------------------------------------------------------*/

#include <ctl3d.h>
#include <commdlg.h>
#include <dir.h>
#include <stdarg.h>

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static HBITMAP local CreateStatusBar	     Args((HWND));
static VOID    local DoAbout 	   	     Args((Void));
static VOID    local DoCommand 	   	     Args((HWND, UINT, WPARAM, LPARAM));
static VOID    local DoBrowseClasses	     Args((HWND, UINT, WPARAM, LPARAM));
static VOID    local DoBrowseTycons	     Args((HWND, UINT, WPARAM, LPARAM));
static VOID    local DoBrowseNames	     Args((HWND, UINT, WPARAM, LPARAM));
static INT     local DoCreate 	   	     Args((HWND, UINT, WPARAM, LPARAM));
static VOID    local DoDestroy 	   	     Args((HWND, UINT, WPARAM, LPARAM));
static VOID    local DoGetMinMaxInfo         Args((HWND, UINT, WPARAM, LPARAM));
static VOID    local DoLButtonDown 	     Args((HWND, UINT, WPARAM, LPARAM));
static VOID    local DoLButtonUp 	     Args((HWND, UINT, WPARAM, LPARAM));
static VOID    local DoMenuSelect 	     Args((HWND, UINT, WPARAM, LPARAM));
static VOID    local DoMouseMove 	     Args((HWND, UINT, WPARAM, LPARAM));
static VOID    local DoOptions 	   	     Args((Void));
static VOID    local DoPaint		     Args((HWND, UINT, WPARAM, LPARAM));
static VOID    local DoSize		     Args((HWND, UINT, WPARAM, LPARAM));
static VOID    local DoInitMenu		     Args((HWND, UINT, WPARAM, LPARAM));
static Void    local DrawToolBar	     Args((HWND));
static CHAR*   local GetaFileName	     Args((HWND, UINT));
static VOID    local Hierarchy	   	     Args((VOID));
static BOOL    local InitApplication         Args((VOID));
static BOOL    local InitInstance 	     Args((LPSTR, INT));
static Void    local LoadProfileOptions      Args((VOID));
static Void    local PutHelpLine	     Args((HWND, LPCSTR));
static Void    local SaveProfileOptions      Args((VOID));
static Void    local pushButton              Args((HWND));
static Void    local unpushButton            Args((HWND));
static Void    local PutBitmap               Args((HBITMAP, HWND, INT, INT, DWORD));
					     
static VOID    local ScriptManager           Args((HWND, UINT, WPARAM, LPARAM));
static Void    local sendString              Args((HWND, const char*, ...));
					     
static String  local describeName            Args((Name));
static Bool    local isParentOf              Args((Class,Class));
					     
static Void    local buildClassGraph         Args((HDC));
static INT     local addClassToGraph         Args((HDC,INT,INT,Class));
					     
static Void    local SetClass		     Args((HWND, Class));
static Void    local SetName		     Args((HWND, UINT, List));
static Void    local SetTycon		     Args((HWND, UINT, List));
	       
static Void    local doInitDialog_Names      Args((HWND));
static Void    local doDestroy_Names         Args((Void));
static Void    local doMeasureItem_Names     Args((MEASUREITEMSTRUCT FAR*));
static INT     local doDrawItem_Names        Args((HWND, DRAWITEMSTRUCT FAR *));
static Void    local doPaint_Names           Args((HWND));
					
static Bool    local doCommand_Names         Args((HWND, WORD, WORD));

static VOID    local setClassBrowserSize     Args((Void));
static VOID    local doCreate_Classes        Args((HWND));
static VOID    local doDestroy_Classes       Args((Void));
static Void    local doMove_Classes          Args((INT,INT));
static Void    local doSize_Classes          Args((HWND,INT,INT));
static Void    local doPaint_Classes         Args((HWND));
static Void    local setOffset_Classes       Args((HWND,INT,INT));
static Void    local lButtonDown_Classes     Args((HWND,INT,INT));
static Void    local lButtonUp_Classes       Args((HWND,INT,INT));
static Void    local doMouseMove_Classes     Args((HWND,INT,INT));
static Void    local doGetMinMaxInfo_Classes Args((MINMAXINFO FAR*));

/* --------------------------------------------------------------------------
 * Nonlocal function prototypes:
 * ------------------------------------------------------------------------*/

INT APIENTRY WinMain	Args((HANDLE, HANDLE, LPSTR, INT));

/* --------------------------------------------------------------------------
 * CALLBACK functions prototypes:
 * ------------------------------------------------------------------------*/

BOOL    FAR APIENTRY _export AboutDlgProc	  (HWND, UINT, WPARAM, LPARAM);
BOOL    FAR APIENTRY _export BrowseClassesDlgProc (HWND, UINT, WPARAM, LPARAM);
BOOL    FAR APIENTRY _export BrowseTyconsDlgProc  (HWND, UINT, WPARAM, LPARAM);
BOOL    FAR APIENTRY _export BrowseNamesDlgProc   (HWND, UINT, WPARAM, LPARAM);
BOOL    FAR APIENTRY _export ScriptManDlgProc	  (HWND, UINT, WPARAM, LPARAM);

LRESULT FAR APIENTRY _export MainWndProc 	  (HWND, UINT, WPARAM, LPARAM);
LRESULT FAR APIENTRY _export ClassesWndProc       (HWND, UINT, WPARAM, LPARAM);
BOOL    FAR APIENTRY _export OptionsDlgProc	  (HWND, UINT, WPARAM, LPARAM);

#define 	ON	TRUE
#define 	OFF	FALSE

#define ROWS		25
#define COLS 		80		/* Default Windows size in chars   */

/* --------------------------------------------------------------------------
 * Debugging tool:
 * ------------------------------------------------------------------------*/

#if DEBUG
static Void local showMessage Args((HWND, const char*, ...));
static Void local showMessage(HWND hWnd, const char *fmt, ...) {
    va_list ap;                    /* pointer into argument list           */
    static char msg[1000];

    va_start(ap, fmt);             /* make ap point to first arg after fmt */
    vsprintf(msg, fmt, ap);

    MessageBox(hWnd, msg, NULL, MB_ICONEXCLAMATION | MB_OK);
    va_end(ap);                    /* clean up                             */
}
#endif

/* --------------------------------------------------------------------------
 * Local Variables:
 * ------------------------------------------------------------------------*/

	HANDLE	hThisInstance;          /* Windows instance of application */
	HWND	hWndMain=NULL;		/* Main Window handle              */
	HWND	hWndText=NULL;		/* Text Window handle   	   */
	HWND	hWndClasses=NULL;	/* Class Hierarchy Window handle   */

static	HANDLE  hAccelTable;		/* Accelerators table		   */
static 	HFONT   theFont=NULL;		/* Text Font to use in text window */
static 	HFONT   theErrorFont=NULL;   	/* Text Font for error messagess   */
static 	HCURSOR NormalCursor=NULL;   	/* Normal mouse cursor		   */
static 	HCURSOR HandCursor=NULL; 	/* Tool Bar mouse cursor	   */
static 	HCURSOR GarbageCursor=NULL;	/* GC mouse cursor		   */
static 	HCURSOR SaveCursor;             /* Used to save and restore cursor */
static 	INT 	WindowWidth=0,
		WindowHeight=0;		/* Size of Main Window		   */
static  INT	NMenus;			/* Number of items in the menu	   */
static	INT	ScreenRows = 0,
		ScreenCols = 0;         /* Text Window size in chars	   */

String appName = "Hugs 98 for Windows";	/* Text for application name	   */

#define MAXBUF 300
CHAR buffer[MAXBUF];

/* --------------------------------------------------------------------------
 * Lower status line implementation:
 * ------------------------------------------------------------------------*/

RECT 	RectStatus;		/* Status Line position in main window     */
HBITMAP bmpStatusBar=NULL;	/* Status line Bitmap           	   */
#define STATUS_HEIGHT  19       /* Status line height           	   */
#define EDIT_LEFT     -120	/* Horizontal offset of "Editor:"          */

/* Draws the Status line on screen */
static VOID local DrawStatusLine(HWND hWnd) {
    HDC     hDC;
    RECT    rc;
    LOGFONT lf;
    HFONT   hSmall, hOld;
    INT     OldBkMode;
    CHAR    szMsg1[300];
    CHAR    drive[MAXDRIVE];
    CHAR    dir[MAXDIR];
    CHAR    file[MAXFILE];
    CHAR    ext[MAXEXT];

    PutBitmap(bmpStatusBar,hWnd,RectStatus.left,RectStatus.top,SRCCOPY);

    hDC = GetDC(hWnd);

    /* Get a small font */
    hOld = (HFONT) GetStockObject(ANSI_VAR_FONT);
    if (hOld) {
	GetObject(hOld,sizeof(LOGFONT),(LPSTR)&lf);
	lf.lfHeight = -1;
	lf.lfWidth  = 0;
	lf.lfWeight = FW_NORMAL;
	hSmall      = CreateFontIndirect(&lf);
    }
    else
	hSmall = 0;

    hOld      = hSmall ? ((HFONT) SelectObject(hDC, hSmall)) : 0;
    OldBkMode = SetBkMode(hDC, TRANSPARENT);
    rc.left   = RectStatus.right+EDIT_LEFT+12;
    rc.right  = RectStatus.right-7;
    rc.top    = RectStatus.top+4;
    rc.bottom = RectStatus.bottom-2;

    if (lastEdit) {
	fnsplit(lastEdit,drive,dir,file,ext);
	wsprintf(szMsg1,"Edit: %s%s%s",drive,file,ext);
    }
    else
	wsprintf(szMsg1,"Edit: %s","None");

    DrawText(hDC,(LPCSTR)szMsg1,strlen((CHAR *)szMsg1),&rc,DT_LEFT);
    SetBkMode(hDC,OldBkMode);

    if (hOld)
	SelectObject(hDC, hOld);
    if (hSmall)
	DeleteObject(hSmall);

    ReleaseDC(hWnd, hDC);
}

static HBITMAP local CreateStatusBar(HWND hWnd) {
    HBITMAP hbmpOldSrc, hbmpOldDest;	/* Create a bitmap for the status   */
    HBITMAP hbmpNew, hbmpSrc;		/* line, according to window size   */
    HDC     hdcSrc, hdcDest, hDC;
    BITMAP  bmp;

    hDC     = GetDC(hWnd);
    hdcSrc  = CreateCompatibleDC(hDC);
    hdcDest = CreateCompatibleDC(hDC);
    ReleaseDC(hWnd,hDC);

    hbmpSrc    = LoadBitmap(hThisInstance,"STATUSBAR");
    GetObject(hbmpSrc,sizeof(BITMAP),&bmp);
    hbmpOldSrc = SelectObject(hdcSrc,hbmpSrc);
    hbmpNew    = CreateCompatibleBitmap(hdcSrc,
					RectStatus.right-RectStatus.left+1,
					RectStatus.bottom-RectStatus.top+1);
    hbmpOldDest = SelectObject(hdcDest, hbmpNew);
    StretchBlt(hdcDest,0,0,RectStatus.right-RectStatus.left+1,
			   RectStatus.bottom-RectStatus.top+1,
	       hdcSrc,0,0, bmp.bmWidth, bmp.bmHeight,
	       SRCCOPY);
    SelectObject(hdcSrc,hbmpOldSrc);
    DeleteObject(hbmpSrc);

    hbmpSrc = LoadBitmap(hThisInstance,"STATUSBARSEP");
    GetObject(hbmpSrc,sizeof(BITMAP),&bmp);
    hbmpOldSrc = SelectObject(hdcSrc,hbmpSrc);
    StretchBlt(hdcDest,RectStatus.right+EDIT_LEFT,0, bmp.bmWidth,
			    RectStatus.bottom-RectStatus.top+1,
	       hdcSrc,0,0, bmp.bmWidth, bmp.bmHeight,
	       SRCCOPY);
    SelectObject(hdcSrc, hbmpOldSrc);
    DeleteObject(hbmpSrc);

    SelectObject(hdcDest, hbmpOldDest);
    DeleteDC(hdcDest);
    DeleteDC(hdcSrc);
    return hbmpNew;
}

static Void local PutHelpLine(HWND hWnd, LPCSTR szMsg) {
#if DOS
    INT     iLen = _fstrlen(szMsg);	/* Print message szMsg on the status*/
#else
    INT     iLen = strlen(szMsg);	/* Print message szMsg on the status*/
#endif
    HDC     hDC  = GetDC(hWnd);		/* line of window hWnd		    */
    RECT    rc;
    LOGFONT lf;
    HFONT   hSmall, hOld;
    INT     OldBkMode;

    if (hOld=(HFONT)GetStockObject(ANSI_VAR_FONT)) {	/* get small font  */
	GetObject(hOld,sizeof(LOGFONT),(LPSTR)&lf);
	lf.lfHeight = -1;
	lf.lfWidth  = 0;
	lf.lfWeight = FW_NORMAL;
	hSmall      = CreateFontIndirect(&lf);
    }
    else
	hSmall = 0;

    hOld      = hSmall ? (HFONT)(SelectObject(hDC,hSmall)) : 0;
    OldBkMode = SetBkMode (hDC, TRANSPARENT);
    rc.left   = RectStatus.left+5;
    rc.right  = RectStatus.right+EDIT_LEFT-2;
    rc.top    = RectStatus.top+4;
    rc.bottom = RectStatus.bottom-2;
    FillRect(hDC,&rc,GetStockObject(LTGRAY_BRUSH));
    DrawText(hDC,szMsg,iLen,&rc,DT_LEFT);
    SetBkMode(hDC,OldBkMode);

    if (hOld)
	SelectObject(hDC, hOld);
    if (hSmall)
	DeleteObject(hSmall);
    ReleaseDC(hWnd, hDC);

    return;
}

/* --------------------------------------------------------------------------
 * Toolbar implementation:
 * ------------------------------------------------------------------------*/

static RECT RectTools;		/* Tool bar position                        */
static INT  Button_Height;	/* Size of one button                      */
static INT  Button_Width;
static INT  nButtons;		/* Number of buttons in the tool bar	   */
static INT  buttonPushed = -1;
static Bool buttonDown   = FALSE;

#define TOOLS_WIDTH	(Button_Width+9)               /* Tool bar width   */
#define TOOLS_BK_COLOR  RGB(192,192,192)               /* Tool bar color   */

typedef struct tagButton {	/* One button on the tool bar:		   */
    WPARAM Msg;			/* Command to execute when pushed	   */
    BOOL   IsEnabled;		/* Is the button enabled?		   */
    LPCSTR Bitmap;		/* Bitmap name for the button		   */
    UINT   IdHelpLine;		/* Help to get when pointed		   */
} hButton;

static hButton theButtons[] = {	/* The tool bar buttons			   */
    {ID_HELPINDEX,       TRUE,  "HELPBUTTON",      ID_HELPINDEX},
    {ID_SCRIPTMAN,    	 TRUE,  "SCRIPTMANBUTTON", ID_SCRIPTMAN},
    {ID_RELOAD,          TRUE,  "RELOADBUTTON",    ID_RELOAD},
    {ID_COPY,        	 TRUE,  "COPYBUTTON",      ID_COPY},
    {ID_CUT,         	 TRUE,  "CUTBUTTON",	   ID_CUT},
    {ID_PASTE,       	 TRUE,  "PASTEBUTTON",     ID_PASTE},
    {ID_CLEAR,       	 TRUE,  "DELETEBUTTON",    ID_CLEAR},
    {ID_RUN,	       	 TRUE,  "RUNBUTTON",	   ID_RUN},
    {ID_STOP,       	 TRUE,  "STOPBUTTON",      ID_STOP},
    {ID_GOEDIT,       	 TRUE,  "EDITBUTTON",      ID_GOEDIT},
    {ID_SETOPTIONS,      TRUE,  "OPTIONSBUTTON",   ID_SETOPTIONS},
    {ID_BROWSEHIERARCHY, TRUE,  "HIERARCHYBUTTON", ID_BROWSEHIERARCHY},
    {ID_EXIT,        	 TRUE,  "EXITBUTTON",      ID_EXIT},
    {0, 0, 0, 0}
};

static VOID local DrawToolBar(HWND hWnd) { /* Draw tool bar on the screen  */
    HDC     hDC = GetDC(hWnd);
    HBRUSH  hBrush, hOldBrush;
    HPEN    hPen, hOldPen;
    HBITMAP hBitmap1, hBitmap2;
    INT     i;

    hBrush    = CreateSolidBrush(TOOLS_BK_COLOR);
    hOldBrush = SelectObject(hDC,hBrush);
    FillRect(hDC,&RectTools,hBrush);
    SelectObject(hDC,hOldBrush);
    DeleteObject(hBrush);

    hPen    = CreatePen(PS_SOLID,1,RGB(0,0,0));
    hOldPen = SelectObject(hDC,hPen);
    MoveToEx(hDC,RectTools.right-1,RectTools.top,NULL);
    LineTo(hDC,RectTools.right-1,RectTools.bottom);
    SelectObject(hDC, hOldPen);
    DeleteObject(hPen);

    hBitmap1 = LoadBitmap(hThisInstance,"BUTTON");
    for (i=0; theButtons[i].Msg; i++) {
	PutBitmap(hBitmap1,hWnd,RectTools.left+4,
				RectTools.top+(i*Button_Height)+1,
				SRCCOPY);
	hBitmap2 = LoadBitmap(hThisInstance, theButtons[i].Bitmap);
	PutBitmap(hBitmap2,hWnd,RectTools.left+7,
				RectTools.top+(i*Button_Height)+2+1,
				SRCCOPY);
	DeleteObject(hBitmap2);
	if (!theButtons[i].IsEnabled) {
	    hBitmap2 = LoadBitmap(hThisInstance,"DISABLED");
	    PutBitmap(hBitmap2,hWnd,RectTools.left+7,
				    RectTools.top+(i*Button_Height)+2+1,
				    SRCAND);
	    DeleteObject(hBitmap2);
	}
    }
    DeleteObject(hBitmap1);
    ReleaseDC(hWnd, hDC);
}

static Void local pushButton(HWND hWnd) {
    if (!buttonDown) {
	HBITMAP hBitmap = LoadBitmap(hThisInstance,"PUSHEDBUTTON");
	PutBitmap(hBitmap,hWnd,RectTools.left+4,
			       RectTools.top+(buttonPushed*Button_Height)+1,
			       SRCCOPY);
	DeleteObject(hBitmap);
	hBitmap = LoadBitmap(hThisInstance, theButtons[buttonPushed].Bitmap);
	PutBitmap(hBitmap,hWnd,RectTools.left+8,
			       RectTools.top+(buttonPushed*Button_Height)+3+1,
			       SRCCOPY);
	DeleteObject(hBitmap);
	buttonDown = TRUE;
    }
}

static Void local unpushButton(HWND hWnd) {
    if (buttonDown) {
	HBITMAP hBitmap = LoadBitmap(hThisInstance,"BUTTON");
	PutBitmap(hBitmap,hWnd,RectTools.left+4,
			       RectTools.top+(buttonPushed*Button_Height)+1,
			       SRCCOPY);
	DeleteObject(hBitmap);
	hBitmap = LoadBitmap(hThisInstance,theButtons[buttonPushed].Bitmap);
	PutBitmap(hBitmap,hWnd,RectTools.left+7,
			       RectTools.top+(buttonPushed*Button_Height)+2+1,
			       SRCCOPY);
	DeleteObject(hBitmap);
	buttonDown = FALSE;
    }
}

/* --------------------------------------------------------------------------
 * Other MS-DOS functions emulation:
 * ------------------------------------------------------------------------*/

INT WinSystem(const char *s) {
    ERRTEXT "\nSorry, this version of Hugs does not support shell escpaes."
    EEND;
    return 0;
}

/* --------------------------------------------------------------------------
 * Responses to messages:
 * ------------------------------------------------------------------------*/

static HWND ahwndSubMenus[16];

/* Browse Classes ... */
#pragma argsused
static VOID local DoBrowseClasses(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  DLGPROC lpProcBrowse;

  lpProcBrowse = (DLGPROC)MakeProcInstance((FARPROC)BrowseClassesDlgProc, hThisInstance);
  DialogBox(hThisInstance,MAKEINTRESOURCE(BROWSECLASSESDLGBOX),GetFocus(),lpProcBrowse);
#if DOS
  FreeProcInstance(lpProcBrowse);
#endif
}

/* Browse Type Constructors ... */
#pragma argsused
static VOID local DoBrowseTycons(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  DLGPROC lpProcBrowse;

  lpProcBrowse = (DLGPROC)MakeProcInstance((FARPROC)BrowseTyconsDlgProc, hThisInstance);
  DialogBox(hThisInstance, MAKEINTRESOURCE(BROWSETYCONSDLGBOX), GetFocus(), lpProcBrowse);
#if DOS
  FreeProcInstance(lpProcBrowse);
#endif
}

/* Browse Names ... */
#pragma argsused
static VOID local DoBrowseNames(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  DLGPROC lpProcBrowse;

  lpProcBrowse = (DLGPROC)MakeProcInstance((FARPROC)BrowseNamesDlgProc, hThisInstance);
  DialogBox(hThisInstance, MAKEINTRESOURCE(BROWSENAMESDLGBOX), GetFocus(), lpProcBrowse);
#if DOS
  FreeProcInstance(lpProcBrowse);
#endif
}

/* Script Manager */
#pragma argsused
static VOID local ScriptManager(HWND hWnd, UINT msg,
				WPARAM wParam, LPARAM lParam) {
  DLGPROC dlgProc = (DLGPROC)MakeProcInstance((FARPROC)ScriptManDlgProc,
					      hThisInstance);
  DialogBox(hThisInstance,
	    MAKEINTRESOURCE(SCRIPTMANDLGBOX),
	    GetFocus(),
	    dlgProc);
#if DOS
  FreeProcInstance(dlgProc);
#endif
}

static Void local sendString(HWND hWnd, const char *fmt, ...) {
  va_list ap;                    /* pointer into argument list           */
  static char msg[1000];
  String s;

  va_start(ap, fmt);             /* make ap point to first arg after fmt */
  vsprintf(msg, fmt, ap);

  for(s=msg; *s; ++s) {
    if (*s == '\n') {
      SendMessage(hWnd, WM_CHAR, (WPARAM) VK_RETURN, 0L);
    } else {
      SendMessage(hWnd, WM_CHAR, (WPARAM) *s, 0L);
    }
  }

  va_end(ap);                    /* clean up                             */
}

/* Response to Menu Commands */
#pragma argsused
static VOID local DoCommand(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    switch (wParam) {
	/* Load script from disk (name of the script is currently selected */
	case ID_OPENSELECTED: {
	    String File = GetSelectedText(hWndText);
	    input(BREAK);
	    WinPuts(hWndText, "\n");
	    setLastEdit(File, 0);
	    if (projectLoaded) {
		if (MessageBox(GetFocus(),
			       "Close current loaded project?",
			       appName,
			       MB_ICONQUESTION|MB_YESNO)!=IDYES) {
		    whatScripts();
		    longjmp(catch_error, 1);
		}
		clearProject();
		forgetScriptsFrom(1);
	    }
	    addScriptName(File,TRUE);
	    readScripts(1);
	    longjmp(catch_error, 1);
	    break;
	}

	case ID_EXIT:			/* Exit hugs interpreter 	   */
	    SendMessage(hWnd, WM_CLOSE, 0, 0L);
	    break;

	case ID_COPY:			/* Clipboard copy		   */
	    SendMessage(hWndText,WM_COPY,0,0L);
	    break;

	case ID_PASTE:			/* Clipboard paste		   */
	    SendMessage(hWndText, WM_PASTE, 0, 0L);
	    break;

	case ID_CUT:			/* Clipboard cut		   */
	    SendMessage(hWndText, WM_CUT, 0, 0L);
	    break;

	case ID_CLEAR:			/* Clipboard clear		   */
	    SendMessage(hWndText, WM_CLEAR, 0, 0L);
	    break;

	case ID_GOEDIT:			/* Open text editor		   */
	    runEditor();
	    break;

	case ID_EDITSELECTED:		/* Open editor for selected text   */
	    setLastEdit((String)GetSelectedText(hWndText), 0);
	    runEditor();
	    break;

	case ID_FIND: {			/* Find defn of selected text	   */
	    String nm = GetSelectedText(hWndText);
	    input(BREAK);
	    WinPuts(hWndText, "\n");
	    sendString(hWndText, ":f %s\n", nm);
	    longjmp(catch_error, 1);
	    break;
	}

	case ID_TYPE: {			/* Show type of selected text	   */
	    String nm = GetSelectedText(hWndText);
	    input(BREAK);
	    WinPuts(hWndText, "\n");
	    sendString(hWndText, ":t %s\n", nm);
	    longjmp(catch_error, 1);
	    break;
	}

	case ID_INFO: {			/* Show info on selected text	   */
	    String nm = GetSelectedText(hWndText);
	    input(BREAK);
	    WinPuts(hWndText, "\n");
	    sendString(hWndText, ":i %s\n");
	    longjmp(catch_error, 1);
	    break;
	}

	case ID_EVAL: {			/* Eval selected text		  */
	    String nm = GetSelectedText(hWndText);
	    input(BREAK);
	    WinPuts(hWndText, "\n");
	    sendString(hWndText, "%s\n", nm);
	    longjmp(catch_error, 1);
	    break;
	}

	case ID_STOP:			/* Stop program execution	   */
	    sigRaise(breakHandler);
	    break;

	case ID_RUN:			/* Evaluate main expression	   */
	    input(BREAK);
	    WinPuts(hWndText, "\n");
	    readScripts(1);
	    sendString(hWndText, "main\n");
	    longjmp(catch_error, 1);
	    break;

	case ID_SETOPTIONS:		/* Set interpreter options	   */
	    DoOptions();
	    SaveProfileOptions();
	    break;

	case ID_RELOAD:			/* Reload script files loaded	   */
	    input(BREAK);
	    WinPuts(hWndText, "\n");
	    readScripts(1);
	    longjmp(catch_error, 1);
	    break;

	case ID_BROWSENAMES:		/* Browse names			   */
	    DoBrowseNames(hWnd, msg, wParam, lParam);
	    break;

	case ID_BROWSETYCONS:		/* Browse type constructors	   */
	    DoBrowseTycons(hWnd, msg, wParam, lParam);
	    break;

	case ID_BROWSECLASSES:		/* Browse classes		   */
	    DoBrowseClasses(hWnd, msg, wParam, lParam);
	    break;

	case ID_BROWSEHIERARCHY:	/* Show class hierarchy		   */
	    Hierarchy();
	    break;

	case ID_SCRIPTMAN:		/* Enter Script Manager		   */
	    ScriptManager(hWnd, msg, wParam, lParam);
	    break;

       case ID_HELPUSE:			/* help about using windows help   */
	    WinHelp(hWnd, "Winhelp.Hlp", HELP_CONTENTS, 0L);
	    break;

	case ID_HELPINDEX:		/* Show help about Hugs		   */
	    WinHelp(hWnd, "Hugs.Hlp", HELP_CONTENTS, 0L);
	    break;

	case ID_ABOUT:			/* Show Hugs copyright		   */
	    DoAbout();
	    break;
    }
}

/* Respond to Windows WM_CREATE message */
#pragma argsused
static INT local DoCreate(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  BITMAP     bm;
  HBITMAP    hBitmap;
  INT        i;

  /* Create a font to use in text window */
  theFont = CreateFont(15, 7, 0, 0,
			 FW_NORMAL,
			 FALSE,
			 FALSE,
			 FALSE,
			 ANSI_CHARSET,
			 OUT_TT_PRECIS,
			 CLIP_TT_ALWAYS,
			 DEFAULT_QUALITY,
			 FIXED_PITCH | FF_MODERN,
			 "Courier New");

  if (!theFont) {
    MessageBox(GetFocus(), "Out of memory creating text font", appName,
	       MB_ICONHAND | MB_SYSTEMMODAL | MB_OK);
    DoDestroy(hWnd, msg, wParam, lParam);
    return -1;
  }

  /* Create a font for error messages */
  theErrorFont = CreateFont( 15, 7, 0, 0,
			     FW_NORMAL,
			     TRUE,  /* Italic */
			     FALSE,
			     FALSE,
			     ANSI_CHARSET,
			     OUT_TT_PRECIS,
			     CLIP_TT_ALWAYS,
			     DEFAULT_QUALITY,
			     FIXED_PITCH | FF_MODERN,
			     "Courier New");

  if (!theErrorFont) {
    MessageBox(GetFocus(), "Out of memory creating error font", appName,
	       MB_ICONHAND | MB_SYSTEMMODAL | MB_OK);
    DoDestroy(hWnd, msg, wParam, lParam);
    return -1;
  }

  /* Load Windows cursor to use */
  NormalCursor  = LoadCursor(NULL, IDC_ARROW);
  HandCursor    = LoadCursor(hThisInstance, "HANDCURSOR");
  GarbageCursor = LoadCursor(hThisInstance, "GARBAGECURSOR");

  if (!(NormalCursor && HandCursor && GarbageCursor)){
    MessageBox(GetFocus(), "Out of memory loading cursors", appName,
	       MB_ICONHAND | MB_SYSTEMMODAL | MB_OK);
    DoDestroy(hWnd, msg, wParam, lParam);
    return -1;
  }

  /* Get buttons size */
  hBitmap = LoadBitmap(hThisInstance, "BUTTON");
  GetObject(hBitmap, sizeof(BITMAP), &bm);
  Button_Width  = bm.bmWidth+2;
  Button_Height = bm.bmHeight+2;
  DeleteObject(hBitmap);

  /* Get Handles to all submenus (needed for status line messages) */
  NMenus = GetMenuItemCount(GetMenu(hWnd));
  for (i=0; i<NMenus; ahwndSubMenus[i]=GetSubMenu(GetMenu(hWnd),i), i++);

  /* Get number of buttons in tool bar */
  for(nButtons=0; theButtons[nButtons].Msg; nButtons++);

  /* Use 3-D look in dialog boxes */
  Ctl3dRegister(hThisInstance);
  Ctl3dAutoSubclass(hThisInstance);

  return 0;
}

/* Respond to Windows WM_DESTROY message */
#pragma argsused
static VOID local DoDestroy(HWND hWnd,UINT msg,WPARAM wParam,LPARAM lParam) {
    /* Close Help Windows, if opened */
    WinHelp(hWnd, "WINHELP.HLP", HELP_QUIT, 0L);
    WinHelp(hWnd, "HUGS.HLP",   HELP_QUIT, 0L);

    /* Free space used for Fonts and cursors */
    if (theFont)       DeleteObject(theFont);
    if (theErrorFont)  DeleteObject(theErrorFont);
    if (NormalCursor)  DestroyCursor(NormalCursor);
    if (HandCursor)    DestroyCursor(HandCursor);
    if (GarbageCursor) DestroyCursor(GarbageCursor);

    Ctl3dUnregister(hThisInstance);	/* No more 3-D look */
}

/* Respond to Windows WM_GETMINMAXINFO message */
#pragma argsused
static VOID local DoGetMinMaxInfo(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    MINMAXINFO FAR* lpmmi;
    RECT	    rWind;

    GetWindowRect(hWnd, &rWind);

    lpmmi 		    = (MINMAXINFO FAR*) lParam;
    lpmmi->ptMaxPosition.x  = rWind.left;
    lpmmi->ptMaxPosition.y  = rWind.top;
    lpmmi->ptMaxTrackSize.x = WindowWidth;
    lpmmi->ptMaxTrackSize.y = WindowHeight;
    lpmmi->ptMaxSize.x      = WindowWidth;
    lpmmi->ptMaxSize.y      = WindowHeight;
}

/* Respond to Windows WM_LBUTTONDOWN message */
#pragma argsused
static VOID local DoLButtonDown(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
   INT     xPos = LOWORD(lParam);
   INT     yPos = HIWORD(lParam);
   INT     nButton;

   /* Check if a button on the Tool bar is pushed */
   if (xPos < RectTools.right &&
       yPos && yPos < RectTools.top + 1 + nButtons*(Button_Height)){
     nButton = (yPos) / (Button_Height);
     if (theButtons[nButton].IsEnabled) {
      buttonPushed = nButton;
      SetCapture(hWnd);
      pushButton(hWnd);
     }
   }
}

/* Respond to Windows WM_LBUTTONUP message */
#pragma argsused
static VOID local DoLButtonUp(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    if (buttonPushed != -1) {
      if (buttonDown) {
	SetCursor(NormalCursor);
	PostMessage(hWnd, WM_COMMAND, theButtons[buttonPushed].Msg, 0L);
	PutHelpLine(hWnd, "");
	unpushButton(hWnd);
      }
      buttonPushed = (-1);
      ReleaseCapture();
    }
}

/* Used to show a message on the help line depending on the menu option selected */
#pragma argsused
static VOID local DoMenuSelect(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  WORD idMenu;
  CHAR szMsg[80];
#if DOS
  UINT Item=wParam, Flags=lParam;
#else
  UINT Item=(UINT)LOWORD(wParam), Flags=(UINT)HIWORD(wParam);
#endif

  strcpy(szMsg, "");

  if (Flags & MF_POPUP){ /* It is a submenu (like Files) */
#if DOS
    for (idMenu=0; idMenu < 16 && Item!=(UINT)ahwndSubMenus[idMenu]; idMenu++);
#else
    for (idMenu=0; idMenu < 16 && GetSubMenu((HMENU)lParam,Item)!=(HMENU)ahwndSubMenus[idMenu]; idMenu++);
#endif

   if (!ahwndSubMenus[idMenu] || idMenu == 16) {
    PutHelpLine(hWnd, "");
    goto end;
   }
   idMenu++; /* offsets begin with 1 */
  }
  else
   idMenu = Item;  /* Item is the curent menu item identifier */

  if ((Flags == 0xffff) ||
     ((LoadString(hThisInstance, idMenu, (LPSTR) szMsg, sizeof(szMsg))) != NULL )){

   if (Flags == 0xffff)
    PutHelpLine(hWnd, ""); /* The menu has been closed */
   else
    PutHelpLine(hWnd, szMsg);
  }
  end:
}

/* Respond to Windows WM_MOUSEMOVE message */
#pragma argsused
static VOID local DoMouseMove(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
   INT  xPos = LOWORD(lParam);
   INT  yPos = HIWORD(lParam);
   INT  nButton;
   CHAR szMsg[256];

   static INT lastHelp = -1;

   /* Show help if cursor on a button of the tool bar */
   if (xPos < RectTools.left+Button_Width+4 && yPos && yPos < RectTools.top + 1 + nButtons*(Button_Height)){
     SetCursor(HandCursor);
     nButton = yPos / (Button_Height);
     if (buttonPushed!=(-1))
       if (buttonPushed==nButton)
	 pushButton(hWnd);
       else
	 unpushButton(hWnd);
     if (lastHelp != nButton) {
      lastHelp = nButton;
      LoadString(hThisInstance,
		 theButtons[nButton].IdHelpLine,
		 (LPSTR) szMsg,
		 sizeof(szMsg));
      PutHelpLine(hWnd, (LPSTR)szMsg);
     }
   }
   else {
     unpushButton(hWnd);
     SetCursor(NormalCursor);
     if (lastHelp != -1) {
      lastHelp = -1;
      PutHelpLine(hWnd, "");
     }
   }
}

/* Respond to Windows WM_PAINT message */
#pragma argsused
static VOID local DoPaint(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
  PAINTSTRUCT ps;
  RECT RectIntersect;

  BeginPaint(hWnd, &ps);
  /* Paint Tool bar if needed */
  if (IntersectRect(&RectIntersect, &RectTools, &ps.rcPaint))
   DrawToolBar(hWnd);
  /* Paint Status line if needed */
  if (IntersectRect(&RectIntersect, &RectStatus, &ps.rcPaint))
   DrawStatusLine(hWnd);
  EndPaint(hWnd, &ps);
}

/* Respond to Windows WM_SIZE message */
#pragma argsused
static VOID local DoSize(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
  RECT rClient;

  GetClientRect(hWnd, (LPRECT) &rClient);

  /* Set lower status line position */
  RectStatus.top    = rClient.bottom - STATUS_HEIGHT;
  RectStatus.bottom = rClient.bottom;
  RectStatus.left   = rClient.left;
  RectStatus.right  = rClient.right;

  /* Set Tool Bar position */
  RectTools.top = rClient.top;
  RectTools.bottom = rClient.bottom - STATUS_HEIGHT;
  RectTools.left   = rClient.left;
  RectTools.right  = rClient.left+TOOLS_WIDTH-1;

  /* Resize status line */
  if (bmpStatusBar)
    DeleteObject(bmpStatusBar);

  bmpStatusBar = CreateStatusBar(hWnd);

  #define H_INDENT     0    /* Separation between tool bar and text window */
  #define V_INDENT     0    /* Separation between main menu and text window */

  /* Resize child text window size */
  MoveWindow(hWndText, TOOLS_WIDTH-1+H_INDENT, V_INDENT,
    rClient.right-RectTools.right-H_INDENT*2, RectStatus.top-V_INDENT*2, TRUE);
}

#pragma argsused
static VOID local DoInitMenu (HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam) {
  BOOL CanCopy  = (BOOL) SendMessage (hWndText, WM_CANCOPY,  0, 0L);
  BOOL CanCut   = (BOOL) SendMessage (hWndText, WM_CANCUT,   0, 0L);
  BOOL CanPaste = (BOOL) SendMessage (hWndText, WM_CANPASTE, 0, 0L);
  BOOL CanClear = (BOOL) SendMessage (hWndText, WM_CANCLEAR, 0, 0L);
  EnableMenuItem((HMENU)wParam, ID_COPY,  (CanCopy  ? MF_ENABLED : MF_GRAYED)|MF_BYCOMMAND);
  EnableMenuItem((HMENU)wParam, ID_PASTE, (CanPaste ? MF_ENABLED : MF_GRAYED)|MF_BYCOMMAND);
  EnableMenuItem((HMENU)wParam, ID_CUT,   (CanCut   ? MF_ENABLED : MF_GRAYED)|MF_BYCOMMAND);
  EnableMenuItem((HMENU)wParam, ID_CLEAR, (CanClear ? MF_ENABLED : MF_GRAYED)|MF_BYCOMMAND);
  EnableMenuItem((HMENU)wParam, ID_EVAL,  (CanCopy  ? MF_ENABLED : MF_GRAYED)|MF_BYCOMMAND);
  EnableMenuItem((HMENU)wParam, ID_TYPE,  (CanCopy  ? MF_ENABLED : MF_GRAYED)|MF_BYCOMMAND);
  EnableMenuItem((HMENU)wParam, ID_INFO,  (CanCopy  ? MF_ENABLED : MF_GRAYED)|MF_BYCOMMAND);
  EnableMenuItem((HMENU)wParam, ID_FIND,  (CanCopy  ? MF_ENABLED : MF_GRAYED)|MF_BYCOMMAND);
  EnableMenuItem((HMENU)wParam, ID_OPENSELECTED,  (CanCopy  ? MF_ENABLED : MF_GRAYED)|MF_BYCOMMAND);
  EnableMenuItem((HMENU)wParam, ID_EDITSELECTED,  (CanCopy  ? MF_ENABLED : MF_GRAYED)|MF_BYCOMMAND);
}

/* --------------------------------------------------------------------------
 * Other functions for Windows GUI:
 * ------------------------------------------------------------------------*/

static HBITMAP local CreatehDC (HWND hWnd, HDC *hBmpDC, LPCSTR bitmapName, COLORREF Color) {
  INT       i, j;
  HDC       hDC;
  HBITMAP   hbmp, hOldBitmap;
  BITMAP    bm;

  /* Create Doc and NoDoc Bitmaps */
  hDC = GetDC (hWnd);

  hbmp = LoadBitmap(hThisInstance, bitmapName);
  GetObject(hbmp, sizeof(BITMAP), &bm);

  *hBmpDC = CreateCompatibleDC(hDC);
  hOldBitmap = SelectObject(*hBmpDC, hbmp);

  /* Set Background color for bitmap */
  for(i=0; i<bm.bmWidth; i++)
    for(j=0; j<bm.bmHeight; j++) {
      if (GetPixel(*hBmpDC, i, j) == RGB(0,128,128)) {
	 SetPixel(*hBmpDC, i, j, Color);
      }
    }

  ReleaseDC (hWnd, hDC);

  return hOldBitmap;
}

/* Draw a bitmap on a Window */
static Void local PutBitmap(HBITMAP hBitmap, HWND hWnd, INT left, INT top, DWORD mode)
{
   HBITMAP hOldBitmap;
   BITMAP  bm;
   HDC     hDC, hDCMemory;

   hDC = GetDC(hWnd);

   GetObject(hBitmap, sizeof(BITMAP), &bm);
   hDCMemory = CreateCompatibleDC(hDC);
   hOldBitmap = SelectObject(hDCMemory, hBitmap);
   BitBlt(hDC, left, top, bm.bmWidth, bm.bmHeight, hDCMemory, 0, 0, mode);

   SelectObject(hDCMemory, hOldBitmap);
   DeleteDC(hDCMemory);

   ReleaseDC(hWnd, hDC);
}

/* Used to get a file name using a common dialog box. Returns a pointer to    */
/* a static string where the file name is, or NULL if cancel button is pushed */
/* Mask is the identificator of a string defined in Hugs.Rc where the type   */
/* of files and their masks are                                               */
static CHAR* local GetaFileName(HWND hWnd, UINT Mask) {
  static OPENFILENAME ofn;
  static CHAR szFileName[256];
  CHAR  szFile[256], szFileTitle[256];
  UINT  i, cbString;
  CHAR  chReplace;    /* Separator between diferent filters in szFilter */
  CHAR  szFilter[256];

  szFile[0] = '\0';

  if ((cbString = LoadString(hThisInstance, Mask,
       szFilter, sizeof(szFilter))) == 0) {
    /* Error */
    return NULL;
  }

  chReplace = szFilter[cbString - 1]; /* Get separator */

  /* Replace separator with NULL */
  for (i = 0; szFilter[i] != '\0'; i++) {
    if (szFilter[i] == chReplace)
       szFilter[i] = '\0';
  }

  memset(&ofn, 0, sizeof(OPENFILENAME));

  ofn.lStructSize = sizeof(OPENFILENAME);
  ofn.hInstance = hThisInstance;
  ofn.hwndOwner = hWnd;
  ofn.lpstrFilter = szFilter;
  ofn.nFilterIndex = 1;
  ofn.lpstrFile= szFile;
  ofn.nMaxFile = sizeof(szFile);
  ofn.lpstrFileTitle = szFileTitle;
  ofn.nMaxFileTitle = sizeof(szFileTitle);
  ofn.lpTemplateName = MAKEINTRESOURCE(OPENFILEDLGBOX);
  ofn.Flags = OFN_HIDEREADONLY
/*          | OFN_PATHMUSTEXIST
	    | OFN_FILEMUSTEXIST */
	    | OFN_ENABLETEMPLATE;

  if (GetOpenFileName(&ofn)) {
    strcpy(szFileName, ofn.lpstrFile);
    strlwr(szFileName);
    return szFileName;
  }
  else
    return NULL;
}

static String* hugs_argv;
static Int     hugs_argc;

static Void local copyArgs Args((LPSTR));

static Void local copyArgs(LPSTR lpszCmdLine) {

  /* Count words */
  String  s = lpszCmdLine;
  Int words = 0;
  for (; *s==' '; ++s) {
  }
  while (*s) {
    /* Invariant: at start of word */
    words++;
    for (; *s && *s!=' '; ++s) {  /* skip to end of word */
    }
    for (; *s==' '; ++s) { /* skip to start of next word */
    }
  }

  /* Allocate hugs_argv */
  hugs_argc = 0;
#if DOS || IS_WIN32 /* DOS doesn't pass progname as arg */
  hugs_argv = malloc((words+1)*sizeof(CHAR *));
  hugs_argv[hugs_argc++] = appName;
#else
  hugs_argv = malloc(words*sizeof(CHAR *));
#endif

  /* Copy words into hugs_argv */
  s = lpszCmdLine;
  for (; *s==' '; ++s) {
  }
  while (*s) {
    /* Invariant: at start of word */
    String start = s;
    for (; *s && *s!=' '; ++s) {  /* skip to end of word */
    }

    /* Allocate space and copy over */
    { 
      String copy = (char *)malloc(s-start);
      if (copy == 0) {
	ERRMSG(0) "String storage space exhausted"
	EEND;
      }
      hugs_argv[hugs_argc++] = copy;
      for(; start != s;) {
	*copy++ = *start++;
      }
      *copy = '\0';
    }

    for (; *s==' '; ++s) { /* skip to start of next word */
    }
  }
}

/* Program entry point */
INT PASCAL WinMain(HANDLE hInstance, HANDLE hPrevInstance,
		   LPSTR lpszCmdLine, INT nCmdShow) {
  hThisInstance = hInstance;
  if (!hPrevInstance) {
    if (!InitApplication()) {
      MessageBeep(0);
#if !DOS
      {  
	DWORD Error = GetLastError();
	sprintf (buffer, "Error number %u",Error);
	MessageBox (NULL, buffer, "Error", MB_OK);
      }
#endif
      return FALSE;
    }
  }
  LoadProfileOptions(); /* Set Hugs interpreter options */
  if (!InitInstance(lpszCmdLine, nCmdShow))
    return FALSE;
  WinKbhit(hWndText);   /* Allow other program to run */
  copyArgs(lpszCmdLine);
  main(hugs_argc, hugs_argv);
  return 0;
}

/* Init application */
static BOOL local InitApplication(VOID) {
    WNDCLASS wc;
    wc.style	      = CS_VREDRAW | CS_HREDRAW;
    wc.lpfnWndProc    = MainWndProc;
    wc.cbClsExtra     = 0;
    wc.cbWndExtra     = 0;
    wc.hInstance      = hThisInstance;
    wc.hIcon	      = LoadIcon(hThisInstance, "Hugs");
    wc.hCursor	      = NULL; /* It must be NULL to use different ones */
    wc.hbrBackground  = GetStockObject(WHITE_BRUSH);
    wc.lpszMenuName   = "HugsMenu";
    wc.lpszClassName  = "HugsMainWindow";

    if (!RegisterClass(&wc))
	return FALSE;

    wc.style	      = CS_VREDRAW | CS_HREDRAW;
    wc.lpfnWndProc    = ClassesWndProc;
    wc.cbClsExtra     = 0;
    wc.cbWndExtra     = 0;
    wc.hInstance      = hThisInstance;
    wc.hIcon	      = NULL;
    wc.hCursor	      = NULL;
    wc.hbrBackground  = GetStockObject(LTGRAY_BRUSH);
    wc.lpszMenuName   = NULL;
    wc.lpszClassName  = "HugsClassesWindow";

    if (!RegisterClass(&wc))
	return FALSE;

    return RegisterTextClass(hThisInstance);
}

/* Init one instance of the application */
#pragma argsused
static BOOL local InitInstance(LPSTR lpCmdLine, INT nCmdShow) {
  RECT           rWind, rText;

  /* Create main window */
  hWndMain = CreateWindow("HugsMainWindow",
			  appName,
			  WS_OVERLAPPED  | WS_CAPTION | WS_SYSMENU |
			  WS_MINIMIZEBOX | WS_MAXIMIZEBOX | WS_THICKFRAME,
			  CW_USEDEFAULT,
			  CW_USEDEFAULT,
			  CW_USEDEFAULT,
			  CW_USEDEFAULT,
			  (HWND) NULL,
			  (HMENU) NULL,
			  hThisInstance,
			  (LPSTR) NULL);

  if (!hWndMain)
   return FALSE;

  /* Load accelerators */
  hAccelTable = LoadAccelerators(hThisInstance, (LPSTR) "HugsAccelerators");

  hWndText = CreateTextWindow(hThisInstance,
			      hWndMain,
			      TOOLS_WIDTH+H_INDENT,
			      V_INDENT,
			      ScreenCols, ScreenRows,
			      theFont,
			      hAccelTable);
  if (!hWndText)
    return FALSE;

  /* Set main window size */
  GetWindowRect (hWndText, &rText);

  WindowWidth =  (rText.right-rText.left+1)+
		 GetSystemMetrics(SM_CXFRAME)*2+
		 GetSystemMetrics(SM_CXVSCROLL)+
		 TOOLS_WIDTH;

  WindowHeight = (rText.bottom-rText.top+1)+
		 GetSystemMetrics(SM_CYFRAME)*2+
		 GetSystemMetrics(SM_CYCAPTION)+
		 GetSystemMetrics(SM_CYMENU)+
		 STATUS_HEIGHT;

  GetWindowRect(hWndMain, &rWind);

  MoveWindow(hWndMain, rWind.top, rWind.left, WindowWidth, WindowHeight, FALSE);

  /* Show the Window */
  ShowWindow(hWndMain, nCmdShow);
  UpdateWindow(hWndMain);

  return TRUE;
}

/* --------------------------------------------------------------------------
 * Main Window WinProc:
 * ------------------------------------------------------------------------*/

LRESULT FAR APIENTRY _export MainWndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
    switch (msg) {
#if DOS
	case WM_SYSCOLORCHANGE:
#else
	case WM_CTLCOLORBTN:			/* 3-D look		   */
	case WM_CTLCOLORDLG:
	case WM_CTLCOLOREDIT:
	case WM_CTLCOLORLISTBOX:
	case WM_CTLCOLORMSGBOX:
	case WM_CTLCOLORSCROLLBAR:
	case WM_CTLCOLORSTATIC:
#endif
	    Ctl3dColorChange();
	    break;

	case WM_LBUTTONDOWN:
	    DoLButtonDown(hWnd, msg, wParam, lParam);
	    break;

	case WM_LBUTTONUP:
	    DoLButtonUp(hWnd, msg, wParam, lParam);
	    break;

	case WM_MOUSEMOVE:
	    DoMouseMove(hWnd, msg, wParam, lParam);
	    break;

	case WM_SIZE:
	    DoSize(hWnd, msg, wParam, lParam);
	    break;

	case WM_GETMINMAXINFO:
	    DoGetMinMaxInfo(hWnd, msg, wParam, lParam);
	    break;

	case WM_CREATE:
	    return ((LONG) DoCreate(hWnd, msg, wParam, lParam));

	case WM_SETFOCUS:
	    SetFocus(hWndText);
	    break;

	case WM_PAINT:
	    DoPaint(hWnd, msg, wParam, lParam);
	    break;

	case WM_COMMAND:
	    DoCommand(hWnd, msg, wParam, lParam);
	    break;

	case WM_MENUSELECT:
	    DoMenuSelect(hWnd, msg, wParam, lParam);
	    break;

	case WM_DESTROY:
	    DoDestroy(hWnd, msg, wParam, lParam);
	    break;

	case WM_CLOSE:
	    everybody(EXIT);
	    DestroyWindow(hWnd);
	    exit(0);
	    break;

	case WM_INITMENU:
	    DoInitMenu(hWnd, msg, wParam, lParam);
	    break;

	default:
	    return DefWindowProc(hWnd, msg, wParam, lParam);
    }
    return (LONG) FALSE;
}

#if DOS
#define DlgSendMessage(h,c,w,l) SendMessage((h),(c),(w),(l))
#else
#define DlgSendMessage(h,c,w,l) SendMessage((h),(c),MAKEWPARAM(w,(HIWORD(l))),(LOWORD(l)))
#endif

/* --------------------------------------------------------------------------
 * About box:
 * ------------------------------------------------------------------------*/

static VOID local DoAbout() {
    DLGPROC lpDlg = (DLGPROC)MakeProcInstance((FARPROC)AboutDlgProc,
					      hThisInstance);
    DialogBox(hThisInstance,MAKEINTRESOURCE(ABOUTDLGBOX),GetFocus(),lpDlg);
#if DOS
    FreeProcInstance(lpDlg);
#endif
}

BOOL FAR APIENTRY _export AboutDlgProc(HWND hDlg, UINT msg,
				       WPARAM wP, LPARAM lP) {
    switch (msg) {
	case WM_INITDIALOG:
	    return TRUE;

	case WM_COMMAND:
	    if (CMDitem(wP,lP) == IDOK)
		EndDialog(hDlg,TRUE);
	    return TRUE;
    }
    return FALSE;
}

/* --------------------------------------------------------------------------
 * Script Manager:
 * ------------------------------------------------------------------------*/

static INT smLoaded, smUpto;
static String smFile[NUM_SCRIPTS];
static INT selScr;

static Void local SmSelScr(HWND hDlg, Int i) {
    selScr = i;
    SendDlgItemMessage(hDlg, LB_SCRIPTS, LB_SETCURSEL, i, 0L);
}

static Void local SmAddScr(HWND hDlg, CHAR *s) {
    smFile[smUpto] = strCopy(s);
    fprintf(stdstr,"%s\n",smFile[smUpto]);
    SendDlgItemMessage(hDlg, LB_SCRIPTS, LB_ADDSTRING, 0,
		       (LONG) (LPSTR) stdstrbuff);
    SmSelScr(hDlg,smUpto);
    smUpto++;
}

#pragma argsused
BOOL CALLBACK ScriptManDlgProc(HWND hDlg, UINT msg,
			       WPARAM wParam, LPARAM lParam) {
    switch (msg) {
	case WM_INITDIALOG: {
	    Int i;
	    smLoaded = numScripts;
	    smUpto   = 0;
	    SendDlgItemMessage(hDlg,LB_SCRIPTS,WM_SETREDRAW,FALSE,0L);
	    for (i=0; i<namesUpto; i++)
		SmAddScr(hDlg,scriptName[i]);
	    SmSelScr(hDlg,-1);
	    SendDlgItemMessage(hDlg,LB_SCRIPTS,WM_SETREDRAW,TRUE,0L);
	    return TRUE;
	}

	case WM_COMMAND:
	    switch (CMDitem(wParam,lParam)) {
		case ID_ADDSCRIPT:
		    if (smUpto>=NUM_SCRIPTS)
			MessageBox(hDlg,"Too many script files",
				   "Add script", MB_ICONEXCLAMATION|MB_OK);
		    else {
			String s = GetaFileName(hDlg,IDS_FILTERFILE);
			if (s)
			    SmAddScr(hDlg,s);
		    }
		    return TRUE;

		case ID_DELSCRIPT:
		    if (selScr < 0)
			MessageBox(hDlg,"No script file selected",
				   "Remove script", MB_ICONEXCLAMATION|MB_OK);
		    else if (selScr == 0)
			MessageBox(hDlg,"Cannot remove prelude file",
				   "Remove script", MB_ICONEXCLAMATION|MB_OK);
		    else {
			Int i;
			SendDlgItemMessage(hDlg, LB_SCRIPTS, LB_DELETESTRING,
					   selScr, 0L);
			if (selScr<smLoaded)
			    smLoaded = selScr;
			if (smFile[selScr]) {
			    free(smFile[selScr]);
			    smFile[selScr] = 0;
			}
			for (i=selScr+1; i<smUpto; ++i)
			    smFile[i-1] = smFile[i];
			smUpto--;
			SmSelScr(hDlg,-1);
		    }
		    return TRUE;

		case ID_EDITSCRIPT:
		    if (selScr >= 0)
			SendMessage(hDlg, WM_COMMAND, LB_SCRIPTS,
				    MAKELONG(0, LBN_DBLCLK));
		    else
			MessageBox(hDlg,"No file selected","Edit",
				   MB_ICONEXCLAMATION|MB_OK);
		    return TRUE;

		case ID_CLEARSCRIPTS: {
		    Int i;
		    for (i=1; i<smUpto; ++i)
			if (smFile[i])
			    free(smFile[i]);
		    smUpto = smLoaded = 1;
		    SendDlgItemMessage(hDlg,LB_SCRIPTS,LB_RESETCONTENT,0,0L);
		    fprintf(stdstr,"%s\n",smFile[0]);
		    SendDlgItemMessage(hDlg, LB_SCRIPTS, LB_ADDSTRING, 0,
				       (LONG) (LPSTR) stdstrbuff);
		    SmSelScr(hDlg,-1);
		    return TRUE;
		}

		case LB_SCRIPTS:
		    switch (CMDdata(wParam,lParam)) {
			case LBN_SELCHANGE:
			    SmSelScr(hDlg,(Int)SendDlgItemMessage(hDlg,
								  LB_SCRIPTS,
								  LB_GETCURSEL,
								  0, 0L));
			    return TRUE;

			case LBN_DBLCLK:
			    SendDlgItemMessage(hDlg,
					       LB_SCRIPTS,
					       LB_GETTEXT,
					       selScr,
					       (LPARAM) (LPSTR) buffer);
			    setLastEdit(buffer,0);
			    runEditor();
			    return TRUE;
		    }
		    break;

		case IDOK: {
		    Int i;
		    for (i=0; i<namesUpto; i++)
			if (scriptName[i])
			    free(scriptName[i]);
		    for (i=0; i<smUpto; i++) {
			scriptName[i] = smFile[i];
			smFile[i]     = 0;
		    }
		    namesUpto  = smUpto;
		    numScripts = smLoaded;
		    dropScriptsFrom(numScripts-1);
		    PostMessage(hWndMain,WM_COMMAND,ID_RELOAD,0L);
		    EndDialog(hDlg,TRUE);
		    return TRUE;
		}

		case IDCANCEL: {
		    Int i;
		    for (i=0; i<smUpto; i++)
			if (smFile[i])
			    free(smFile[i]);
		    EndDialog(hDlg, TRUE);
		    return TRUE;
		}
	    }
	    break;
    }
    return FALSE;
}

/* --------------------------------------------------------------------------
 * Options:
 * ------------------------------------------------------------------------*/

static VOID local DoOptions() {
    DLGPROC lpDlg = (DLGPROC)MakeProcInstance((FARPROC)OptionsDlgProc,
					      hThisInstance);
    DialogBox(hThisInstance,MAKEINTRESOURCE(OPTIONSDLGBOX),GetFocus(),lpDlg);
#if DOS
    FreeProcInstance(lpDlg);
#endif
}

#pragma argsused
BOOL FAR APIENTRY _export OptionsDlgProc(HWND hDlg, UINT msg,
					 WPARAM wP, LPARAM lP) {
    switch (msg) {
	case WM_INITDIALOG: {
	    Int i = 0;
	    for (; toggle[i].c; i++)
		CheckDlgButton(hDlg,ID_OP+i,*toggle[i].flag);
	    SetDlgItemText(hDlg,ID_PROMPT,  (LPCSTR)prompt);
	    SetDlgItemText(hDlg,ID_LASTEXPR,(LPCSTR)repeatStr);
	    SetDlgItemText(hDlg,ID_HUGSEDIT,(LPCSTR)hugsEdit);
	    SetDlgItemText(hDlg,ID_HUGSPATH,(LPCSTR)hugsPath);
	    return TRUE;
	}

	case WM_COMMAND :
	    switch (CMDitem(wP,lP)) {

		case IDCANCEL :
		    EndDialog(hDlg, TRUE);
		    return FALSE;

		case IDOK : {
		    Int i=0;
		    for (; toggle[i].c; i++)
			*toggle[i].flag
			    = (Bool)IsDlgButtonChecked(hDlg,ID_OP+i);

		    GetDlgItemText(hDlg,ID_PROMPT,(LPSTR)buffer,MAXBUF);
		    if (*buffer && buffer[strlen(buffer)-1]!=' ')
			strcat(buffer," ");
		    if (prompt) free(prompt);
		    prompt = strCopy(buffer);

		    GetDlgItemText(hDlg,ID_LASTEXPR,(LPSTR)buffer,MAXBUF);
		    if (repeatStr) free(repeatStr);
		    repeatStr = strCopy(buffer);

		    GetDlgItemText(hDlg,ID_HUGSPATH,(LPSTR)buffer,MAXBUF);
		    if (hugsPath) free(hugsPath);
		    hugsPath = strCopy(buffer);

		    GetDlgItemText(hDlg,ID_HUGSEDIT,(LPSTR)buffer,MAXBUF);
		    if (hugsEdit) free(hugsEdit);
		    hugsEdit = strCopy(buffer);

		    EndDialog(hDlg, TRUE);
		    return TRUE;
		}

		default	:
		    return TRUE;
	    }
    }
    return FALSE;
}

/* --------------------------------------------------------------------------
 * Loading and saving options to registry
 * ------------------------------------------------------------------------*/

#if !USE_REGISTRY
static String inifile="Hugs.ini";
#endif

static Void local LoadProfileOptions(VOID) {
#if USE_REGISTRY /* should always be set - but why risk it? */
    INT    NewRows, NewCols;
    NewRows = readRegInt("Screen_Rows", ROWS);
    if (ScreenRows && NewRows != ScreenRows) {
	MessageBox(GetFocus(),
		   "To change number of screen rows run Hugs again",
		   appName, MB_OK|MB_ICONINFORMATION);
    } else {
	ScreenRows = NewRows;
    }

    NewCols   = readRegInt("Screen_Columns", COLS);
    if (ScreenCols && NewCols != ScreenCols) {
	MessageBox(GetFocus(),
		   "To change number of screen columns run Hugs again",
		   appName, MB_OK|MB_ICONINFORMATION);
    } else {
	ScreenCols = NewCols;
    }
#else
    GetPrivateProfileString("Hugs","Options","",buffer,MAXBUF,inifile);
    readOptions(buffer);
    ScreenRows = GetPrivateProfileInt("Screen", "Rows",    ROWS, inifile);
    ScreenCols = GetPrivateProfileInt("Screen", "Columns", COLS, inifile);
#endif /* USE_REGISTRY */
}

static Void local SaveProfileOptions(VOID) {
#if USE_REGISTRY /* should always be set - but why risk it? */
    writeRegString("Options", optionsToStr());
    writeRegInt("Screen_Rows",    ScreenRows);
    writeRegInt("Screen_Columns", ScreenCols);
#else
    WritePrivateProfileString("Hugs", "Options", optionsToStr(), inifile);
    wsprintf(buffer, "%d", ScreenRows);
    WritePrivateProfileString("Screen",  "Rows",    buffer,     inifile);
    wsprintf(buffer, "%d", ScreenCols);
    WritePrivateProfileString("Screen",  "Columns", buffer,     inifile);
    WritePrivateProfileString(NULL,NULL,NULL,inifile); /* flush to disk */
#endif
}

/* --------------------------------------------------------------------------
 * Browser dialog boxes:
 * ------------------------------------------------------------------------*/

/* When a class changes to currClass get new list of instances, */
/* members and contexts for the new class                       */

static local VOID SetClass(HWND hDlg, Class currClass) {
    List instances = cclass(currClass).instances;
    List members   = cclass(currClass).members;
    List supers    = cclass(currClass).supers;

    /* Update list of instances */
    SendDlgItemMessage(hDlg,LB_INSTANCES,LB_RESETCONTENT,0,0L);
    for (; nonNull(instances); instances=tl(instances)) {
	SendDlgItemMessage(hDlg,LB_INSTANCES,LB_ADDSTRING,0,
			   (LONG)hd(instances));
	SendDlgItemMessage(hDlg,LB_INSTANCES,LB_SETCURSEL,0,0L);
    }

    /* Update list of members */
    SendDlgItemMessage(hDlg,LB_MEMBERS,LB_RESETCONTENT,0,0L);
    for (; nonNull(members); members=tl(members)) {
	SendDlgItemMessage(hDlg,LB_MEMBERS,LB_ADDSTRING,0,
			   (LONG)hd(members));
	SendDlgItemMessage(hDlg,LB_MEMBERS,LB_SETCURSEL,0,0L);
    }

    /* Update superclasses */
    SendDlgItemMessage(hDlg,LB_CONTEXT,LB_RESETCONTENT,0,0L);
    for (; nonNull(supers); supers=tl(supers)) {
	printPred(stdstr,hd(supers));
	if (nonNull(tl(supers)))
	    fprintf(stdstr,", ");
    }
    fprintf(stdstr,"\n");
    SendDlgItemMessage(hDlg,LB_CONTEXT,LB_ADDSTRING,0,(LONG)(LPSTR)stdstrbuff);
}

/* Handles browse classes dialog box */
#pragma argsused
BOOL FAR APIENTRY _export BrowseClassesDlgProc(HWND hDlg, UINT msg,
					       WPARAM wParam, LPARAM lParam) {
    static Class currClass;
    Class	 theClass;
    Inst	 theInst;
    Name	 theMember;
    Script       script;
    HBITMAP      hBitmap;
    RECT         aRect, DlgRect;
    HBITMAP      hBmp, hsvBmp1, hsvBmp2, hsvBmp3, hsvBmp4, hsvBmp5, hsvBmp6;
    BITMAP	 bm;
    static HDC   hDCMemory, hCDC, hCDCSel, hIDC, hIDCSel, hMDC, hMDCSel;

    switch (msg) {

	case WM_INITDIALOG: {/* Create list of classes and set current class */
	    Int i;
	    for (i=CLASSMIN; i<classMax(); i++)
		SendDlgItemMessage(hDlg, LB_CLASS ,LB_ADDSTRING,
						0, (LPARAM)(LPSTR) i);
	    SendDlgItemMessage(hDlg, LB_CLASS, LB_SETCURSEL, 0, 0L);
	    currClass = (Class)SendDlgItemMessage(hDlg,
					       LB_CLASS,
					       LB_GETITEMDATA,
					       (WPARAM)SendDlgItemMessage(hDlg, LB_CLASS ,LB_GETCURSEL, 0, 0L), 0L);
	    SetClass(hDlg, currClass);

	    /* Create Bitmaps */
	    hsvBmp1 = CreatehDC(hDlg, &hCDC,    "CLASSBMP",    GetSysColor(COLOR_WINDOW));
	    hsvBmp2 = CreatehDC(hDlg, &hCDCSel, "CLASSBMP",    GetSysColor(COLOR_HIGHLIGHT));
	    hsvBmp3 = CreatehDC(hDlg, &hIDC,    "INSTANCEBMP", GetSysColor(COLOR_WINDOW));
	    hsvBmp4 = CreatehDC(hDlg, &hIDCSel, "INSTANCEBMP", GetSysColor(COLOR_HIGHLIGHT));
	    hsvBmp5 = CreatehDC(hDlg, &hMDC,    "MEMBERBMP",   GetSysColor(COLOR_WINDOW));
	    hsvBmp6 = CreatehDC(hDlg, &hMDCSel, "MEMBERBMP",   GetSysColor(COLOR_HIGHLIGHT));
	    break;
	}

	case WM_DESTROY: /* Destroy Bitmaps */
	    SelectObject(hCDC, hsvBmp1);     DeleteDC(hCDC);
	    SelectObject(hCDCSel, hsvBmp2);  DeleteDC(hCDCSel);
	    SelectObject(hIDC, hsvBmp3);     DeleteDC(hIDC);
	    SelectObject(hIDCSel, hsvBmp4);  DeleteDC(hIDCSel);
	    SelectObject(hMDC, hsvBmp5);     DeleteDC(hMDC);
	    SelectObject(hMDCSel, hsvBmp6);  DeleteDC(hMDCSel);
	    break;

	case WM_PAINT: /* Paint classes Bitmap */
	    GetWindowRect(hDlg, &DlgRect);
	    GetWindowRect(GetDlgItem(hDlg,ID_BROWSEBMP),&aRect);

	    hBitmap = LoadBitmap(hThisInstance, "CLASSESDLGBMP");
	    PutBitmap(hBitmap,
		      hDlg,
		      aRect.left-DlgRect.left-GetSystemMetrics(SM_CXDLGFRAME),
		      aRect.top-DlgRect.top-GetSystemMetrics(SM_CYDLGFRAME)
				-GetSystemMetrics(SM_CYCAPTION),
		      SRCCOPY);
	    DeleteObject(hBitmap);
	    break;

	case WM_COMPAREITEM: {
	    LPCOMPAREITEMSTRUCT lpcis = (LPCOMPAREITEMSTRUCT) lParam;
	    switch (wParam) {
		case LB_CLASS     :
		case LB_INSTANCES :
		case LB_MEMBERS   : return (lpcis->itemData1==lpcis->itemData2);
	    }
	    break;
	}

	case WM_MEASUREITEM: {
	    DRAWITEMSTRUCT FAR *lpdis = (DRAWITEMSTRUCT FAR *) lParam;
	    if (lpdis->CtlID == LB_CLASS || lpdis->CtlID == LB_INSTANCES ||
		lpdis->CtlID == LB_MEMBERS ) {
		LPMEASUREITEMSTRUCT lpmis = (LPMEASUREITEMSTRUCT) lParam;

		/* Set the height of the list box items to Bitmap height */
		hBmp = LoadBitmap(hThisInstance, "CLASSBMP");
		GetObject(hBmp, sizeof(BITMAP), &bm);
		DeleteObject(hBmp);
		lpmis->itemHeight = bm.bmHeight+1;
		return TRUE;
	    }
	    break;
	}

	case WM_DRAWITEM: {
	    DRAWITEMSTRUCT FAR *lpdis = (DRAWITEMSTRUCT FAR *) lParam;
	    if (lpdis->CtlID == LB_CLASS || lpdis->CtlID == LB_INSTANCES ||
		lpdis->CtlID == LB_MEMBERS ) {
		Bool selected = FALSE;

		if (lpdis->itemID == (UINT)(-1))
		    return TRUE;

		switch (lpdis->itemAction) {
		    case ODA_DRAWENTIRE:
		    case ODA_SELECT:
		    case ODA_FOCUS:
			if ((lpdis->itemState & ODS_SELECTED)) {
			    SetBkColor(lpdis->hDC, GetSysColor(COLOR_HIGHLIGHT));
			    SetTextColor(lpdis->hDC, GetSysColor(COLOR_HIGHLIGHTTEXT));
			    selected = TRUE;
			}
			else {
			    SetBkColor(lpdis->hDC, GetSysColor(COLOR_WINDOW));
			    SetTextColor(lpdis->hDC, GetSysColor(COLOR_WINDOWTEXT));
			}
			break;

		    default:
			return FALSE;
		}

		/* Get bitmap size */
		hBmp = LoadBitmap(hThisInstance, "CLASSBMP");
		GetObject(hBmp, sizeof(BITMAP), &bm);
		DeleteObject(hBmp);

		switch (lpdis->CtlID) {
		    case LB_CLASS:
			theClass = (Class) SendDlgItemMessage(hDlg, lpdis->CtlID, LB_GETITEMDATA, lpdis->itemID, 0);
			printPred(stdstr,cclass(theClass).head);
			fprintf(stdstr,"\n");
			ExtTextOut(lpdis->hDC, lpdis->rcItem.left+21, lpdis->rcItem.top, ETO_OPAQUE, &(lpdis->rcItem), stdstrbuff, strlen(stdstrbuff), NULL);
			hDCMemory = selected ? hCDCSel : hCDC;
			break;

		    case LB_INSTANCES:
			theInst = (Inst) SendDlgItemMessage(hDlg, lpdis->CtlID, LB_GETITEMDATA, lpdis->itemID, 0);
			printPred(stdstr,inst(theInst).head);
			fprintf(stdstr,"\n");
			ExtTextOut(lpdis->hDC, lpdis->rcItem.left+21, lpdis->rcItem.top, ETO_OPAQUE, &(lpdis->rcItem), stdstrbuff, strlen(stdstrbuff), NULL);
			hDCMemory = selected ? hIDCSel : hIDC;
			break;

		    case LB_MEMBERS:
			theMember = (Name) SendDlgItemMessage(hDlg, lpdis->CtlID, LB_GETITEMDATA, lpdis->itemID, 0);
			printExp(stdstr, theMember);
			fprintf(stdstr, " :: ");
			printType(stdstr, name(theMember).type);
			fprintf(stdstr,"\n");
			ExtTextOut(lpdis->hDC, lpdis->rcItem.left+21, lpdis->rcItem.top, ETO_OPAQUE, &(lpdis->rcItem), stdstrbuff, strlen(stdstrbuff), NULL);
			hDCMemory = selected ? hMDCSel : hMDC;
			break;
		}

		BitBlt(lpdis->hDC, (lpdis->rcItem.left)+4, (lpdis->rcItem.top),
				   bm.bmWidth,             bm.bmHeight,
				   hDCMemory,
				   0,
				   0,
				   SRCCOPY);

		/* If selected draw rectangle */
		if ((lpdis->itemState & ODS_SELECTED)&&(lpdis->itemState & ODS_FOCUS))
		    DrawFocusRect(lpdis->hDC, &(lpdis->rcItem));

		return TRUE;
	    }
	    break;
	}

	case WM_COMMAND:
	    switch (CMDitem(wParam,lParam)) {
		case LB_CLASS:
		    switch(CMDdata(wParam,lParam)) {
			case LBN_SELCHANGE: /* select a new class */
			    currClass = (Class) SendDlgItemMessage(hDlg, LB_CLASS ,LB_GETITEMDATA, SendDlgItemMessage(hDlg, LB_CLASS ,LB_GETCURSEL, 0, 0L), 0L);
			    SetClass(hDlg, currClass);
			    break;

			case LBN_DBLCLK: /* Open in text editor script file with class definition */
			    currClass = (Class) SendDlgItemMessage(hDlg, LB_CLASS ,LB_GETITEMDATA, SendDlgItemMessage(hDlg, LB_CLASS ,LB_GETCURSEL, 0, 0L), 0L);
			    script = startNewScript(0);
			    setLastEdit(scriptName[scriptThisClass(currClass)], cclass(currClass).line);
			    dropScriptsFrom(script);
			    runEditor();
			    break;
		    }
		    break;

		/*case LB_MEMBERS:*/
		case LB_INSTANCES:
		    switch(CMDdata(wParam,lParam)) {/* Open in text editor script file with instance definition */
			case LBN_DBLCLK: {
			    Inst  currInst;
			    currInst = (Inst) SendDlgItemMessage(hDlg, LB_INSTANCES, LB_GETITEMDATA, SendDlgItemMessage(hDlg, LB_INSTANCES ,LB_GETCURSEL, 0, 0L), 0L);

			    /* Find instance script */
			    script = startNewScript(0);
			    setLastEdit(scriptName[scriptThisInst(currInst)], inst(currInst).line);
			    dropScriptsFrom(script);
			    runEditor();
			    break;
			}
		    }
		    break;

		case ID_HIERARCHY: /* Draw class hierarchy */
		    Hierarchy();
		    break;

		case ID_EDITCLASS: /* Pushed on Edit class button */
		    if (SendDlgItemMessage(hDlg, LB_CLASS, LB_GETCURSEL, 0, 0L) != LB_ERR)
			DlgSendMessage(hDlg, WM_COMMAND, LB_CLASS, MAKELONG(0, LBN_DBLCLK));
		    break;

		case ID_EDITINSTANCE: /* Pushed on Edit instance button */
		    if (SendDlgItemMessage(hDlg, LB_INSTANCES, LB_GETCURSEL, 0, 0L) != LB_ERR)
			DlgSendMessage(hDlg, WM_COMMAND, LB_INSTANCES, MAKELONG(0, LBN_DBLCLK));
		    break;

		case IDCANCEL: /* Close dialog */
		case IDOK:
		    EndDialog(hDlg, TRUE);
		    return TRUE;

		default:
		    return TRUE;
	    }
    }
    return FALSE;
}

static local String describeName( Name nm ) {
  if (isCfun(nm))
    fprintf(stdstr,"Data constructor, ");
  else if (isMfun(nm))
    fprintf(stdstr, "Class member, ");
  else if (isSfun(nm))
    fprintf(stdstr, "Selector function, ");
  else if (name(nm).primDef)
    fprintf(stdstr, "Primitive, ");
  printExp(stdstr,nm);
  fprintf(stdstr," :: ");
  if (nonNull(name(nm).type))
      printType(stdstr,name(nm).type);
  else
    fprintf(stdstr,"<Unknown type>");
  if (name(nm).line)
      fprintf(stdstr,"   (%s,%d)",scriptName[scriptThisName(nm)],name(nm).line);
  fprintf(stdstr, "\n");
  return stdstrbuff;
}


/* When the name  selected changes to currName get its type and definition */
static local VOID SetName(HWND hDlg, UINT currName, List names) {
  Name nm = nth(currName,names);
  SendDlgItemMessage(hDlg,LB_NAMESNOTES,LB_RESETCONTENT,0,0L);
  SendDlgItemMessage(hDlg,LB_NAMESNOTES,LB_ADDSTRING,0,
		     (LONG)(LPSTR)describeName(nm));
}

typedef struct {
  List names; /* ADR note: This is not marked during GC */
  HDC  hPDC, hPDCSel;
  HDC  hDDC, hDDCSel;
  HDC  hMDC, hMDCSel;
  HDC  hNDC, hNDCSel;
  HDC  hSDC, hSDCSel;
} NameBrowserState;

static NameBrowserState nBrowse;

static Void local doInitDialog_Names(HWND hDlg) {
  List names;
  SendDlgItemMessage(hDlg, LB_NAMES, LB_SETCOLUMNWIDTH, (WPARAM)(100), 0L);

  nBrowse.names = addNamesMatching((String)0, NIL);
  for (names=nBrowse.names; nonNull(names); names=tl(names)) {
    fprintf(stdstr, "%s\n",textToStr(name(hd(names)).text));
    SendDlgItemMessage(hDlg, LB_NAMES ,LB_ADDSTRING, 0, (LONG)(LPSTR) stdstrbuff);
    SendDlgItemMessage(hDlg, LB_NAMES, LB_SETCURSEL, 0, 0L);
  }
  SetName(hDlg, 0, nBrowse.names);

  CreatehDC(hDlg, &nBrowse.hPDC,    "PRIMBMP",     GetSysColor(COLOR_WINDOW));
  CreatehDC(hDlg, &nBrowse.hPDCSel, "PRIMBMP",     GetSysColor(COLOR_HIGHLIGHT));
  CreatehDC(hDlg, &nBrowse.hDDC,    "DATACONSBMP", GetSysColor(COLOR_WINDOW));
  CreatehDC(hDlg, &nBrowse.hDDCSel, "DATACONSBMP", GetSysColor(COLOR_HIGHLIGHT));
  CreatehDC(hDlg, &nBrowse.hMDC,    "MEMBERBMP",   GetSysColor(COLOR_WINDOW));
  CreatehDC(hDlg, &nBrowse.hMDCSel, "MEMBERBMP",   GetSysColor(COLOR_HIGHLIGHT));
  CreatehDC(hDlg, &nBrowse.hNDC,    "NAMEBMP",     GetSysColor(COLOR_WINDOW));
  CreatehDC(hDlg, &nBrowse.hNDCSel, "NAMEBMP",     GetSysColor(COLOR_HIGHLIGHT));
  CreatehDC(hDlg, &nBrowse.hSDC,    "SELECTORBMP", GetSysColor(COLOR_WINDOW));
  CreatehDC(hDlg, &nBrowse.hSDCSel, "SELECTORBMP", GetSysColor(COLOR_HIGHLIGHT));
}

static Void local doDestroy_Names() {
  /* ToDo: this is bogus - but no more so than the original code */
  SelectObject(nBrowse.hPDC,    NULL); DeleteDC(nBrowse.hPDC);
  SelectObject(nBrowse.hPDCSel, NULL); DeleteDC(nBrowse.hPDCSel);
  SelectObject(nBrowse.hDDC,    NULL); DeleteDC(nBrowse.hDDC);
  SelectObject(nBrowse.hDDCSel, NULL); DeleteDC(nBrowse.hDDCSel);
  SelectObject(nBrowse.hMDC,    NULL); DeleteDC(nBrowse.hMDC);
  SelectObject(nBrowse.hMDCSel, NULL); DeleteDC(nBrowse.hMDCSel);
  SelectObject(nBrowse.hNDC,    NULL); DeleteDC(nBrowse.hNDC);
  SelectObject(nBrowse.hNDCSel, NULL); DeleteDC(nBrowse.hNDCSel);
  SelectObject(nBrowse.hSDC,    NULL); DeleteDC(nBrowse.hSDC);
  SelectObject(nBrowse.hSDCSel, NULL); DeleteDC(nBrowse.hSDCSel);
}

static Void local doMeasureItem_Names(MEASUREITEMSTRUCT FAR *lpmis) {
  /* Set the height of the list box items to Bitmap height */
  HBITMAP hBmp = LoadBitmap(hThisInstance, "PRIMBMP");
  BITMAP  bm;
  GetObject(hBmp, sizeof(BITMAP), &bm);
  DeleteObject(hBmp);
  lpmis->itemHeight = bm.bmHeight+1;
}
  
static INT local doDrawItem_Names(HWND hDlg, DRAWITEMSTRUCT FAR *lpdis) {
  Bool    Selected = FALSE;
  HBITMAP hBmp;
  BITMAP  bm;
  Name    nm;
  HDC     hDCMemory;

  if (lpdis->itemID == (UINT)-1) {
      return TRUE;
  }
  switch (lpdis->itemAction) {
  case ODA_DRAWENTIRE:
  case ODA_SELECT:
  case ODA_FOCUS:
      if ((lpdis->itemState & ODS_SELECTED) /*&& (lpdis->itemState & ODS_FOCUS)*/) {
	SetBkColor(lpdis->hDC, GetSysColor(COLOR_HIGHLIGHT));
	SetTextColor(lpdis->hDC, GetSysColor(COLOR_HIGHLIGHTTEXT));
	Selected = TRUE;
      } else {
	SetBkColor(lpdis->hDC, GetSysColor(COLOR_WINDOW));
	SetTextColor(lpdis->hDC, GetSysColor(COLOR_WINDOWTEXT));
      }
      break;

  default:
      return FALSE;
  }

  SendDlgItemMessage(hDlg, lpdis->CtlID, LB_GETTEXT, lpdis->itemID, (LPARAM)buffer);
  ExtTextOut(lpdis->hDC, lpdis->rcItem.left+21, lpdis->rcItem.top, 
	     (ETO_CLIPPED|ETO_OPAQUE), &(lpdis->rcItem), 
	     buffer, strlen(buffer), NULL);

  /* Get bitmap size */
  hBmp = LoadBitmap(hThisInstance, "PRIMBMP");
  GetObject(hBmp, sizeof(BITMAP), &bm);
  DeleteObject(hBmp);

  nm = nth(lpdis->itemID, nBrowse.names);
  if (isCfun(nm))
    hDCMemory = Selected ? nBrowse.hDDCSel : nBrowse.hDDC;
  else if (isMfun(nm))
    hDCMemory = Selected ? nBrowse.hMDCSel : nBrowse.hMDC;
  else if (isSfun(nm))
    hDCMemory = Selected ? nBrowse.hSDCSel : nBrowse.hSDC;
  else if (name(nm).primDef)
    hDCMemory = Selected ? nBrowse.hPDCSel : nBrowse.hPDC;
  else
    hDCMemory = Selected ? nBrowse.hNDCSel : nBrowse.hNDC;

  BitBlt(lpdis->hDC, (lpdis->rcItem.left)+4, (lpdis->rcItem.top), bm.bmWidth, bm.bmHeight, hDCMemory, 0, 0, SRCCOPY);

  /* If selected draw rectangle */
  if ((lpdis->itemState & ODS_SELECTED)&&(lpdis->itemState & ODS_FOCUS)) {
    DrawFocusRect(lpdis->hDC, &(lpdis->rcItem));
  }
  return TRUE;
}

static Void local doPaint_Names(HWND hDlg) {
  RECT    aRect, DlgRect;
  HBITMAP hBitmap = LoadBitmap(hThisInstance, "NAMESDLGBMP");
  GetWindowRect(hDlg, &DlgRect);
  GetWindowRect(GetDlgItem(hDlg, ID_BROWSEBMP), &aRect);
  PutBitmap(hBitmap, hDlg, aRect.left-DlgRect.left-GetSystemMetrics(SM_CXDLGFRAME),
	    aRect.top-DlgRect.top-GetSystemMetrics(SM_CYDLGFRAME)-GetSystemMetrics(SM_CYCAPTION),
	    SRCCOPY);
  DeleteObject(hBitmap);
}

static Bool local doCommand_Names(HWND hDlg, WORD wId, WORD NotifyCode)
{
  INT    theName;
  Name   nm;
  Script script;

  switch (wId) {
  case LB_NAMES:
    switch(NotifyCode) {
    case LBN_SELCHANGE: /* Select a new name */
      theName = (UINT) SendDlgItemMessage(hDlg, LB_NAMES, LB_GETCURSEL, 0, 0L);
      SetName(hDlg, theName, nBrowse.names);
      break;
    case LBN_DBLCLK: /* Open in text editor script file with name definition */
      /* Get the selected name */
      theName = (UINT) SendDlgItemMessage(hDlg, LB_NAMES, LB_GETCURSEL, 0, 0L);
      nm = nth(theName, nBrowse.names);      
      if (!name(nm).primDef) {
	script = startNewScript(0);
	setLastEdit(scriptName[scriptThisName(nm)], name(nm).line);
	dropScriptsFrom(script);
	runEditor();
      } else {
	MessageBox(hDlg, "Primitive function:\nNo definition available.", appName, MB_ICONINFORMATION | MB_OK);
      }
      break;
    }
    break;

  case IDC_SEARCHNAME:  /* Search a name */
    if (HIBYTE(NotifyCode) == HIBYTE(EN_CHANGE)) {
      /* Get edit control contents */
      SendDlgItemMessage(hDlg, IDC_SEARCHNAME, WM_GETTEXT, MAXBUF, (LPARAM) ((LPSTR) buffer));
      /* Search in names list box */
      SendDlgItemMessage(hDlg, LB_NAMES, LB_SELECTSTRING, 0, (LPARAM) ((LPSTR) buffer));
      /* Update window contents */
      DlgSendMessage(hDlg, WM_COMMAND, LB_NAMES, MAKELONG(0, LBN_SELCHANGE));
    }
    break;
    
  case ID_EDITNAME: /* Pushed on Edit name button */
    if (SendDlgItemMessage(hDlg, LB_NAMES, LB_GETCURSEL, 0, 0L) != LB_ERR)
      DlgSendMessage(hDlg, WM_COMMAND, LB_NAMES, MAKELONG(0, LBN_DBLCLK));
    break;
    
  case IDCANCEL: /* Close dialog */
  case IDOK:
    EndDialog(hDlg, TRUE);
    return TRUE;
    
  default:
    return TRUE;
  }
  return FALSE;
}

#pragma argsused
/* Handles browse names dialog box */
BOOL FAR APIENTRY _export BrowseNamesDlgProc(HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam)
{
  switch (msg) {
  case WM_INITDIALOG:
    doInitDialog_Names(hDlg);
    break;
  case WM_DESTROY:
    doDestroy_Names();
    break;
  case WM_MEASUREITEM: 
    {
      LPMEASUREITEMSTRUCT lpmis = (LPMEASUREITEMSTRUCT) lParam;
      if (lpmis->CtlID == LB_NAMES) {
	doMeasureItem_Names(lpmis);
	return TRUE;
      }
    }
    break;
  case WM_DRAWITEM:
    {
      LPDRAWITEMSTRUCT lpdis = (LPDRAWITEMSTRUCT) lParam;
      if (lpdis->CtlID == LB_NAMES) {
	return doDrawItem_Names(hDlg, (DRAWITEMSTRUCT FAR *) lParam);
      }
    }
    break;
  case WM_PAINT:
    doPaint_Names(hDlg);
    break;
  case WM_COMMAND:
    return doCommand_Names(hDlg,CMDitem(wParam,lParam),CMDdata(wParam,lParam));
 }
 return FALSE;
}

static Int numCfuns;
static Int numSfuns;

/* A new Tycon was selected */
static local VOID SetTycon(HWND hDlg, UINT currTycon, List tycons) {
    List  tyconList = tycons;
    Tycon tc;
    Inst  in;
    UINT  j;
    Type  t;

    tc = nth(currTycon,tyconList);
    numCfuns = 0;
    numSfuns = 0;

    t = tc;
    for (j=0; j<tycon(tc).arity; ++j)
	t = ap(t,mkOffset(j));

    switch(tycon(tc).what) {
	case RESTRICTSYN :
	case SYNONYM     : fprintf(stdstr,"Type synonym");
			   break;
	case NEWTYPE	 : fprintf(stdstr,"Newtype constructor");
			   break;
	case DATATYPE    : fprintf(stdstr,"Datatype constructor");
			   break;
	default          : fprintf(stdstr,"Type constructor");
			   break;
    }

    if (kindExpert) {
	fprintf(stdstr," with kind ");
	printKind(stdstr,tycon(tc).kind);
    }

    fprintf(stdstr, ":\n");
    SendDlgItemMessage(hDlg, LB_CONS ,LB_RESETCONTENT, 0, 0L);
    SendDlgItemMessage(hDlg, LB_CONS ,LB_ADDSTRING, 0, (LONG)(LPSTR) stdstrbuff);

    switch (tycon(tc).what) {
     case SYNONYM:
     fprintf(stdstr, "type ");
     printType(stdstr, t);
     fprintf(stdstr, " = ");
     printType(stdstr, tycon(tc).defn);
     fprintf(stdstr, "\n");
     SendDlgItemMessage(hDlg, LB_CONS ,LB_ADDSTRING, 0, (LONG)(LPSTR) stdstrbuff);
     break;

   case NEWTYPE :
   case DATATYPE: {
     List cs = tycon(tc).defn;
     fprintf  (stdstr,(tycon(tc).what==DATATYPE ? "data " : "newtype "));
     printType(stdstr, t);
     fprintf  (stdstr, " = ...\n");
     SendDlgItemMessage(hDlg, LB_CONS ,LB_ADDSTRING, 0, (LONG)(LPSTR) stdstrbuff);
     for (; nonNull(cs); cs=tl(cs)) {
	 printExp (stdstr, hd(cs));
	 fprintf  (stdstr, " :: ");
	 printType(stdstr, name(hd(cs)).type);
	 fprintf  (stdstr, "\n");
	 SendDlgItemMessage(hDlg, LB_CONS ,LB_ADDSTRING, 0, (LONG)(LPSTR) stdstrbuff);
	 SendDlgItemMessage(hDlg, LB_CONS, LB_SETCURSEL, (WPARAM)(-1), 0L);
	 if (isCfun(hd(cs)))
	     numCfuns++;
	 else
	     numSfuns++;
     }
   }
   break;

   case RESTRICTSYN:
     fprintf  (stdstr, "type");
     printType(stdstr, t);
     fprintf  (stdstr, " = <restricted>\n");
     SendDlgItemMessage(hDlg, LB_CONS ,LB_ADDSTRING, 0, (LONG)(LPSTR) stdstrbuff);
     break;
  }

  for (in=findFirstInst(tc); nonNull(in); in=findNextInst(tc,in)) {
      fprintf(stdstr,"instance ");
      if (nonNull(inst(in).specifics)) {
	  printContext(stdstr,inst(in).specifics);
	  fprintf(stdstr," => ");
      }
      printPred(stdstr,inst(in).head);
      fprintf(stdstr,"\n");
      SendDlgItemMessage(hDlg, LB_CONS ,LB_ADDSTRING, 0, (LONG)(LPSTR) stdstrbuff);
      SendDlgItemMessage(hDlg, LB_CONS, LB_SETCURSEL, (WPARAM)(-1), 0L);
  }
  SendDlgItemMessage(hDlg, LB_CONS, LB_SETCURSEL, (WPARAM)(-1), 0L);
}


/* Handles browse Tycons dialog box */
#pragma argsused
BOOL FAR APIENTRY _export BrowseTyconsDlgProc(HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam)
{
 static        List tyconList = NIL; /* ADR note: This is not marked during GC */
 List          tycons=NIL;
 Tycon         tc;
 UINT          theTycon;
 Script        script;
 WORD          NotifyCode = CMDdata(wParam,lParam);
 WORD	       wId        = CMDitem(wParam,lParam);
 RECT          aRect, DlgRect;
 HBITMAP       hBitmap;
 HBITMAP       hBmp, hsvBmp0, hsvBmp1, hsvBmp2, hsvBmp3, hsvBmp4, hsvBmp5,
	       hsvBmp6, hsvBmp7, hsvBmp8, hsvBmp9;
 BITMAP	       bm;
 static HDC    hDCMemory, hTCDC, hTCDCSel, hDDC, hDDCSel, hTSDC, hTSDCSel,
	       hTNDC, hTNDCSel, hSDC, hSDCSel;
 DRAWITEMSTRUCT FAR *lpdis;
 LPMEASUREITEMSTRUCT lpmis;
 BOOL                Selected = FALSE;
 UINT	       i;

 switch (msg) {

     case WM_INITDIALOG:
	  tyconList = addTyconsMatching((String)0, NIL);
	  for (tycons=tyconList; nonNull(tycons); tycons=tl(tycons)) {
	     tc = hd(tycons);
	     fprintf(stdstr, "%s\n",textToStr(tycon(tc).text));
	     SendDlgItemMessage(hDlg, LB_TYCONS, LB_SETCOLUMNWIDTH, (WPARAM)(100), 0L);
	     SendDlgItemMessage(hDlg, LB_TYCONS ,LB_ADDSTRING, 0, (LONG)(LPSTR) stdstrbuff);
	     SendDlgItemMessage(hDlg, LB_TYCONS, LB_SETCURSEL, 0, 0L);
	  }

	  theTycon = 0;
	  SetTycon(hDlg, theTycon, tyconList);

	  hsvBmp0 = CreatehDC(hDlg, &hTCDC,    "TYPECONSBMP", GetSysColor(COLOR_WINDOW));
	  hsvBmp1 = CreatehDC(hDlg, &hTCDCSel, "TYPECONSBMP", GetSysColor(COLOR_HIGHLIGHT));
	  hsvBmp2 = CreatehDC(hDlg, &hDDC,     "DATACONSBMP", GetSysColor(COLOR_WINDOW));
	  hsvBmp3 = CreatehDC(hDlg, &hDDCSel,  "DATACONSBMP", GetSysColor(COLOR_HIGHLIGHT));
	  hsvBmp4 = CreatehDC(hDlg, &hTSDC,    "TYPESINBMP", GetSysColor(COLOR_WINDOW));
	  hsvBmp5 = CreatehDC(hDlg, &hTSDCSel, "TYPESINBMP", GetSysColor(COLOR_HIGHLIGHT));
	  hsvBmp6 = CreatehDC(hDlg, &hTNDC,    "NEWTYPEBMP", GetSysColor(COLOR_WINDOW));
	  hsvBmp7 = CreatehDC(hDlg, &hTNDCSel, "NEWTYPEBMP", GetSysColor(COLOR_HIGHLIGHT));
	  hsvBmp8 = CreatehDC(hDlg, &hSDC,     "SELECTORBMP", GetSysColor(COLOR_WINDOW));
	  hsvBmp9 = CreatehDC(hDlg, &hSDCSel,  "SELECTORBMP", GetSysColor(COLOR_HIGHLIGHT));
	  break;

     case WM_DESTROY:
	  SelectObject(hTCDC,    hsvBmp0); DeleteDC(hTCDC);
	  SelectObject(hTCDCSel, hsvBmp1); DeleteDC(hTCDCSel);
	  SelectObject(hDDC,     hsvBmp2); DeleteDC(hDDC);
	  SelectObject(hDDCSel,  hsvBmp3); DeleteDC(hDDCSel);
	  SelectObject(hTSDC,    hsvBmp4); DeleteDC(hTSDC);
	  SelectObject(hTSDCSel, hsvBmp5); DeleteDC(hTSDCSel);
	  SelectObject(hTNDC,    hsvBmp6); DeleteDC(hTNDC);
	  SelectObject(hTNDCSel, hsvBmp7); DeleteDC(hTNDCSel);
	  SelectObject(hSDC,     hsvBmp8); DeleteDC(hSDC);
	  SelectObject(hSDCSel,  hsvBmp9); DeleteDC(hSDCSel);
	  break;

     case WM_MEASUREITEM:
	 lpdis = (DRAWITEMSTRUCT FAR *) lParam;

	 if (lpdis->CtlID == LB_TYCONS || lpdis->CtlID == LB_CONS ) {
	  lpmis = (LPMEASUREITEMSTRUCT) lParam;

	  /* Set the height of the list box items to Bitmap height */
	  hBmp = LoadBitmap(hThisInstance, "CLASSBMP");
	  GetObject(hBmp, sizeof(BITMAP), &bm);
	  DeleteObject(hBmp);

	  lpmis->itemHeight = bm.bmHeight+1;
	  return TRUE;
	 }
	 break;

     case WM_DRAWITEM: {
	 Int offs = 0;

	 lpdis = (DRAWITEMSTRUCT FAR *) lParam;

	 if (lpdis->CtlID == LB_TYCONS || lpdis->CtlID == LB_CONS ) {

	  if (lpdis->itemID == (UINT)-1)
	   return TRUE;

	  switch (lpdis->itemAction) {
	    case ODA_DRAWENTIRE:
	    case ODA_SELECT:
	    case ODA_FOCUS:
		if ((lpdis->itemState & ODS_SELECTED) /*&& (lpdis->itemState & ODS_FOCUS)*/) {
		  SetBkColor(lpdis->hDC, GetSysColor(COLOR_HIGHLIGHT));
		  SetTextColor(lpdis->hDC, GetSysColor(COLOR_HIGHLIGHTTEXT));
		  Selected = TRUE;
		}
		else {
		  SetBkColor(lpdis->hDC, GetSysColor(COLOR_WINDOW));
		  SetTextColor(lpdis->hDC, GetSysColor(COLOR_WINDOWTEXT));
		}
		break;

	    default:
		return FALSE;
	  }

	   SendDlgItemMessage(hDlg, lpdis->CtlID, LB_GETTEXT, lpdis->itemID, (LPARAM) buffer);

	   if (lpdis->CtlID == LB_CONS)
	       if (lpdis->itemID <= 1 || lpdis->itemID>=2+numCfuns+numSfuns) {
		   ExtTextOut(lpdis->hDC, lpdis->rcItem.left+4, lpdis->rcItem.top, (ETO_CLIPPED|ETO_OPAQUE), &(lpdis->rcItem), buffer, strlen(buffer), NULL);
		   return TRUE;
	       }
	       else
		   offs = 15;
	   }

	   ExtTextOut(lpdis->hDC, lpdis->rcItem.left+21+offs, lpdis->rcItem.top, (ETO_CLIPPED|ETO_OPAQUE), &(lpdis->rcItem), buffer, strlen(buffer), NULL);

	   /* Get bitmap size */
	   hBmp = LoadBitmap(hThisInstance, "CLASSBMP");
	   GetObject(hBmp, sizeof(BITMAP), &bm);
	   DeleteObject(hBmp);

	   switch (lpdis->CtlID) {
	     case LB_TYCONS:
		theTycon = (UINT) lpdis->itemID;
		for (i=0, tycons=tyconList; i<theTycon; tycons=tl(tycons))
		    i++;
		tc = hd(tycons);

		switch(tycon(tc).what) {
		    case RESTRICTSYN :
		    case SYNONYM     : hDCMemory = Selected ? hTSDCSel : hTSDC;
				       break;
		    case DATATYPE    : hDCMemory = Selected ? hTCDCSel : hTCDC;
				       break;
		    case NEWTYPE     : hDCMemory = Selected ? hTNDCSel : hTNDC;
				       break;
		}
		break;

	     case LB_CONS:
		if (((UINT)(lpdis->itemID)-2)>=numCfuns)
		    hDCMemory = Selected ? hSDCSel : hSDC;
		else
		    hDCMemory = Selected ? hDDCSel : hDDC;
		break;
	   }

	   BitBlt(lpdis->hDC, (lpdis->rcItem.left)+4+offs, (lpdis->rcItem.top), bm.bmWidth, bm.bmHeight, hDCMemory, 0, 0, SRCCOPY);

	   /* If selected draw rectangle */
	   if ((lpdis->itemState & ODS_SELECTED)&&(lpdis->itemState & ODS_FOCUS)) {
	     DrawFocusRect(lpdis->hDC, &(lpdis->rcItem));
	   }
	 }
	 return TRUE;

  case WM_PAINT:

     GetWindowRect(hDlg, &DlgRect);
     GetWindowRect(GetDlgItem(hDlg, ID_BROWSEBMP), &aRect);

     hBitmap = LoadBitmap(hThisInstance, "TYCONSDLGBMP");
     PutBitmap(hBitmap, hDlg, aRect.left-DlgRect.left-GetSystemMetrics(SM_CXDLGFRAME),
	       aRect.top-DlgRect.top-GetSystemMetrics(SM_CYDLGFRAME)-GetSystemMetrics(SM_CYCAPTION),
	       SRCCOPY);
     DeleteObject(hBitmap);
     break;

  case WM_COMMAND:
   switch (wId) {
      case LB_TYCONS:
	switch(NotifyCode) {
	   case LBN_SELCHANGE: {
	     /* A new tycon was selected */
	     theTycon = (UINT) SendDlgItemMessage(hDlg, LB_TYCONS, LB_GETCURSEL, 0, 0L);
	     SetTycon(hDlg, theTycon, tyconList);
	   }
	   break;

	   case LBN_DBLCLK: {
	     /* Open in text editor script file with tycon definition */
	     CHAR   TyconStr[300];
	     INT    TheTycon;
	     Tycon  tc;

	     /* Get selected tycon */
	     TheTycon = (UINT) SendDlgItemMessage(hDlg, LB_TYCONS ,LB_GETCURSEL, 0, 0L);
	     tc = nth(TheTycon,tyconList);

	     if (isTycon(tc) && tycon(tc).line) {
	       script = startNewScript(0);
	       setLastEdit(scriptName[scriptThisTycon(tc)], tycon(tc).line);
	       dropScriptsFrom(script);
	       runEditor();
	     }
	     else {
	       MessageBox(hDlg, "Primitive type:\nNo definition available.", appName, MB_ICONINFORMATION | MB_OK);
	     }
	   }
	   break;
	}
	break;

      case IDC_SEARCHTYCON:  /* Search a name */
	switch(HIBYTE(wId)) {
	  case HIBYTE(EN_CHANGE): {
	    /* Get edit control contents */
	    SendDlgItemMessage(hDlg, IDC_SEARCHTYCON, WM_GETTEXT, MAXBUF, (LPARAM) ((LPSTR) buffer));

	    /* Search in names list box */
	    SendDlgItemMessage(hDlg, LB_TYCONS, LB_SELECTSTRING, 0, (LPARAM) ((LPSTR) buffer));

	    /* Update window contents */
	    DlgSendMessage(hDlg, WM_COMMAND, LB_TYCONS, MAKELONG(0, LBN_SELCHANGE));
	  }
	  break;
	}
	break;

      case ID_EDITTYCON: /* Pushed on Edit tycon button */
	if (SendDlgItemMessage(hDlg, LB_TYCONS, LB_GETCURSEL, 0, 0L) != LB_ERR)
	 DlgSendMessage(hDlg, WM_COMMAND, LB_TYCONS, MAKELONG(0, LBN_DBLCLK));
	break;

      case IDCANCEL:  /* Close dialog */
      case IDOK:
	EndDialog(hDlg, TRUE);
	return TRUE;

      default:
	return TRUE;
   }
 }
 return FALSE;
}

/*-----------------------------------------------------------------------------
 * Class Hierarchy browser
 *
 * When the hierarchy browser is created, we call buildClassGraph to
 *  construct a table of class-position pairs.
 * The positions in the table can be adjusted using left button to drag nodes.
 * Edges (superclass relationships) are added in as the graph is being drawn.
 *----------------------------------------------------------------------------*/

/* Layout controls */

#define VERTICAL_SEPARATION      35
#define HORIZONTAL_SEPARATION    50
#define INIT_COLUMN              10
#define INIT_ROW		 20
#define MAX_WIDTH  		600
#define MAX_HEIGHT 		500

/* structure used to draw class hierarchy */
typedef struct {
  RECT   Pos;
  Class  Class;
} HierarchyInfo;

typedef INT Node;
static  HierarchyInfo *Nodes    = NULL; /* The list of nodes             */
static  Node           LastNode = 0;

static Node local findClassInNodes   Args((Class));
static Bool local allocNodes         Args((INT));
static Void local drawNode           Args((HDC, Node));
static Void local drawClassRelations Args((HDC));

static Bool local allocNodes(INT n) /* Get memory for nodes list */
{
#if DOS
   if (Nodes) farfree(Nodes);
   Nodes = farcalloc((ULONG)(sizeof(HierarchyInfo)), (ULONG) n);
#else
   if (Nodes) free(Nodes);
   Nodes = calloc((ULONG)(sizeof(HierarchyInfo)), (ULONG) n);
#endif
   LastNode = 0;
   return (Nodes != NULL);
}

static Node local findClassInNodes( Class cls ) {
  Node n;
  for(n=0; n<LastNode; n++) {
    if (Nodes[n].Class == cls) {
      return n;
    }
  }
  return -1;
}

static Bool local isParentOf(Class parent, Class child)
{
  List supers;
  for(supers=cclass(child).supers; nonNull(supers); supers=tl(supers)) {
    if (getHead(hd(supers)) == parent) {
      return TRUE;
    }
  }
  return FALSE;
}


/* Add a class and all its children recursive */
/* returns the row for placing next node      */
static INT local addClassToGraph(HDC hDC, INT Column, INT startRow, Class ThisClass)
{
  Node newNode = LastNode++;
  SIZE Size;
  INT  row = startRow;

  /* Get size of class name on the screen */
  fprintf(stdstr, "%s\n",textToStr(cclass(ThisClass).text));
  GetTextExtentPoint(hDC,(LPSTR)stdstrbuff, (INT)strlen(stdstrbuff), &Size);

  Nodes[newNode].Class      = ThisClass;
  Nodes[newNode].Pos.left   = Column;
  Nodes[newNode].Pos.top    = startRow;
  Nodes[newNode].Pos.right  = Nodes[newNode].Pos.left + Size.cx;
  Nodes[newNode].Pos.bottom = Nodes[newNode].Pos.top  + Size.cy;

  /* Add subclasses of ThisClass */
  { Class cls;
    INT   col   = Nodes[newNode].Pos.right+HORIZONTAL_SEPARATION;
    INT   child = 0;
    for(cls=CLASSMIN; cls<classMax(); cls++) {
      if (-1 == findClassInNodes(cls) /* Check for cycles in graph */
	  && isParentOf(ThisClass, cls)) {
	if (child++ > 0) {
	  row += VERTICAL_SEPARATION;
	}
	row = addClassToGraph(hDC, col, row, cls);
      }
    }
  }
  /* Set to average position of children */
  { INT height = row-startRow;
    Nodes[newNode].Pos.top    += height/2;
    Nodes[newNode].Pos.bottom += height/2;
  }
  return row;
}

static Void local buildClassGraph( HDC hDC )
{
  INT   row = INIT_ROW;
  Class cls;
  for (cls=CLASSMIN; cls<classMax(); cls++) {
    if (cclass(cls).numSupers == 0) {
      row = addClassToGraph(hDC, INIT_COLUMN, row, cls) + VERTICAL_SEPARATION;
    }
  }
  /* Since Haskell has acyclic class dependencies, we should be done by now;
   * but it does no harm to make sure.
   */
  for(cls=CLASSMIN; cls<classMax(); cls++) {
    if (-1 == findClassInNodes(cls)) { /* Not added yet */
      row = addClassToGraph(hDC, INIT_COLUMN, row, cls) + VERTICAL_SEPARATION;
    }
  }
}

static Void local drawClassRelations(HDC hDC)
{
  Class cls;
  for(cls=CLASSMIN; cls<classMax(); cls++) {
    List supers;
    for(supers=cclass(cls).supers; nonNull(supers); supers=tl(supers)) {
      Class parent = getHead(hd(supers));
      if (isClass(parent)) {
	if (parent == cls) {     /* child of itself - draw an arc */
	  Class source = findClassInNodes(cls);
	  Arc(hDC, Nodes[source].Pos.right-5,  Nodes[source].Pos.bottom-5,
		   Nodes[source].Pos.right+15, Nodes[source].Pos.bottom+20,
		   Nodes[source].Pos.right-5,  Nodes[source].Pos.bottom-5,
		   Nodes[source].Pos.right-4,  Nodes[source].Pos.bottom-4);
	} else { 	               /* Join the two classes with a line */
	  Class source = findClassInNodes(parent);
	  Class target = findClassInNodes(cls);

	  INT sx = Nodes[source].Pos.right + 4;
	  INT sy = Nodes[source].Pos.top
		   + (Nodes[source].Pos.bottom - Nodes[source].Pos.top)/2;
	  INT tx = Nodes[target].Pos.left  - 4;
	  INT ty = Nodes[target].Pos.top
		   + (Nodes[target].Pos.bottom - Nodes[target].Pos.top)/2;

	  MoveToEx(hDC, sx, sy,NULL);
	  LineTo(hDC, tx, ty);
	}
      }
    }
  }
}

static Void local drawNode( HDC hDC, Node n )
{
  /* frame */
  Rectangle(hDC, Nodes[n].Pos.left-4, Nodes[n].Pos.top-2,
		 Nodes[n].Pos.right+4,Nodes[n].Pos.bottom+2);

  /* frame shadow */
  MoveToEx(hDC, Nodes[n].Pos.right+4, Nodes[n].Pos.top, NULL);
  LineTo(hDC,   Nodes[n].Pos.right+4, Nodes[n].Pos.bottom+2);
  LineTo(hDC,   Nodes[n].Pos.left-2,  Nodes[n].Pos.bottom+2);

  /* class text */
  fprintf(stdstr, "%s\n",textToStr(cclass(Nodes[n].Class).text));
  TextOut(hDC, Nodes[n].Pos.left, Nodes[n].Pos.top, 
	       (LPSTR)stdstrbuff, (INT)strlen(stdstrbuff));
}

typedef struct {
  HCURSOR 	hMoveClassCursor;
  HCURSOR       hNormalCursor;
  Node		SelectedClass;            
  BOOL 		Moved;                   
  INT 		ClassesTopX, ClassesTopY;
  INT 		XOffset,     YOffset;
  INT 		RealWidth,   RealHeight;  /* size of window      */
  INT           width,       height;      /* size of total graph */
} ClassBrowserState;

static ClassBrowserState cBrowse; /* state of browser */

static VOID local setClassBrowserSize() {
  Node i;
  INT  width  = 0;
  INT  height = 0;
  for (i=0; i<LastNode; i++) {
    width  = max(width, Nodes[i].Pos.right);
    height = max(height,Nodes[i].Pos.bottom);
  }
  cBrowse.width  = width  + 2*GetSystemMetrics(SM_CXFRAME);
  cBrowse.height = height + 2*GetSystemMetrics(SM_CYFRAME)
			 + GetSystemMetrics(SM_CYCAPTION);
  MoveWindow (hWndClasses, cBrowse.ClassesTopX, cBrowse.ClassesTopY,
	      cBrowse.width  + GetSystemMetrics(SM_CXVSCROLL)+10,
	      cBrowse.height + GetSystemMetrics(SM_CYHSCROLL)+10, TRUE);
}

static Void local doGetMinMaxInfo_Classes(MINMAXINFO FAR*lpmmi)
{
   lpmmi->ptMinTrackSize.x = 50;
   lpmmi->ptMinTrackSize.y = 50;

   lpmmi->ptMaxTrackSize.x = MAX_WIDTH;
   lpmmi->ptMaxTrackSize.y = MAX_HEIGHT;

   lpmmi->ptMaxSize.x = MAX_WIDTH;
   lpmmi->ptMaxSize.y = MAX_HEIGHT;
}

static Void local doMouseMove_Classes(HWND hWnd, INT x, INT y)
{
  if (cBrowse.SelectedClass < 0) {
    SetCursor(cBrowse.hNormalCursor);
  } else {
    Node n = cBrowse.SelectedClass;
    INT  dx, dy;
    RECT ClearRect;
      
    SetCursor(cBrowse.hMoveClassCursor);

    /* Don't allow move it out of window */
    x = max(5, min(cBrowse.RealWidth-10, x));
    y = max(5, min(cBrowse.RealHeight-10,y));

    dx = x - Nodes[n].Pos.left;
    dy = y - Nodes[n].Pos.top;

    ClearRect.left      = Nodes[n].Pos.left   - 5;
    ClearRect.right     = Nodes[n].Pos.right  + 5;
    ClearRect.top       = Nodes[n].Pos.top    - 3;
    ClearRect.bottom    = Nodes[n].Pos.bottom + 3;

    InvalidateRect(hWnd, &ClearRect, FALSE); /* erase old class */

    Nodes[n].Pos.left   += dx;
    Nodes[n].Pos.top    += dy;
    Nodes[n].Pos.right  += dx;
    Nodes[n].Pos.bottom += dy;

    ClearRect.left      = Nodes[n].Pos.left   - 5;
    ClearRect.right     = Nodes[n].Pos.right  + 5;
    ClearRect.top       = Nodes[n].Pos.top    - 3;
    ClearRect.bottom    = Nodes[n].Pos.bottom + 3;

    InvalidateRect(hWnd, &ClearRect, TRUE);  /* draw new class */

    SendMessage(hWnd, WM_PAINT, 0, 0L);
  }
}

#define clamp(_min,_max,x) max(_min,min(_max,x))

static Void local setOffset_Classes(HWND hWnd, INT x, INT y)
{
  Node n;
  INT dx, dy;

  x  = clamp(-cBrowse.width, 0,x);
  y  = clamp(-cBrowse.height,0,y);
  dx = x - cBrowse.XOffset;
  dy = y - cBrowse.YOffset;

  for (n=0; n<LastNode; n++) {
    Nodes[n].Pos.left   += dx;
    Nodes[n].Pos.right  += dx;
    Nodes[n].Pos.top    += dy;
    Nodes[n].Pos.bottom += dy;
  }

  cBrowse.XOffset = x;
  cBrowse.YOffset = y;
  SetScrollPos(hWnd, SB_HORZ, -x, TRUE);
  SetScrollPos(hWnd, SB_VERT, -y, TRUE);

  ScrollWindow(hWnd, dx, dy, NULL, NULL);
  InvalidateRect(hWnd,NULL,TRUE);
  UpdateWindow(hWnd);
}

#undef clamp

static Void local lButtonDown_Classes(HWND hWnd, INT x, INT y)
{
  /* Select a class to drag it */
  Node n;
  for(n=0; n<LastNode; n++) {
    if (Nodes[n].Pos.left-4 < x && Nodes[n].Pos.right+4  > x &&
	Nodes[n].Pos.top-2  < y && Nodes[n].Pos.bottom+2 > y) {

      SetCursor(cBrowse.hMoveClassCursor);
      cBrowse.SelectedClass = n;

      InvalidateRect(hWnd, NULL, TRUE);
      SetCapture(hWnd);
      return;
    }
  }
}

static Void local lButtonUp_Classes(HWND hWnd, INT x, INT y)
{
  if (cBrowse.SelectedClass >= 0) {
    Node n = cBrowse.SelectedClass;
    INT width  = Nodes[n].Pos.right  - Nodes[n].Pos.left;
    INT height = Nodes[n].Pos.bottom - Nodes[n].Pos.top;

    ReleaseCapture();

    if (cBrowse.Moved) {
      Nodes[n].Pos.left   = x;
      Nodes[n].Pos.top    = y;
      Nodes[n].Pos.right  = Nodes[n].Pos.left + width;
      Nodes[n].Pos.bottom = Nodes[n].Pos.top  + height;
    }

    cBrowse.SelectedClass = -1;
    cBrowse.Moved = FALSE;
    SetCursor(cBrowse.hNormalCursor);

    InvalidateRect(hWnd, NULL, TRUE);
    SendMessage(hWnd, WM_PAINT, 0, 0L);

    setClassBrowserSize();
  }
}

static Void local doPaint_Classes(HWND hWnd)
{
  PAINTSTRUCT ps;
  HDC         hDC;
  HFONT       hFont=NULL, hSaveFont;
  COLORREF    SaveColor;
  Node        i;

  hDC = BeginPaint(hWnd, &ps);

  /* Get font */
  hFont = CreateFont( 15,
		      7,
		      0,
		      0,
		      FW_BOLD,
		      FALSE,
		      FALSE,
		      FALSE,
		      ANSI_CHARSET,
		      OUT_TT_PRECIS,
		      CLIP_TT_ALWAYS,
		      DEFAULT_QUALITY,
		      FIXED_PITCH | FF_MODERN,
		      "Arial New");
  if (hFont)
    hSaveFont = SelectObject(hDC, hFont);
  SaveColor = SetTextColor(hDC, RGB(0,0,190));   /* Blue Color for text  */

  if (cBrowse.SelectedClass < 0) {                /* not dragging a class */
    drawClassRelations(hDC);
  }
  for(i=0; i<LastNode; i++) {                   
      drawNode(hDC, i);
  }

  SetTextColor(hDC, SaveColor);                  /* Restore color        */
  if (hFont) {                                   /* Restore font         */
    SelectObject(hDC, hSaveFont);
    DeleteObject(hFont);
  }
  EndPaint(hWnd, &ps);
}

static VOID local doCreate_Classes(HWND hWnd)
{
   PAINTSTRUCT ps;
   HDC   hDC;
   HFONT hFont=NULL, hSaveFont;
   INT   numClasses = classMax() - CLASSMIN; /* total number of classes */

   cBrowse.hNormalCursor = LoadCursor(NULL, IDC_ARROW);
   cBrowse.hMoveClassCursor = LoadCursor(hThisInstance, "MOVECLASSCURSOR");
   cBrowse.SelectedClass = -1;            
   cBrowse.Moved         = FALSE;                   
   cBrowse.ClassesTopX   = 10;
   cBrowse.ClassesTopY   = 10;
   cBrowse.XOffset       = 0;
   cBrowse.YOffset       = 0;

   if (!allocNodes(numClasses)) {
     MessageBox(hWnd, "Out of memory: create nodes list", NULL, MB_ICONEXCLAMATION | MB_OK);
     return;
   }

   hDC = BeginPaint(hWnd, &ps);

   /* Get a text font */
   hFont = CreateFont( 15,
			7,
			0,
			0,
			FW_BOLD,
			FALSE,
			FALSE,
			FALSE,
			ANSI_CHARSET,
			OUT_TT_PRECIS,
			CLIP_TT_ALWAYS,
			DEFAULT_QUALITY,
			FIXED_PITCH | FF_MODERN,
			"Arial New");

   if (hFont)
     hSaveFont = SelectObject(hDC, hFont);

   buildClassGraph( hDC );

   if (hFont) {                       /* Restore font */
     SelectObject(hDC, hSaveFont);
     DeleteObject(hFont);
   }

   EndPaint(hWnd, &ps);

   /* Show upper-left part of window */
   SetScrollPos(hWnd, SB_HORZ, 0, TRUE);
   SetScrollPos(hWnd, SB_VERT, 0, TRUE);

   setClassBrowserSize();
}

static Void local doDestroy_Classes() {
#if DOS
   if (Nodes) farfree(Nodes);
#else
   if (Nodes) free(Nodes);
#endif
   Nodes = NULL;
   LastNode = 0;
   DestroyCursor(cBrowse.hMoveClassCursor);
   hWndClasses=NULL;
}

static Void local doMove_Classes(INT x, INT y) {
   /* We must subtract as it gets client position */
   cBrowse.ClassesTopX = x - GetSystemMetrics(SM_CXFRAME);
   cBrowse.ClassesTopY = y - GetSystemMetrics(SM_CYCAPTION)
			   - GetSystemMetrics(SM_CYFRAME)
			   + GetSystemMetrics(SM_CYBORDER);
}

static Void local doSize_Classes(HWND hWnd, INT width, INT height)
{
   static BOOL RecursiveCall=FALSE;
   if (!RecursiveCall) {
     RecursiveCall = TRUE;

     cBrowse.RealWidth  = width;
     cBrowse.RealHeight = height;

     if (cBrowse.RealWidth < cBrowse.width || cBrowse.XOffset) {
       SetScrollRange(hWnd, SB_HORZ, 0, cBrowse.width, TRUE);
     } else { /* Hide scroll bar */
       SetScrollRange(hWnd, SB_HORZ, 0, 0, TRUE);
     }

     if (cBrowse.RealHeight < cBrowse.height || cBrowse.YOffset) {
       SetScrollRange(hWnd, SB_VERT, 0, cBrowse.height, TRUE);
     } else { /* Hide scroll bar */
       SetScrollRange(hWnd, SB_VERT, 0, 0, TRUE);
     }

     RecursiveCall = FALSE;
   }
}


/* Hierarchy class window proc */
LRESULT FAR APIENTRY _export ClassesWndProc     (HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
  switch (msg) {
  case WM_CREATE: 
    doCreate_Classes(hWnd);
    break;
  case WM_DESTROY:
    doDestroy_Classes();
    return (LRESULT) FALSE;
  case WM_GETMINMAXINFO: 
    doGetMinMaxInfo_Classes((MINMAXINFO FAR*) lParam);
    break;
  case WM_SIZE: 
    doSize_Classes(hWnd, (INT) LOWORD(lParam), (INT) HIWORD(lParam));
    break;
  case WM_MOVE: 
    doMove_Classes((INT) LOWORD(lParam), (INT) HIWORD(lParam));
    break;
  case WM_HSCROLL: 
    switch(LOWORD(wParam)) {
    case SB_PAGEUP:
    case SB_LINEUP:
      setOffset_Classes(hWnd, cBrowse.XOffset + 5, cBrowse.YOffset);
      break;
    case SB_PAGEDOWN:
    case SB_LINEDOWN:
      setOffset_Classes(hWnd, cBrowse.XOffset + 5, cBrowse.YOffset);
      break;
    case SB_THUMBPOSITION:
#if DOS
      setOffset_Classes(hWnd, -LOWORD(wParam), cBrowse.YOffset);      
#else
      setOffset_Classes(hWnd, -HIWORD(wParam), cBrowse.YOffset);      
#endif
      break;
    }
    break;
  case WM_VSCROLL:
    switch(LOWORD(wParam)) {
    case SB_PAGEUP:
    case SB_LINEUP:
      setOffset_Classes(hWnd, cBrowse.XOffset, cBrowse.YOffset + 5);
      break; 
    case SB_PAGEDOWN:
    case SB_LINEDOWN:
      setOffset_Classes(hWnd, cBrowse.XOffset, cBrowse.YOffset - 5);
      break;
    case SB_THUMBPOSITION:
#if DOS
      setOffset_Classes(hWnd, cBrowse.XOffset, -LOWORD(wParam));
#else
      setOffset_Classes(hWnd, cBrowse.XOffset, -HIWORD(wParam));
#endif
      break;
    }
    break;
  case WM_PAINT: 
    doPaint_Classes(hWnd);
    break;
  case WM_LBUTTONDOWN: 
    lButtonDown_Classes(hWnd, LOWORD(lParam), HIWORD(lParam));
    break;
   case WM_LBUTTONUP:
     lButtonUp_Classes(hWnd, LOWORD(lParam), HIWORD(lParam));
     break;
   case WM_MOUSEMOVE:
     doMouseMove_Classes(hWnd, LOWORD(lParam), HIWORD(lParam));
     break;
  default:
    return DefWindowProc(hWnd, msg, wParam, lParam);
  }
  return (LONG) TRUE;
}


/* Create class hierarchy and show it on a window */
static VOID local Hierarchy(VOID)
{
 HWND hActiveWindow;
 RECT rActive, rWindow;

 if (hWndClasses) { /* If window exists keep its position */
   GetWindowRect(hWndClasses, &rWindow);
   DestroyWindow(hWndClasses);
 } else {
   GetWindowRect(hActiveWindow, &rActive);
   rWindow.top  = rActive.top + 50;
   rWindow.left = rActive.left + 50;
 }

 hActiveWindow = GetActiveWindow();

 hWndClasses =
  CreateWindow("HugsClassesWindow",
	       "Class Hierarchy",
	       WS_CAPTION | WS_BORDER | WS_SYSMENU | WS_THICKFRAME | WS_VSCROLL | WS_HSCROLL,
	       rWindow.left, rWindow.top, 0, 0,
	       (HWND) hActiveWindow,
	       (HMENU) NULL,
	       hThisInstance,
	       (LPSTR) NULL);

 if (!hWndClasses) {
   MessageBox (hWndMain, "Error creating window", appName, MB_ICONEXCLAMATION | MB_OK);
   return;
 }

 setClassBrowserSize();
 ShowWindow  (hWndClasses, SW_SHOWNORMAL);
 UpdateWindow(hWndClasses);

 SetFocus(hWndClasses);

 return;
}


