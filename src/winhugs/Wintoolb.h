/* --------------------------------------------------------------------------
 * WinToolB.h:	José Enrique Gallardo Ruiz, Feb 1999
 *
 * Hugs for Windows is Copyright (c) José Enrique Gallardo, Mark P Jones,
 * Alastair Reid and the Yale Haskell Group 1994-99, and is distributed as
 * Open Source software under the Artistic License; see the file "Artistic"
 * that is included in the distribution for details.
 *
 * This file contains Header file for a toolbar  definition
 * ------------------------------------------------------------------------*/


#define __WINTOOLB_H

#define MAXLNG	64
#define MAXBTS	35

/* One button on the tools bar */
typedef struct tagButton {
 WPARAM  Command;         	/* Command to execute when pushed         */
 CHAR    BitmapName[MAXBTS];	/* Bitmap name for the button             */
 HBITMAP hBitmap;		/* Handle to bitmap			  */
 UINT    IdHelpLine;		/* Help to get when pointed               */
 BOOL    IsEnabled;		/* Is the button enabled ?             	  */
} HBUTTON;



typedef struct tagTOOLBAR {
  HINSTANCE	hInstance;		/* The current instance                     */
  INT		BtWidth, BtHeight;      /* Buttons width and height                 */
  CHAR		BtBitmap[MAXLNG],
		PushedBtBitmap[MAXLNG]; /* Bitmap names for button and pushed buttons */
  UINT		nBts;			/* Number of buttons in tool bar       	      */
  HBUTTON	Bts[MAXBTS];		/* Array of buttons 			      */
  INT		ButtonPushed;		/* Button currently pushed or -1	      */
  INT		LastButton;		/* Last button where mouse was on or -1       */
  HWND		hHintWnd;		/* Window to show hint 			      */

} TOOLBAR;

typedef TOOLBAR *HTOOLBAR;


/* Functions defined in WinToolB.c that are exported */
HWND 	       TBCreateWindow	   	(HINSTANCE, HWND, LPCSTR, LPCSTR);
BOOL 	       TBRegisterClass   	(HINSTANCE);
BOOL	       TBAppendButton		(HWND, WPARAM, LPCSTR, UINT, BOOL);

