
#include "Header.h"
#include "resrc1.h"
#include "Winmenu.h"

const int BmpWidth = 111;
const int BmpHeight = 112;
const int BmpTransparent = RGB(253,5,255);

typedef struct _AboutData
{
    HIMAGELIST hImgList;
} AboutData;

LRESULT CALLBACK AboutDlgProc(HWND hDlg, UINT msg, WPARAM wParam, LPARAM lParam);

void ShowAboutDialog(HWND hParent)
{
    DialogBox(hThisInstance, MAKEINTRESOURCE(ABOUTDLGBOX), hParent, AboutDlgProc);
}

LRESULT CALLBACK AboutDlgProc(HWND hDlg, UINT Msg, WPARAM wParam, LPARAM lParam)
{
    switch (Msg) {
	case WM_INITDIALOG:
	    {
		AboutData* ad = malloc(sizeof(AboutData));
		HBITMAP hBmp;
		SetWindowLongPtr(hDlg, GWL_USERDATA, (LONG) ad);

	    CenterDialogInParent(hDlg);

	    ad->hImgList = ImageList_Create(BmpWidth, BmpHeight, ILC_COLOR24 | ILC_MASK, 1, 1);
	    hBmp = LoadBitmap(hThisInstance, MAKEINTRESOURCE(BMP_ABOUT));
	    ImageList_AddMasked(ad->hImgList, hBmp, BmpTransparent);
	    DeleteObject(hBmp);
	}
	return TRUE;

	case WM_PAINT:
	    {
		PAINTSTRUCT ps;
		AboutData* ad = (AboutData*) GetWindowLongPtr(hDlg, GWL_USERDATA);

		BeginPaint(hDlg, &ps);
		ImageList_Draw(ad->hImgList, 0, ps.hdc, 20, 25, ILD_TRANSPARENT);
		EndPaint(hDlg, &ps);
	    }
	    break;

	case WM_COMMAND:
	    switch (LOWORD(wParam)) {
		case IDOK: case IDCANCEL:
		    EndDialog(hDlg, TRUE);
		    return TRUE;
	    }
	    break;

	case WM_DESTROY:
	    {
		AboutData* ad = (AboutData*) GetWindowLongPtr(hDlg, GWL_USERDATA);
		ImageList_Destroy(ad->hImgList);
		free(ad);
	    }
    }
    return FALSE;
}
