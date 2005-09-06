#include "Header.h"
#include <stdio.h>
#include "Winhugs.h"

// stdstr output definitions
#define MAX_STDSTR 1024
int     StrInx = 0;
FILE   *stdstr = NULL;
char    stdstrbuff[MAX_STDSTR];

void WinHugsPutS(FILE* f, char* Buffer)
{
    if (f == stderr) {
	int LastColor = WinHugsColor(RED);
	RtfWindowPutS(Buffer);
	WinHugsColor(LastColor);
    } else if (f == stdout) {
	RtfWindowPutS(Buffer);
    } else if (f == stdstr) {
	int i;
	for (i = 0; Buffer[i]; i++) {
	    if (Buffer[i] == '\n') {
		stdstrbuff[StrInx] = 0;
		StrInx = 0;
	    }
	    else
		stdstrbuff[StrInx++] = Buffer[i];
	}
    } else {
	fputs(Buffer, f);
    }
}

int WinHugsAnyPrintf(FILE* f, const char* format, va_list* args)
{
    char Buffer[2048];
    int Count = vsprintf(Buffer, format, *args);
    WinHugsPutS(f, Buffer);
    return Count;
}

int WinHugsPrintf(const char* format, ...)
{
    va_list args;
    int Count;
    va_start(args, format);
    Count = WinHugsAnyPrintf(stdout, format, &args);
    va_end(args);
    return Count;
}

int WinHugsFPrintf(FILE* f, const char* format, ...)
{
    va_list args;
    int Count;
    va_start(args, format);
    Count = WinHugsAnyPrintf(f, format, &args);
    va_end(args);
    return Count;
}

int WinHugsPutC(FILE* f, char c)
{
    char Buf[2];
    Buf[0] = c;
    Buf[1] = 0;
    WinHugsPutS(f, Buf);
    return c;
}

int WinHugsGetC(FILE* f)
{
    if (f == stdin)
	return 0; // no support for interact
    else
	return fgetc(f);
}
