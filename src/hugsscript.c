/*
 * DllMain() for Hugsscript.dll
 */
#include <windows.h>

extern void setHugsModule(HMODULE);

BOOL
WINAPI
DllMain(
  HINSTANCE hinstDLL,  /* handle to the DLL module    */
  DWORD fdwReason,     /* reason for calling function */
  LPVOID lpvReserved   /* reserved                    */
)
{
    switch(fdwReason) {
    case DLL_PROCESS_ATTACH:
	/* Stash away the HMODULE for later use. */
	setHugsModule(hinstDLL);
	break;
    }
    return TRUE;
}

