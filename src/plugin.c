/* --------------------------------------------------------------------------
 * Statically linked plugins
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the Oregon Graduate Institute of Science and
 * Technology, 1994-1999, All rights reserved.  It is distributed as
 * free software under the license in the file "License", which is
 * included in the distribution.
 *
 * $RCSfile: plugin.c,v $
 * $Revision: 1.3 $
 * $Date: 1999/09/13 11:01:04 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"

/* This file is often compiled with a command-line argument such as
 *   '-DPLUGINS1={Xlib,1,&initXlib},'
 * default to empty if not present.
 */
#ifndef PLUGINS1
# define PLUGINS1
#endif
/* If you're using GreenCard2, you need to set PLUGINS2 instead.. */
#ifndef PLUGINS2
# define PLUGINS2
#endif

struct pluginInfo1 {
    String         nm;            /* Name of plugin module                 */
    InitModuleFun1 initModule;    /* Initialisation code for the plugin    */
};

static struct pluginInfo1 pluginList1[] = { /* hardwired list of all plugins */
  /* {"Test",  initTest},  */
  /* {"Xlib",  initXlib},  */
  PLUGINS1
  {0,0}
};

struct pluginInfo2 {
    String         nm;            /* Name of plugin module                 */
    InitModuleFun2 initModule;    /* Initialisation code for the plugin    */
};

static struct pluginInfo2 pluginList2[] = { /* hardwired list of all plugins */
  /* {"Win32", initWin32}, */
  PLUGINS2
  {0,0}
};

Bool havePlugin(mod)                /* did we statically link this plugin? */
String mod; {
    Int i;
    for(i=0; pluginList1[i].nm; ++i) {
	if (0 == strcmp(mod, pluginList1[i].nm)) {
	    return TRUE;
	}
    }
    for(i=0; pluginList2[i].nm; ++i) {
	if (0 == strcmp(mod, pluginList2[i].nm)) {
	    return TRUE;
	}
    }
    return FALSE;
}

/* --------------------------------------------------------------------------
 * Plugins control:
 * ------------------------------------------------------------------------*/

Void plugins(what)
Int what; {
    Int i;
    switch (what) {
	case INSTALL : for (i=0; pluginList1[i].initModule; i++) {
			   (*pluginList1[i].initModule)(hugsAPI1());
		       }
		       for (i=0; pluginList2[i].initModule; i++) {
			   (*pluginList2[i].initModule)(hugsAPI2());
		       }
		       break;
    }
}

/*-------------------------------------------------------------------------*/
