/* --------------------------------------------------------------------------
 * Statically linked plugins
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2002, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 * $RCSfile: plugin.c,v $
 * $Revision: 1.5 $
 * $Date: 2002/06/14 14:41:10 $
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"

/* This file is often compiled with a command-line argument such as
 *   '-DPLUGINS={Xlib,1,&initXlib},'
 * default to empty if not present.
 */
#ifndef PLUGINS
# define PLUGINS
#endif

struct pluginInfo {
    String         nm;            /* Name of plugin module                 */
    InitModuleFun4 initModule;    /* Initialisation code for the plugin    */
};

static struct pluginInfo pluginList[] = { /* hardwired list of all plugins */
  /* {"Test",  initTest},  */
  /* {"Xlib",  initXlib},  */
  PLUGINS
  {0,0}
};

Bool havePlugin(mod)                /* did we statically link this plugin? */
String mod; {
    Int i;
    for(i=0; pluginList[i].nm; ++i) {
	if (0 == strcmp(mod, pluginList[i].nm)) {
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
	case INSTALL : for (i=0; pluginList[i].initModule; i++) {
			   (*pluginList[i].initModule)(hugsAPI4());
		       }
		       break;
    }
}

/*-------------------------------------------------------------------------*/
