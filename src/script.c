/*
 * Maintaining a stack of files / scripts.
 * 
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2002, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 *
 */
#include "prelude.h"
#include "storage.h"
#include "machdep.h"
#include "errors.h"
#include "connect.h"
#include "script.h"

/* --------------------------------------------------------------------------
 * Local script state:
 * ------------------------------------------------------------------------*/

/*
 * The scripts that either have been loaded or will be later are all
 * stored in fixed-size stacks. (Moving to growable tables will also 
 * require making the module table expandable.)
 */
static String scriptName[NUM_SCRIPTS];  /* Script file names               */
static String scriptReal[NUM_SCRIPTS];  /* Full path to canonical name     */
static Time   lastChange[NUM_SCRIPTS];  /* Time of last change to script   */
static Bool   postponed[NUM_SCRIPTS];   /* Indicates postponed load        */
static Bool   chased[NUM_SCRIPTS];      /* Added by import chasing?        */

static Int    numScripts;               /* Number of scripts loaded        */
static Int    namesUpto;                /* Number of script names set      */
static Int    scriptsStable;            /* Number of (Prelude) scripts     */
                                        /* considered 'stable'             */
                                        /* (=> won't be nuked when clearing */
                                        /* the script stack / reloading.)   */

static Bool   needsImports;             /* set to TRUE if imports required */
       String scriptFile;               /* Name of current script (if any) */

       String currProject = 0;          /* Name of current project file    */
static Bool   projectLoaded = FALSE;    /* TRUE => project file loaded     */

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/
static Bool local addScript     Args((String,Long));

/* --------------------------------------------------------------------------
 * Initialising / freeing script stacks:
 * ------------------------------------------------------------------------*/
Void initScripts() {
  scriptFile = 0;
  numScripts = 0;
  namesUpto  = 0;
}

Void stopScripts() {
  int i;

  for (i=0; i < numScripts ; i++) {
    if (scriptName[i]) free(scriptName[i]);
    if (scriptReal[i]) free(scriptReal[i]);
  }
}

/* We record the number of scripts that loading the Prelude
 * brought about, so that when the user comes to clear the module
 * stack (e.g., ":l<ENTER>"), only modules later than the Prelude
 * ones are scratched.
 */
Void setScriptStableMark() {
  scriptsStable = namesUpto;
}

String getScriptName(s)  /* access the script name at index 's' */
Script s; {
  if ( s >=0 && s <= numScripts ) {
    return scriptName[s];
  } else {
    ERRMSG(0) "Illegal script index %d (max: %d)", s, numScripts
    EEND;
  }
  return NULL;
}

Int getScriptHwMark() { /* return number of on the stack, loaded or not. */
  return namesUpto;
}

Int numLoadedScripts() { /* return number of currently loaded scripts */
  return numScripts;
}

/* --------------------------------------------------------------------------
 * Loading script files:
 * ------------------------------------------------------------------------*/

Void addScriptName(s,sch)  /* Add script to list of scripts   */
String s;                  /* to be read in ...               */
Bool   sch; {              /* TRUE => requires pathname search*/
    if (namesUpto>=NUM_SCRIPTS) {
	ERRMSG(0) "Too many module files (maximum of %d allowed)",
		  NUM_SCRIPTS
	EEND;
	return;
    }
    scriptName[namesUpto] = strCopy(sch ? findPathname(NULL,s) : s);
    scriptReal[namesUpto] = strCopy(RealPath(scriptName[namesUpto]));
    chased[namesUpto]     = !sch;
    namesUpto++;
}

static Bool local addScript(fname,len)  /* read single script file */
String fname;                           /* name of script file     */
Long   len; {                           /* length of script file   */
    scriptFile = fname;

#if HUGS_FOR_WINDOWS         /* Set clock cursor while loading   */
    allowBreak();
    SetCursor(LoadCursor(NULL, IDC_WAIT));
    AddFileToFileNamesMenu(&FilesMenu, RealPath(fname));
#endif

    if (!quiet) {
	Printf("Reading file \"%s\":\n",fname);  FlushStdout();
    }
    setLastEdit(fname,0);

    needsImports = FALSE;
    if (!parseScript(fname,len)) {   /* process script file */
	/* file or parse error, drop the script */ 
	forgetAScript(numScripts);
	errFail();
    }
    if (needsImports) return FALSE;
    checkDefns();
    typeCheckDefns();
    compileDefns();
    scriptFile    = 0;
    preludeLoaded = TRUE;
    return TRUE;
}

Bool chase(imps)                 /* Process list of import requests */
List imps; {
    if (chaseImports) {
	Int    origPos  = numScripts;   /* keep track of original position */
	String origName = scriptName[origPos];
	for (; nonNull(imps); imps=tl(imps)) {
	    String iname = findPathname(origName,textToStr(textOf(hd(imps))));
	    String rname = RealPath(iname);
	    Int    i     = 0;
	    for (; i<namesUpto; i++)
		if (filenamecmp(scriptReal[i],rname)==0)
		    break;
	    if (i>=origPos) {           /* Neither loaded or queued        */
		String theName;
		String theReal;
		Time   theTime;
		Bool   thePost;
		Bool   theChase;

		postponed[origPos] = TRUE;
		needsImports       = TRUE;

		if (i>=namesUpto)       /* Name not found (i==namesUpto)   */
		    addScriptName(iname,FALSE);
		else if (postponed[i]) {/* Check for recursive dependency  */
		    ERRMSG(0)
		      "Recursive import dependency between \"%s\" and \"%s\"",
		      scriptName[origPos], iname
		    EEND;
		}
		/* Right rotate section of tables between numScripts and i so
		 * that i ends up with other imports in front of orig. script
		 */
		theName = scriptName[i];
		theReal = scriptReal[i];
		thePost = postponed[i];
		theChase = chased[i];
		timeSet(theTime,lastChange[i]);
		for (; i>numScripts; i--) {
		    scriptName[i] = scriptName[i-1];
		    scriptReal[i] = scriptReal[i-1];
		    postponed[i]  = postponed[i-1];
		    chased[i]     = chased[i-1];
		    timeSet(lastChange[i],lastChange[i-1]);
		}
		scriptName[numScripts] = theName;
		scriptReal[numScripts] = theReal;
		postponed[numScripts]  = thePost;
		chased[numScripts]     = theChase;
		timeSet(lastChange[numScripts],theTime);
		origPos++;
	    }
	}
	return needsImports;
    }
    return FALSE;
}

/* --------------------------------------------------------------------------
 * Project file handling:
 * ------------------------------------------------------------------------*/

Void loadProject(s)        /* Load project file               */
String s; {
    clearProject();
    currProject = s;
    projInput(currProject);
    scriptFile = currProject;
    forgetAllScripts();
    while ((s=readFilename())!=0)
	addScriptName(s,TRUE);
    if (namesUpto<=1) {
	ERRMSG(0) "Empty project file"
	EEND;
    }
    scriptFile    = 0;
    projectLoaded = TRUE;
}

Void clearProject() {      /* clear name for current project  */
    if (currProject)
      free(currProject);
    currProject   = 0;
    projectLoaded = FALSE;
#if HUGS_FOR_WINDOWS
    setLastEdit((String)0,0);
#endif
}

/* --------------------------------------------------------------------------
 * Dropping script files:
 * ------------------------------------------------------------------------*/
Void forgetScriptsFrom(scno) /* remove scripts from system     */
Script scno; {
    Script i;
    for (i=scno; i<namesUpto; ++i)
	if (scriptName[i])
	    free(scriptName[i]);
    dropScriptsFrom(scno-1); /* don't count prelude as script  */
    namesUpto = scno;
    if (numScripts>namesUpto)
	numScripts = scno;
}

Void forgetAllScripts() {
  /* Drop all but the stable scripts; i.e., the
   * Prelude and (possibly) its implementation module(s).
   */
  forgetScriptsFrom( scriptsStable ); 
}

Void forgetAScript(scno) /* remove a script from system */
Script scno; {
    Script i;
    
    if (scno > namesUpto)
	return;

    if (scriptName[scno])
	free(scriptName[scno]);
    if (scriptReal[scno])
	free(scriptReal[scno]);

    for (i=scno+1; i < namesUpto; i++) {
	scriptName[i-1] = scriptName[i];
	scriptReal[i-1] = scriptReal[i];
	lastChange[i-1] = lastChange[i];
        postponed[i-1]  = postponed[i];
        chased[i-1]     = chased[i];
    }
    dropAScript(scno);
    namesUpto--;
}

Void readScripts(n)        /* Reread current list of scripts, */
Int n; {                   /* loading everything after and    */
    Time timeStamp;        /* including the first script which*/
    Long fileSize;         /* has been either changed or added*/

#if HUGS_FOR_WINDOWS
    SetCursor(LoadCursor(NULL, IDC_WAIT));
#endif

    for (; n<numScripts; n++) {         /* Scan previously loaded scripts  */
	getFileInfo(scriptName[n], &timeStamp, &fileSize);
	if (timeChanged(timeStamp,lastChange[n])) {
	    dropScriptsFrom(n-1);
	    numScripts = n;
	    break;
	}
    }
    for (; n<NUM_SCRIPTS; n++)          /* No scripts have been postponed  */
	postponed[n] = FALSE;           /* at this stage                   */


    while (numScripts<namesUpto) {      /* Process any remaining scripts   */
	getFileInfo(scriptName[numScripts], &timeStamp, &fileSize);
	timeSet(lastChange[numScripts],timeStamp);
	if (numScripts>0)               /* no new script for prelude       */
	    startNewScript(scriptName[numScripts]);
        generate_ffi = generateFFI && !chased[numScripts];
	if (addScript(scriptName[numScripts],fileSize))
	    numScripts++;
	else
	    dropScriptsFrom(numScripts-1);
    }

    if (listScripts)
	whatScripts();
    if (numScripts<=1)
	setLastEdit((String)0, 0);
}

Void whatScripts() {       /* list scripts in current session */
    int i;
#if HUGS_FOR_WINDOWS
    if (!InAutoReloadFiles) {
#endif
    Printf("\nHugs session for:");
    if (projectLoaded)
	Printf(" (project: %s)",currProject);
    for (i=0; i<numScripts; ++i)
	Printf("\n%s",scriptName[i]);
    Putchar('\n');
#if HUGS_FOR_WINDOWS
    }
#endif
}

Void script(what)
Int what; {

  /* not much to it..will be more when/if the local tables
   * become resizable.
   */
  switch(what) {
  case RESET:   return;
  case MARK:    return;
  case INSTALL: return;
  case EXIT:    stopScripts();
                return;
  case BREAK:   return;
  }
}
  
