/*
 * String utilities needed throughout the Hugs codebase.
 */ 
#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "errors.h"
#include "strutil.h"

/* --------------------------------------------------------------------------
 * String manipulation routines:
 * ------------------------------------------------------------------------*/

String strCopy(s)         /* make malloced copy of a string   */
String s; {
    if (s && *s) {
	char *t, *r;
	if ((t=(char *)malloc(strlen(s)+1))==0) {
	    ERRMSG(0) "String storage space exhausted"
	    EEND;
	}
	for (r=t; (*r++ = *s++)!=0; ) {
	}
	return t;
    }
    return NULL;
}

/* Given a string containing a possibly qualified name,
 * split it up into a module and a name portion.
 */
Void splitQualString(nm, pMod, pName) 
String nm;
String* pMod;
String* pName; {
  String dot;

  /* Find the last occurrence of '.' */
  dot = strrchr(nm, '.');
  
  if (!dot) {
    *pMod = NULL;
    *pName = nm;
  } else {
    unsigned int nmLen;
    unsigned int modLen;
    unsigned int dotLen;

    nmLen  = strlen(nm);
    dotLen = strlen(dot);
    modLen = (unsigned int)(nmLen - dotLen);

    *pMod  = (String) malloc(sizeof(Char) * (modLen + 1));
    *pName = (String) malloc(sizeof(Char) * (dotLen + 1));

    /* The module portion consists of everything upto the last dot. */
    strncpy(*pMod, nm, modLen);
    (*pMod)[modLen] = '\0';
    
    /* Copy everything after the last '.' to the name string */
    strcpy(*pName, dot+1);
  }

}
