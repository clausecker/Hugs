/* --------------------------------------------------------------------------
 * launch.c:    Copyright (c) Hans Aberg 1996.  All rights reserved.
 *
 * Function call macsystem(), replacing the C standard library system()
 * function call, for use on Macintoshes.
 * Compiles with Symantec C++ version 8.
 * ------------------------------------------------------------------------*/



#include <stdio.h>
#include <string.h>
#include <errno.h>

#include <Processes.h>
#include <OSUtils.h>
#include <Resources.h>
#include <Memory.h>
#include <SegLoad.h>
#ifndef __TRAPS__
#include <Traps.h>
#endif
#include <pascal.h>

static char* i_parse;
static char buf[256];

/*	gettok
	
	The gettok function parses token delimited by whitespace, or
	delimited by quotes ("), between ther may occur whitespaces.
	  The useage is the same as the strtok() library function,
	that is, if the argument is not NULL, the parsing is initiated;
	otherwise, the old saved string value is used.
*/
static
char* gettok(char* string)
{
	char quote = '\"';
	char* j = buf;

	if (string != NULL)
		i_parse = string;

	while (*i_parse && isspace(*i_parse))
		++i_parse;

	if (!*i_parse)
		return NULL;

	if (*i_parse == quote) {
		for (++i_parse; *i_parse && *i_parse != quote; *j++ = *i_parse++)
			;
		if (*i_parse != quote)
			return NULL;
		++i_parse;
		*j = '\0';
		return buf;
	}

	while (*i_parse && !isspace(*i_parse))
		*j++ = *i_parse++;
	*j = '\0';
	return buf;
}

int macsystem(char *filenames)	 // launches names separated by spaces
{
#if 0
	/* 	Parse token delimited by whitespaces  */
	char* delims = " \f\n\r\t\v";
	char* filename;

	int k = 0;

	for (filename = strtok(filenames, delims);
		 filename != NULL;
		 filename = strtok(NULL, delims))
		k += (launch(filename) != 0);

	errno = k;
	
	return k;
#elif 0
	/* 	Parse token delimited by whitespaces, or by quotes (inbetween
		which there may occur whitespaces. */
	char* filename;

	int k = 0;

	for (filename = gettok(filenames);
		 filename != NULL;
		 filename = gettok(NULL))
		k += (launch(filename) != 0);

	errno = k;
	
	return k;
#else
	char buf[256];
	char *i, *j, *lw;
	
	/* 	Strip initial whitespaces  */
	for (i = filenames; *i && isspace(*i); ++i)
		;
	
	/*	Copy string to buf, and record last non-white  */
	for (j = buf; *i; *j++ = *i++)
		if (!isspace(*i))
			lw = j;

	*++lw = '\0';	/* Terminate string after last non-white */

	return (launch(buf) != 0);
#endif
}


OSErr OpenSelection(FSSpecPtr theDoc);


int launch(char *filename)	 	// Launches program, file, alias
{
	unsigned char pname[FILENAME_MAX];

	FSSpec fsp;
	OSErr oserr;
	LaunchParamBlockRec lpb;
	
	strcpy((char*)pname, filename);
	C2PStr((char*)pname);

	oserr = FSMakeFSSpec( 0,0,pname,&fsp);
	if (oserr) {
		errno = oserr;
		return oserr;
	}
	
	return OpenSelection(&fsp);
}


// Assumes inclusion of <MacHeaders>
#include <AppleEvents.h>
#include <Processes.h>
#include <Aliases.h>

// Constants for dealing with FinderEvents. See Chapter 8 of the Apple Event
// Registry for more information.
#define kFinderSig	'FNDR'
#define kAEFinderEvents	'FNDR'
#define kSystemType	'MACS'

#define kAEOpenSelection	'sope'
#define keySelection		'fsel'

OSErr FindAProcess(OSType, OSType, ProcessSerialNumber*);

// Given a FSSpecPtr to either an application or a document, OpenSelection creates a
// finder Open Selection Apple event for the object described by the FSSpec.
OSErr OpenSelection(FSSpecPtr theDoc)
{
	AppleEvent	aeEvent;	// the event to create;
	AEDesc	myAddressDesc;	// descriptors for the 
	AEDesc	aeDirDesc;
	AEDesc	listElem;
	AEDesc	fileList;			// our list
	FSSpec	dirSpec;
	AliasHandle	dirAlias;				// alias to directory with our file
	AliasHandle	fileAlias;				// alias of the file itself
	ProcessSerialNumber process;		// the finder's psn
	OSErr			myErr;					// duh

	// Get the psn of the Finder and create the target address for the .
	if(FindAProcess(kFinderSig,kSystemType,&process))
			return procNotFound;
	myErr = AECreateDesc(typeProcessSerialNumber,(Ptr) &process,
							sizeof(process), &myAddressDesc);
	if(myErr)	return myErr;

	// Create an empty 
	myErr = AECreateAppleEvent (kAEFinderEvents, kAEOpenSelection,
					&myAddressDesc, kAutoGenerateReturnID, kAnyTransactionID, 
					&aeEvent);
	if(myErr)
			return myErr;

	// Make an FSSpec and alias for the parent folder, and an alias for the file
	FSMakeFSSpec(theDoc->vRefNum,theDoc->parID,nil,&dirSpec);
	NewAlias(nil,&dirSpec,&dirAlias);
	NewAlias(nil,theDoc,&fileAlias);

	// Create the file list.
	if((myErr=AECreateList(nil,0,false,&fileList)) != noErr)
			return myErr;

	/* Create the folder descriptor
	*/
	HLock((Handle)dirAlias);
	AECreateDesc(typeAlias, (Ptr) *dirAlias, GetHandleSize
					((Handle) dirAlias), &aeDirDesc);
	HUnlock((Handle)dirAlias);
	DisposHandle((Handle)dirAlias);

	if((myErr = AEPutParamDesc(&aeEvent,keyDirectObject,&aeDirDesc)) ==
			noErr)
	{
			AEDisposeDesc(&aeDirDesc);
			HLock((Handle)fileAlias);

			AECreateDesc(typeAlias, (Ptr)*fileAlias,
					GetHandleSize((Handle)fileAlias), &listElem);
			HUnlock((Handle)fileAlias);
			DisposHandle((Handle)fileAlias);
			myErr = AEPutDesc(&fileList,0,&listElem);
	}
	if(myErr)
			return myErr;
	AEDisposeDesc(&listElem);

	if((myErr = AEPutParamDesc(&aeEvent,keySelection,&fileList)) != noErr)
			return myErr;

	myErr = AEDisposeDesc(&fileList);

	myErr = AESend(&aeEvent, nil,
			kAENoReply+kAEAlwaysInteract+kAECanSwitchLayer,
			kAENormalPriority, kAEDefaultTimeout, nil, nil);
	AEDisposeDesc(&aeEvent);
	
	return myErr;
}


// Search through the current process list to find the given application. See
// Using the Process Manager for a similar way of doing this.
OSErr FindAProcess(OSType typeToFind, OSType creatorToFind,
			ProcessSerialNumberPtr processSN)
{
	ProcessInfoRec	tempInfo;
	FSSpec	procSpec;
	Str31			processName;
	OSErr			myErr = noErr;

	// start at the beginning of the process list
	processSN->lowLongOfPSN = kNoProcess;
	processSN->highLongOfPSN = kNoProcess;

	// initialize the process information record
	tempInfo.processInfoLength = sizeof(ProcessInfoRec);
	tempInfo.processName = (StringPtr)&processName;
	tempInfo.processAppSpec = &procSpec;

	while((tempInfo.processSignature != creatorToFind ||
			tempInfo.processType != typeToFind) ||
			myErr != noErr)
	{
			myErr = GetNextProcess(processSN);
			if (myErr == noErr)
					GetProcessInformation(processSN, &tempInfo);
	}
	return(myErr);
}
