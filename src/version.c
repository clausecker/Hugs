/* --------------------------------------------------------------------------
 * Version string
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2002, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 * ------------------------------------------------------------------------*/

/* Is this a major release or not? */
#define MAJOR_RELEASE 1

/* Define this as a string (of _max_ 14 characters) uniquely identifying the 
 * current version. Upper limit of 14 chars is there to make the banner
 * come out nice and aligned.
 *
 * Major releases are of the form "<month> <year>"
 * Minor releases are of the form "Version YYYYMMDD"
 * Anyone else should use a different format to avoid confusion.    
 *
 * On standard unix platforms, the Makefile will automatically fill in
 * the date for both major and minor releases.  Only modify this file
 * if you need to override the automatically generated date.
 *
 * However, if your system doesn't have GNU date, then the version strings
 * will be empty, and you'll want to manually specify the version strings.
 */

#include "prelude.h"

String versionString =
#ifndef MONTH_YEAR
    "<snapshot>"
#else
#if MAJOR_RELEASE
    MONTH_YEAR
#else
    YYYYMMDD
#endif
#endif
    ;
