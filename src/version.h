/* --------------------------------------------------------------------------
 * Version number
 * ------------------------------------------------------------------------*/

/* Is this a major release or not? */
#define MAJOR_RELEASE 0

/* Define this as a string (of exactly 14 characters) uniquely identifying the 
 * current version.
 * Major releases are of the form "<month> <year>"
 * Minor releases are of the form "Version YYMMDD"
 * Anyone else should use a different format to avoid confusion.    
 */
#if MAJOR_RELEASE
#define HUGS_VERSION "December 2000 "
#else
#define HUGS_VERSION "Version 001212"
#endif

