/* --------------------------------------------------------------------------
 * Version number
 * ------------------------------------------------------------------------*/

/* Is this a major release or not? */
#define MAJOR_RELEASE 1

/* Define this as a string (of exactly 13 characters) uniquely identifying the 
 * current version.
 * Major releases from Nottingham/Yale are of the form "<month> <year>"
 * Minor releases from Nottingham/Yale are of the form "Version YYMMDD"
 * Anyone else should use a different format to avoid confusion.    
 */
#if MAJOR_RELEASE
#define HUGS_VERSION "May 1999     "
#else
#define HUGS_VERSION "990525 Beta  "
#endif

