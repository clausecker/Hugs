/* --------------------------------------------------------------------------
 * Version number
 * ------------------------------------------------------------------------*/

/* Is this a major release or not? */
#define MAJOR_RELEASE 1

/* Define this as a string (of _max_ 14 characters) uniquely identifying the 
 * current version. Upper limit of 14 chars is there to make the banner
 * come out nice and aligned.
 *
 * Major releases are of the form "<month> <year>"
 * Minor releases are of the form "Version YYMMDD"
 * Anyone else should use a different format to avoid confusion.    
 */
#if MAJOR_RELEASE
#define HUGS_VERSION "February 2001"
#else
#define HUGS_VERSION "Version 021402"
#endif

