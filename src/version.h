/* --------------------------------------------------------------------------
 * Version number
 * ------------------------------------------------------------------------*/

/* Is this a major release or not? */
#define MAJOR_RELEASE 0

/* Define this as a string (of _max_ 14 characters) uniquely identifying the 
 * current version. Upper limit of 14 chars is there to make the banner
 * come out nice and aligned.
 *
 * Major releases are of the form "<month> <year>"
 * Minor releases are of the form "Version YYYYMMDD"
 * Anyone else should use a different format to avoid confusion.    
 */
#if MAJOR_RELEASE
#define HUGS_VERSION "MONTH_YEAR"
#else
#define HUGS_VERSION "Version YYYYMMDD"
#endif

