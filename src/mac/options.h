/* --------------------------------------------------------------------------
 * Configuration options for Mac/Symantec C (based on src/options.h.in)
 *
 * Hugs 98 is Copyright (c) Mark P Jones, Alastair Reid and the Yale
 * Haskell Group 1994-99, and is distributed as Open Source software
 * under the Artistic License; see the file "Artistic" that is included
 * in the distribution for details.
 *
 * $RCSfile: options.h,v $
 * $Revision: 1.1 $
 * $Date: 1999/06/07 23:53:38 $
 * ------------------------------------------------------------------------*/

/* --------------------------------------------------------------------------
 * Hugs paths and directories
 * ------------------------------------------------------------------------*/

/* Define this as the default setting of HUGSPATH.                        
 * Value may contain string "{Hugs}" (for which we will substitute the
 * value of HUGSDIR) and should be either colon-separated (Unix)
 * or semicolon-separated (Macintosh, Windows, DOS).  Escape
 * characters in the path string are interpreted according to normal
 * Haskell conventions.
 *
 * This value can be overridden from the command line by setting the
 * HUGSFLAGS environment variable or by storing an appropriate value
 * for HUGSFLAGS in the registry (Win32 only).  In all cases, use a 
 * string of the form -P"...".  
 */
#define HUGSPATH "{Hugs}:lib;{Hugs}:lib:hugs;{Hugs}:lib:exts"

/* The directory name which is substituted for the string "{Hugs}"
 * in a path variable.  This normally points to where the Hugs libraries
 * are installed - ie so that the file HUGSDIR/lib/Prelude.hs exists    
 * Typical values are:                                  
 *    "/usr/local/lib/hugs"                             
 *    "/usr/homes/JFHaskell/hugs"                       
 *    ".."      
 *
 * This value is ignored on Windows and Macintosh versions since
 * it is assumed that the binary is installed in HUGSDIR.
 *
 * This value cannot be overridden from the command line or by using 
 * environment variables.  This isn't quite as limitting as you'd think
 * since you can always choose _not_ to use the {Hugs} variable - however,
 * it's obviously _nicer_ to have it set correctly.
 */
#ifndef HUGSDIR
#define HUGSDIR ""
#endif

/* --------------------------------------------------------------------------
 * User interface options
 * ------------------------------------------------------------------------*/

/* Define if you want to use the "Hugs for Windows" GUI.
 * (Windows 3.1 and compatibles only)
 */
#define HUGS_FOR_WINDOWS 0

/* Define if you want filenames to be converted to normal form by:
 * o replacing relative pathnames with absolute pathnames and
 *   eliminating .. and . where possible.
 * o converting to lower case (only in case-insensitive filesystems)
 */
#define PATH_CANONICALIZATION 0

/* Define if a command line editor is available and should be used. 
 * There are two choices of command line editor that can be used with Hugs:
 * GNU readline and editline (from comp.sources.misc, vol 31, issue 71)
 */
#define USE_READLINE 0

/* Define if you want the small startup banner.
 */
#define SMALL_BANNER 0

/* --------------------------------------------------------------------------
 * Making Hugs smaller
 * ------------------------------------------------------------------------*/

/* Define one of these to select overall size of Hugs
 *   SMALL_HUGS     for 16 bit operation on a limited memory PC.
 *   REGULAR_HUGS   for 32 bit operation using largish default table sizes.
 *   LARGE_HUGS     for 32 bit operation using larger default table sizes.
 */
#define SMALL_HUGS   0
#define REGULAR_HUGS 0
#define LARGE_HUGS   1

/* Define if you want to turn off the module system to reduce memory usage.
 * Warning: some of the standard libraries and demo programs require that
 * the module system be enabled.
 */
#define IGNORE_MODULES 0

/* --------------------------------------------------------------------------
 * Fancy features
 * ------------------------------------------------------------------------*/

/* Define if T-REX; Typed Rows and EXtension should be enabled             */
#define TREX 1

/* Define if implicit parameters should be enabled			   */
#define IPARAM 1

/* Define if heap profiling should be used                                 */
#define PROFILING 0

/* Define if you want to use double precision floating point arithmetic    */
#define USE_DOUBLE_PRECISION 0

/* Define if you want to time every evaluation. 
 *
 * Timing is included in the Hugs distribution for the purpose of benchmarking
 * the Hugs interpreter, comparing its performance across a variety of
 * different machines, and with other systems for similar languages.
 *
 * It would be somewhat foolish to try to use the timings produced in this
 * way for any other purpose.  In particular, using timings to compare the
 * performance of different versions of an algorithm is likely to give very
 * misleading results.  The current implementation of Hugs as an interpreter,
 * without any significant optimizations, means that there are much more
 * significant overheads than can be accounted for by small variations in
 * Hugs code.
 */
#undef WANT_TIMER


/* --------------------------------------------------------------------------
 * Debugging options (intended for use by maintainers)
 * ------------------------------------------------------------------------*/

/* Define if debugging generated bytecodes or the bytecode interpreter     */
#define DEBUG_CODE 0

/* Define if debugging generated supercombinator definitions or compiler   */
#define DEBUG_SHOWSC 0

/* Define if you want to use a low-level printer from within a debugger    */
#define DEBUG_PRINTER 0

/* Define if you want to perform runtime tag-checks as an internal 
 * consistency check.  This makes Hugs run very slowly - but is very
 * effective at detecting and locating subtle bugs.
 */
#define CHECK_TAGS 0


/* --------------------------------------------------------------------------
 * Experimental features
 * These are likely to disappear/change in future versions and should not
 * be used by most people..
 * ------------------------------------------------------------------------*/

/* Define if you want to use the primitives which let you examine Hugs 
 * internals.
 */
#define INTERNAL_PRIMS 0

/* Define if you want to use the primitives which let you examine Hugs
 * bytecodes (requires INTERNAL_PRIMS).
 */
#define BYTECODE_PRIMS 0

/* ----------------------------------------------------------------------- */
