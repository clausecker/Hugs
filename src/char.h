#ifndef CHAR_H
#define CHAR_H

/* --------------------------------------------------------------------------
 * Character set handling:
 *
 * Hugs follows Haskell 1.3 in assuming that input uses the ISO-8859-1
 * (Latin-1) character set.  The following code provides methods for
 * classifying input characters according to the lexical structure
 * specified by the report.  Hugs should still accept older programs
 * because ASCII is just a subset of the Latin-1 character set.
 *
 * Extended to Unicode by Dimitry Golubovsky <dimitry@golubovsky.org>.
 * ------------------------------------------------------------------------*/

extern	Bool		charTabBuilt;
extern  unsigned char   charTable[];

#if UNICODE_CHARS
extern const Char	max_uni_char;
#define	MAXCHARVAL	max_uni_char
#else
#define	MAXCHARVAL	(NUM_LAT1_CHARS-1)
#endif

#define isIn(c,x)       (charTable[(unsigned char)(c)]&(x))
#define isLatin1(c)     (0<=(c) && (c)<NUM_LAT1_CHARS)

/* character classes for lexical analysis */
#define DIGIT           0x01
#define SMALL           0x02
#define LARGE           0x04
#define SYMBOL          0x08
#define IDAFTER         0x10
#define SPACE           0x20
#define PRINT           0x40

/* predicates for the Char module */

#define	isLowerLat1(c)	(isIn((c),SMALL) && (c)!='_')
#define	isUpperLat1(c)	isIn((c),LARGE)
#define	isAlphaNumLat1(c) (isIn((c),IDAFTER) && (c)!='_' && (c)!='\'')
#define	isPrintLat1(c)	isIn((c),PRINT)

#if UNICODE_CHARS

extern	Bool	isLower		Args((Char));
extern	Bool	isUpper		Args((Char));
extern	Bool	isAlphaNum	Args((Char));
extern	Bool	isPrint 	Args((Char));

#else

#define	isLower(c)	isLowerLat1(c)
#define	isUpper(c)	isUpperLat1(c)
#define	isAlphaNum(c)	isAlphaNumLat1(c)
#define	isPrint(c)	isPrintLat1(c)

#endif

extern	Void	initCharTab	Args((void));
extern	Char	toUpper		Args((Char));
extern	Char	toLower		Args((Char));
#if UNICODE_CHARS
extern	Char	toTitle		Args((Char));
extern	Int	uni_gencat	Args((Char));
#endif

#endif /* CHAR_H */
