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

/* Possibly shorter version of Char for use in arrays. */
#if UNICODE_CHARS
typedef	Char ShortChar;
#else
typedef	unsigned char ShortChar;
#endif

/* --------------------------------------------------------------------------
 * Character classification and other primitives.
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

/* --------------------------------------------------------------------------
 * Encoding of Chars as sequences of bytes.
 *
 * The byte encoding is assumed to be an extension of ASCII.  These macros
 * should be used unless you're sure you're dealing with ASCII.
 *
 * In an ideal world, we would use Unicode characters uniformly inside
 * the program.  However, to minimize changes to the rest of the program,
 * and to save space, we also encode strings in the text table with the
 * same encoding as we use for I/O.
 *
 * The interface is:
 *
 *	int MAX_CHAR_ENCODING
 *		Maximum number of bytes to encode a Char.
 *	Char FPutChar(Char c, FILE *f)
 *		Output the encoding of the c, returning c if successful,
 *		EOF if not.
 *	Char FGetChar(FILE *f)
 *		Read and decode a Char, returning the Char if successful,
 *		or EOF if end-of-file or a coding error is encountered.
 *	void AddChar(Char c, String &sp)
 *		Add the encoding of c to the string, moving the pointer.
 *		At least MAX_CHAR_ENCODING bytes should be available.
 *	Char ExtractChar(String &sp)
 *		Decode a Char from the string, moving the pointer.
 *		Returns '\0' on end of string or coding error.
 *	Bool charIsRepresentable(Char c)
 *		Tests whether s can be represented without loss using
 *		the selected encoding.
 * ------------------------------------------------------------------------*/

#define MAX_CHAR_ENCODING 1

#define	FPutChar(c,f)	(fputc(c, f))
#define	FGetChar(f)	(getc(f))
#define	AddChar(c,s)	(*(s)++ = (c))
#define	ExtractChar(s)	(*(unsigned char *)(s)++)

#define	charIsRepresentable(c)	isLatin1(c)

#endif /* CHAR_H */
