/* --------------------------------------------------------------------------
 * Operations on Chars.
 *
 * Extended to Unicode by Dimitry Golubovsky <dimitry@golubovsky.org>.
 *
 * The Hugs 98 system is Copyright (c) Mark P Jones, Alastair Reid, the
 * Yale Haskell Group, and the OGI School of Science & Engineering at OHSU,
 * 1994-2003, All rights reserved.  It is distributed as free software under
 * the license in the file "License", which is included in the distribution.
 * ------------------------------------------------------------------------*/

#include "prelude.h"
#include "storage.h"
#include "connect.h"
#include "char.h"

#if UNICODE_CHARS

/* --------------------------------------------------------------------------
 * Unicode character properties (cf http://www.unicode.org/ucd/)
 * ------------------------------------------------------------------------*/

/* Unicode general categories, listed in the same order as in the Unicode
 * standard -- this must be the same order as in Hugs.Unicode.
 */
enum {
    GENCAT_Lu,	/* Letter, Uppercase */
    GENCAT_Ll,	/* Letter, Lowercase */
    GENCAT_Lt,	/* Letter, Titlecase */
    GENCAT_Lm,	/* Letter, Modifier */
    GENCAT_Lo,	/* Letter, Other */
    GENCAT_Mn,	/* Mark, Non-Spacing */
    GENCAT_Mc,	/* Mark, Spacing Combining */
    GENCAT_Me,	/* Mark, Enclosing */
    GENCAT_Nd,	/* Number, Decimal */
    GENCAT_Nl,	/* Number, Letter */
    GENCAT_No,	/* Number, Other */
    GENCAT_Pc,	/* Punctuation, Connector */
    GENCAT_Pd,	/* Punctuation, Dash */
    GENCAT_Ps,	/* Punctuation, Open */
    GENCAT_Pe,	/* Punctuation, Close */
    GENCAT_Pi,	/* Punctuation, Initial quote */
    GENCAT_Pf,	/* Punctuation, Final quote */
    GENCAT_Po,	/* Punctuation, Other */
    GENCAT_Sm,	/* Symbol, Math */
    GENCAT_Sc,	/* Symbol, Currency */
    GENCAT_Sk,	/* Symbol, Modifier */
    GENCAT_So,	/* Symbol, Other */
    GENCAT_Zs,	/* Separator, Space */
    GENCAT_Zl,	/* Separator, Line */
    GENCAT_Zp,	/* Separator, Paragraph */
    GENCAT_Cc,	/* Other, Control */
    GENCAT_Cf,	/* Other, Format */
    GENCAT_Cs,	/* Other, Surrogate */
    GENCAT_Co,	/* Other, Private Use */
    GENCAT_Cn	/* Other, Not Assigned */
};

/* properties of a Unicode character */
struct CharProperties {
    int category;		/* Unicode general category */
    int upper_offset;		/* offset of the result of toUpper */
    int lower_offset;		/* offset of the result of toUpper */
    int title_offset;		/* offset of the result of toUpper */
};

/* A contiguous block of characters with the same properties */
struct CharBlock {
    Char blk_start;		/* first character in the block            */
    Char blk_length;		/* number of characters in the block       */
    const struct CharProperties *blk_properties;
				/* properties shared by these characters,  */
    				/* and possibly by other blocks too.       */
};

/* property table automatically generated from UnicodeData file */
#include "unitable.c"

/* properties for an character not covered by the table */
static const struct CharProperties null_properties = { GENCAT_Cn, 0, 0, 0 };

const Char max_uni_char = MAX_UNI_CHAR;

#endif

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void local initConsCharTable	Args((Void));
static Void local markConsCharTable	Args((Void));
#if UNICODE_CHARS
static const struct CharProperties * local get_properties Args((Char));
#endif

/* --------------------------------------------------------------------------
 * Character set handling:
 *
 * Hugs follows Haskell 1.3 in assuming that input uses the ISO-8859-1
 * (Latin-1) character set.  The following code provides methods for
 * classifying input characters according to the lexical structure
 * specified by the report.  Hugs should still accept older programs
 * because ASCII is just a subset of the Latin-1 character set.
 * ------------------------------------------------------------------------*/

Bool		charTabBuilt;
unsigned char   charTable[NUM_LAT1_CHARS];

Void initCharTab() {			/* Initialize char decode table    */
#define setRange(x,f,t) {Int i=f;   while (i<=t) charTable[i++] |=x;}
#define setChar(x,c)	charTable[c] |= (x)
#define setChars(x,s)   {char *p=s; while (*p)   charTable[(Int)*p++]|=x;}
#define setCopy(x,c)    {Int i;                         \
			 for (i=0; i<NUM_LAT1_CHARS; ++i)    \
			     if (isIn(i,c))             \
				 charTable[i]|=x;          \
			}

    setRange(DIGIT,     '0','9');	/* ASCII decimal digits		   */

    setRange(SMALL,     'a','z');	/* ASCII lower case letters	   */
    setRange(SMALL,     223,246);	/* Latin-1 lower case letters	   */
    setRange(SMALL,     248,255);	/* (omits division symbol, 247)	   */
    setChar (SMALL,     '_');

    setRange(LARGE,     'A','Z');	/* ASCII upper case letters	   */
    setRange(LARGE,     192,214);	/* Latin-1 upper case letters	   */
    setRange(LARGE,     216,222);	/* (omits multiplication, 215)	   */

    setRange(SYMBOL,    161,191);	/* Symbol characters + ':'	   */
    setRange(SYMBOL,    215,215);
    setChar (SYMBOL,    247);
    setChars(SYMBOL,    ":!#$%&*+./<=>?@\\^|-~");

    setChar (IDAFTER,   '\'');		/* Characters in identifier	   */
    setCopy (IDAFTER,   (DIGIT|SMALL|LARGE));

    setChar (SPACE,     ' ');		/* ASCII space character	   */
    setChar (SPACE,     160);		/* Latin-1 non breaking space	   */
    setRange(SPACE,     9,13);		/* special whitespace: \t\n\v\f\r  */

    setChars(PRINT,     "(),;[]_`{}");  /* Special characters		   */
    setChars(PRINT,     " '\"");        /* Space and quotes		   */
    setCopy (PRINT,     (DIGIT|SMALL|LARGE|SYMBOL));
    
#undef setRange
#undef setChar
#undef setChars
#undef setCopy

    charTabBuilt = TRUE;
}

#if UNICODE_CHARS

#define	UPPER_MASK	((1<<GENCAT_Lu)|(1<<GENCAT_Lt)|(1<<GENCAT_Lm))
#define	ALPHANUM_MASK	(UPPER_MASK|(1<<GENCAT_Ll)|(1<<GENCAT_Nd))

Bool isLower(c)
Char c; {
    return isLatin1(c) ? isLowerLat1(c) :
	    get_properties(c)->category==GENCAT_Ll;
}

Bool isUpper(c)
Char c; {
    return isLatin1(c) ? isUpperLat1(c) :
	    ((1<<get_properties(c)->category)&UPPER_MASK)!=0;
}

Bool isAlphaNum(c)
Char c; {
    return isLatin1(c) ? isAlphaNumLat1(c) :
	    ((1<<get_properties(c)->category)&ALPHANUM_MASK)!=0;
}

Bool isPrint(c)
Char c; {
    return isLatin1(c) ? isAlphaNumLat1(c) :
	    get_properties(c)->category<=GENCAT_Zs;
}

Char toUpper(c)
Char c; {
    return c + get_properties(c)->upper_offset;
}

Char toLower(c)
Char c; {
    return c + get_properties(c)->lower_offset;
}

Char toTitle(c)
Char c; {
    return c + get_properties(c)->title_offset;
}

Int uni_gencat(c)
Char c; {
    return get_properties(c)->category;
}

/* binary search of the properties table */
static const struct CharProperties * local get_properties(c)
Char c; {
    Int lo, hi, mid;
    lo = 0;
    hi = NUM_BLOCKS-1;
    while (lo!=hi) {
	/* i <= lo => char_block[i].blk_start <= c */
	/* i >  hi => char_block[i].blk_start >  c */
	mid = (lo+hi+1)/2;	/* lo < mid <= hi */
	if (char_block[mid].blk_start<=c)
	    lo = mid;
	else
	    hi = mid-1;
    }
    /* i <= lo => char_block[i].blk_start <= c */
    /* i >  lo => char_block[i].blk_start >  c */
    return c<char_block[lo].blk_start+char_block[lo].blk_length ?
		char_block[lo].blk_properties : &null_properties;
}

#else

Char toUpper(c)
Char c; {
    /* two lowercase letters have no Latin-1 uppercase counterpart */
    if (isLower(c) && c!=0xdf && c!=0xff)
	return c - 'a' + 'A';
    return c;
}

Char toLower(c)
Char c; {
    if (isUpper(c))
	return c - 'A' + 'a';
    return c;
}

#endif

/* --------------------------------------------------------------------------
 * Build array of character conses:
 * ------------------------------------------------------------------------*/

static Cell consCharArray[NUM_LAT1_CHARS];

Cell consChar(c)                        /* return application (:) c        */
Char c; {
    assert(c >= 0);
    return consCharArray[c];
}

static void local initConsCharTable() {
    Int i;

    for (i=0; i<NUM_LAT1_CHARS; ++i)
	consCharArray[i] = ap(nameCons,mkChar(i));
}

static void local markConsCharTable() {
    Int i;

    for (i=0; i<NUM_LAT1_CHARS; ++i)
	mark(consCharArray[i]);
}

Void charOps(what)
Int what; {
    switch (what) {
	case INSTALL : initCharTab();
		       initConsCharTable();
		       break;

	case MARK    : markConsCharTable();
		       break;
    }
}
