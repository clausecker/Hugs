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
#include "errors.h"
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
static Void local freeConsCharTable	Args((Void));
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
	    get_properties(c)->category<=GENCAT_No;
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
 * a tabulation of (:) c for each character c.
 * ------------------------------------------------------------------------*/

#ifdef UNICODE_CHARS

/* --------------------------------------------------------------------------
 * To save space, we maintain a sparse array of character conses,
 * allocated and filled lazily.  The sparseness is achieved by making the
 * table a 3-level array.  The top level array is statically allocated,
 * as are the first array are each of the other levels.  The rest will
 * be allocated on demand.
 * ------------------------------------------------------------------------*/

#define	AllocArray(ty,n)	((ty *)malloc((n)*sizeof(ty)))

#define NUM_LEVEL3 256		/* size of subarrays at the bottom level */
#define NUM_LEVEL2 256		/* size of subarrays at the middle level */
#define NUM_LEVEL1 (MAX_UNI_CHAR / (NUM_LEVEL3 * NUM_LEVEL2) + 1)

/* To speed up a common case (i.e. ASCII), a small initial subset of
 * chars have pre-built conses, so that they can be accessed directly,
 * bypassing the 3-level array.
 */

/* Number of char conses to pre-build: must be <= NUM_LEVEL3 */
#define	NUM_PRE_BUILT 128

/* indices at each level */
#define IDX1(c) ((c) / (NUM_LEVEL3 * NUM_LEVEL2))
#define IDX2(c) ((c) / NUM_LEVEL3 % NUM_LEVEL2)
#define IDX3(c) ((c) % NUM_LEVEL3 )

/* statically allocated parts of the table */
static Cell **consCharTable[NUM_LEVEL1];
static Cell *consCharTable_0[NUM_LEVEL2];
static Cell consCharTable_0_0[NUM_LEVEL3];

static void local initConsCharTable() {
    Char c;

    consCharTable[0] = consCharTable_0;
    consCharTable_0[0] = consCharTable_0_0;
    for (c=0; c<NUM_PRE_BUILT; c++)
	consCharTable_0_0[c] = ap(nameCons,mkChar(c));
}

/* --------------------------------------------------------------------------
 * Get a character cons: follow the three-level lookup table.
 * If a level does not exist, allocate it.
 * ------------------------------------------------------------------------*/
Cell consChar(c)                /* return application (:) c */
Char c; {
    Int i, j, k;
    Cell **consCharTable_i;
    Cell *consCharTable_i_j;

    if (c<NUM_PRE_BUILT)	/* short cut for a common case */
	return consCharTable_0_0[c];

    i = IDX1(c);
    consCharTable_i = consCharTable[i];
    if (consCharTable_i==NULL) {	/* level 2 entry */
	consCharTable_i = consCharTable[i] = AllocArray(Cell *, NUM_LEVEL2);
	if (consCharTable_i==NULL) {
	    ERRMSG(0) "Cannot allocate char cons table"
	    EEND;
	}
	for (j=0; j<NUM_LEVEL2; j++)
	    consCharTable_i[j] = NULL;
    }

    j = IDX2(c);
    consCharTable_i_j = consCharTable_i[j];
    if (consCharTable_i_j==NULL) {	/* level 3 entry */
	consCharTable_i_j = consCharTable_i[j] = AllocArray(Cell, NUM_LEVEL3);
	if (consCharTable_i_j==NULL) {
	    ERRMSG(0) "Cannot allocate char cons table"
	    EEND;
	}
	for (k=0; k<NUM_LEVEL3; k++)
	    consCharTable_i_j[k] = NIL;
    }

    k = IDX3(c);
    if (isNull(consCharTable_i_j[k]))
	consCharTable_i_j[k] = ap(nameCons,mkChar(c));
    return consCharTable_i_j[k];
}

static void local markConsCharTable() {
    int i, j, k;

    for (i=0; i<NUM_LEVEL1; i++)
	if (consCharTable[i]!=NULL)
	    for (j=0; j<NUM_LEVEL2; j++)
		if (consCharTable[i][j]!=NULL)
		    for (k=0; k<NUM_LEVEL3; k++)
			mark(consCharTable[i][j][k]);
}

static void local freeConsCharTable() {
    int i, j;

    for (j=1; j<NUM_LEVEL2; j++)
	if (consCharTable[0][j]!=NULL)
	    free(consCharTable[0][j]);
    for (i=1; i<NUM_LEVEL1; i++)
	if (consCharTable[i]!=NULL) {
	    for (j=0; j<NUM_LEVEL2; j++)
		if (consCharTable[i][j]!=NULL)
		    free(consCharTable[i][j]);
	    free(consCharTable[i]);
	}
}

#else /* !UNICODE_CHARS */

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

#endif /* !UNICODE_CHARS */

Void charOps(what)
Int what; {
    switch (what) {
	case INSTALL : initCharTab();
		       initConsCharTable();
		       break;

	case MARK    : markConsCharTable();
		       break;

#if UNICODE_CHARS
	case EXIT    : freeConsCharTable();
		       break;
#endif
    }
}
