/* --------------------------------------------------------------------------
 * Operations on Chars.
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

/* --------------------------------------------------------------------------
 * Local function prototypes:
 * ------------------------------------------------------------------------*/

static Void local initConsCharTable	Args((Void));
static Void local markConsCharTable	Args((Void));

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
