module Hugs.Unicode(
	GeneralCategory(..),
	generalCategory,	-- :: Char -> GeneralCategory

	isLetter,		-- :: Char -> Bool
	isMark,			-- :: Char -> Bool
	isNumber,		-- :: Char -> Bool
	isPunctuation,		-- :: Char -> Bool
	isSymbol,		-- :: Char -> Bool
	isSeparator,		-- :: Char -> Bool

	toUpper,		-- :: Char -> Char
	toLower,		-- :: Char -> Char
	toTitle,		-- :: Char -> Char
	) where

import Data.Char (toUpper, toLower)

-- Unicode character properties
-- Currently just:

-- Unicode General Categories (column 2 of the UnicodeData table)
-- in the order they are listed in the Unicode standard.
-- (The order, though not necessarily the names, must match the
-- constants defined in src/char.c so that toEnum will work.)

data GeneralCategory
	= UppercaseLetter	-- Lu  Letter, Uppercase
	| LowercaseLetter	-- Ll  Letter, Lowercase
	| TitlecaseLetter	-- Lt  Letter, Titlecase
	| ModifierLetter	-- Lm  Letter, Modifier
	| OtherLetter		-- Lo  Letter, Other
	| NonSpacingMark	-- Mn  Mark, Non-Spacing
	| SpacingCombiningMark	-- Mc  Mark, Spacing Combining
	| EnclosingMark		-- Me  Mark, Enclosing
	| DecimalNumber		-- Nd  Number, Decimal
	| LetterNumber		-- Nl  Number, Letter
	| OtherNumber		-- No  Number, Other
	| ConnectorPunctuation	-- Pc  Punctuation, Connector
	| DashPunctuation	-- Pd  Punctuation, Dash
	| OpenPunctuation	-- Ps  Punctuation, Open
	| ClosePunctuation	-- Pe  Punctuation, Close
	| InitialQuote		-- Pi  Punctuation, Initial quote
	| FinalQuote		-- Pf  Punctuation, Final quote
	| OtherPunctuation	-- Po  Punctuation, Other
	| MathSymbol		-- Sm  Symbol, Math
	| CurrencySymbol	-- Sc  Symbol, Currency
	| ModifierSymbol	-- Sk  Symbol, Modifier
	| OtherSymbol		-- So  Symbol, Other
	| Space			-- Zs  Separator, Space
	| LineSeparator		-- Zl  Separator, Line
	| ParagraphSeparator	-- Zp  Separator, Paragraph
	| Control		-- Cc  Other, Control
	| Format		-- Cf  Other, Format
	| Surrogate		-- Cs  Other, Surrogate
	| PrivateUse		-- Co  Other, Private Use
	| NotAssigned		-- Cn  Other, Not Assigned
	deriving (Eq, Ord, Enum, Read, Show, Bounded)

generalCategory :: Char -> GeneralCategory
generalCategory c = toEnum (primUniGenCat c)

primitive primUniGenCat :: Char -> Int

-- derived character classifiers

isLetter :: Char -> Bool
isLetter c = case generalCategory c of
	UppercaseLetter		-> True
	LowercaseLetter		-> True
	TitlecaseLetter		-> True
	ModifierLetter		-> True
	OtherLetter		-> True
	_			-> False

isMark :: Char -> Bool
isMark c = case generalCategory c of
	NonSpacingMark		-> True
	SpacingCombiningMark	-> True
	EnclosingMark		-> True
	_			-> False

isNumber :: Char -> Bool
isNumber c = case generalCategory c of
	DecimalNumber		-> True
	LetterNumber		-> True
	OtherNumber		-> True
	_			-> False

isPunctuation :: Char -> Bool
isPunctuation c = case generalCategory c of
	ConnectorPunctuation	-> True
	DashPunctuation		-> True
	OpenPunctuation		-> True
	ClosePunctuation	-> True
	InitialQuote		-> True
	FinalQuote		-> True
	OtherPunctuation	-> True
	_			-> False

isSymbol :: Char -> Bool
isSymbol c = case generalCategory c of
	MathSymbol		-> True
	CurrencySymbol		-> True
	ModifierSymbol		-> True
	OtherSymbol		-> True
	_			-> False

isSeparator :: Char -> Bool
isSeparator c = case generalCategory c of
	Space			-> True
	LineSeparator		-> True
	ParagraphSeparator	-> True
	_			-> False

primitive toTitle :: Char -> Char
