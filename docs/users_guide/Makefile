ROOT	= users_guide
PARTS	= $(ROOT).sgml commands.sgml haskell98.sgml hugs_ghc.sgml \
	  hugs_only.sgml introduction.sgml license.sgml running.sgml
HTML_OPTS = -V '%use-id-as-filename%'

HTML	= $(ROOT)/index.html

all: html dvi ps pdf

html: $(HTML)

dvi: $(ROOT).dvi

ps: $(ROOT).ps

pdf: $(ROOT).pdf

$(HTML): $(PARTS)
	rm -rf $(ROOT)
	docbook2html -o $(ROOT) $(HTML_OPTS) $(ROOT).sgml

$(ROOT).dvi: $(PARTS)
	docbook2dvi $(ROOT).sgml

$(ROOT).ps: $(PARTS)
	docbook2ps $(ROOT).sgml

$(ROOT).pdf: $(PARTS)
	docbook2pdf $(ROOT).sgml
