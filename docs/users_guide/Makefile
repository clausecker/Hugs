ROOT	= users_guide
PARTS	= $(ROOT).sgml commands.sgml extensions.sgml haskell98.sgml \
	  install.sgml introduction.sgml license.sgml running.sgml

HTML	= $(ROOT)/index.html

all: html dvi ps pdf

html: $(HTML)

dvi: $(ROOT).dvi

ps: $(ROOT).ps

pdf: $(ROOT).pdf

$(HTML): $(PARTS)
	docbook2html $(ROOT).sgml

$(ROOT).dvi: $(PARTS)
	docbook2dvi $(ROOT).sgml

$(ROOT).ps: $(PARTS)
	docbook2ps $(ROOT).sgml

$(ROOT).pdf: $(PARTS)
	docbook2pdf $(ROOT).sgml
