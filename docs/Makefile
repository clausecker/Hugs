# Makefile for Documentation
# Note that some documentation is in other directories.

all		:: server.ps

server.ps	: server.dvi
		  dvips server.dvi -o server.ps

server.dvi	: server.tex 
		  latex server.tex

# End of Makefile
