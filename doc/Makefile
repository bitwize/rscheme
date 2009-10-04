all: rscheme.tex book1.htm

SGML_FILES=rscheme.sgml decl.sgml rsdoc.dsl
SGML_CAT=${HOME}/lib/text/catalog

rscheme.tex: $(SGML_FILES) deps
	SGML_CATALOG_FILES=$(SGML_CAT) \
	jade -t tex -d rsdoc.dsl#print decl.sgml rscheme.sgml

book1.htm: $(SGML_FILES) deps
	SGML_CATALOG_FILES=$(SGML_CAT) \
	jade -t sgml -ihtml -d rsdoc.dsl#html decl.sgml rscheme.sgml

REZ=400

rscheme.ps: rscheme.dvi
	dvips -D$(REZ) -orscheme.ps rscheme.dvi

rscheme.dvi: rscheme.tex
	jadetex rscheme

clean::
	rm -f rscheme.tex rscheme.dvi rscheme.ps
	rm -f ref.index chaps.index figs.index
	rm -f *.html

deps:: ref.index
deps:: chaps.index
deps:: figs.index

ref.index: ref
	ls ref | grep '\.sgml$$' | sed 's/\.sgml//' \
	 | awk '{ printf("<!entity ref.%s SYSTEM \"ref/%s.sgml\">\n",$$0,$$0); }' > ref.index

chaps.index: chaps
	ls chaps | grep '\.sgml$$' | sed 's/\.sgml//' \
	 | awk '{ printf("<!entity chap.%s SYSTEM \"chaps/%s.sgml\">\n",$$0,$$0); }' > chaps.index

tech.index: tech
	ls tech | grep '\.sgml$$' | sed 's/\.sgml//' \
	 | awk '{ printf("<!entity tech.%s SYSTEM \"tech/%s.sgml\">\n",$$0,$$0); }' > tech.index

figs.index: figs
	ls figs | grep '\.eps$$' | sed 's/\.eps//' \
	 | awk '{ printf("<!entity fig.%s SYSTEM \"figs/%s.eps\" ndata eps>\n",$$0,$$0); }' > figs.index

depend::
	awk -F\" '{ printf "deps:: %s\n", $$2 }' \
	  chaps.index ref.index figs.index > .depend

-include .depend
