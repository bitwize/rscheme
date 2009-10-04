PRODUCT=dv

all:: about.tiff dv
RS=fshell

$(PRODUCT):
	$(RS) dv.scm -c dv.fas
	mv dv.fas dv
	makex dv $(RS)

about.tiff: about-full.tiff about-q99.tiff
	tiffcp -c packbits about-full.tiff about-q99.tiff about.tiff

about-q99.tiff: about-full.tiff
	tiffmedian -C 99 about-full.tiff about-q99.tiff

clean::
	rm -f dv dv.fas about-q99.tiff about.tiff scm.dep

depend::
	$(RS) ../../../MM.scm -e '(MM "scm.dep")' dv.scm -exit

-include scm.dep
