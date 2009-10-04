

EPS_FILES=list1.eps list2.eps list3.eps str1.eps

all:: $(EPS_FILES)

.SUFFIXES: .ofig .strfig .eps

.ofig.eps:
	../tools/ofig2eps $*.ofig


.strfig.eps:
	../tools/strfig2eps -o $*.eps < $*.strfig

clean::
	rm -f $(EPS_FILES)
