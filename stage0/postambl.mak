include ../preamble.mak

OFILES=$(CFILES:.c=.o) $(CCFILES:.cc=.o)

$(PRODUCT): $(OFILES)
	$(LD) $(LD_FLAGS) $(OFILES) -o $(PRODUCT)
	ar ru ../install/lib/librs.a $(PRODUCT)

clean::
	rm -f $(OFILES) $(PRODUCT)

depend::
	$(CC) $(CFLAGS) -MM -I. $(CFILES) > depends
