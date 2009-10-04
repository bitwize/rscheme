include /u/donovan/lib/rs/0.6.0/resource/buildenv/preamble.mak

CFILES=main.c cursesx.c
PRODUCT=rsmail
RSC=/u/TKG/bin/rs -image /u/rscheme/rs-0.6.0/compiler/compiler.img
OFILES=$(CFILES:.c=.o)

all:: $(PRODUCT) curses.img

$(PRODUCT): $(OFILES)
	$(CC) -pg $(OFILES) $(LDX_FLAGS) -o $(PRODUCT) -lrs -lm -lcurses

clean::
	rm -f $(OFILES) $(PRODUCT)

depend::
	$(CC) $(CFLAGS) -MM -I. $(CFILES) > depends

#.. include depends

curses.mif: wfuncs.scm curses.mcf smail.scm viewquery.scm query.scm
	$(RSC) curses.mcf

curses.img: curses.mif
	echo '(load "bld")' | $(RSC)

PKG=curses

install.mif::
	cp $(PKG)/$(PKG).mif $(INSTALL_DIR)/resource/modules
	cp $(PKG)/$(PKG).mx $(INSTALL_DIR)/resource/modules
