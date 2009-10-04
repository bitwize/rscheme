SHELL=/bin/sh

TARGET_DIR=src
SRC_DIR=.
MODULES_DIR=$(TARGET_DIR)/install/resource/modules

#
#  can set `RSC_FLAGS=-ccode' to build nearly entire system to C
#  instead of bytecodes
#
RSC_FLAGS=-pragma function-definitions-are-const \
	  -pragma slots-are-sealed

#
#  `RSC_FLAGS2=-ccode' can be used to compile just certain modules to C
#  (ie: corelib lowscm objsys iolib highscm compiler codegen)
#  (by default -- most builds are development builds -- don't even set that)
#
#RSC_FLAGS2=-ccode
RSC_FLAGS2=

RSC=$(TARGET_DIR)/tmp/rsc
RSC_F=$(RSC) $(RSC_FLAGS)

RS=rs

MODULES=$(MODULES_DIR)/primops.mif \
	$(MODULES_DIR)/precore.mif \
	$(MODULES_DIR)/corelib.mif \
	$(MODULES_DIR)/low_scheme.mif \
	$(MODULES_DIR)/objsys.mif \
	$(MODULES_DIR)/paths.mif \
	$(MODULES_DIR)/mathlib.mif \
	$(MODULES_DIR)/tables.mif \
	$(MODULES_DIR)/earley.mif \
	$(MODULES_DIR)/iolib.mif \
	$(MODULES_DIR)/high_scheme.mif \
	$(MODULES_DIR)/start.mif \
	$(MODULES_DIR)/sort.mif \
	$(MODULES_DIR)/imageio.mif \
	$(MODULES_DIR)/editinp.mif \
	$(MODULES_DIR)/mlink.mif \
	$(MODULES_DIR)/compiler.mif \
	$(MODULES_DIR)/codegen.mif \
	$(MODULES_DIR)/repl.mif \
	$(MODULES_DIR)/hacks.mif \
	$(MODULES_DIR)/regex.mif \
	$(MODULES_DIR)/debugger.mif \
	$(MODULES_DIR)/threads.mif

base_system::  $(TARGET_DIR) rsc modules base_image configure_script

rsc:: $(TARGET_DIR)/tmp/rsc.img
modules:: $(MODULES)
base_image:: $(TARGET_DIR)/tmp/system.bas
configure_script:: $(TARGET_DIR)/configure

$(TARGET_DIR):
	rm -rf $(TARGET_DIR) ; mkdir $(TARGET_DIR)
	(cd ${SRC_DIR}/handc ; \
         find . \! \( -name CVS -prune \) -print) > .handc.list
	cat .handc.list | (cd ${SRC_DIR}/handc ; cpio -oc) \
	                | (cd ${TARGET_DIR} ; cpio -idc)
	if test -d $(TARGET_DIR)/tmp ; then : ; \
	else mkdir $(TARGET_DIR)/tmp ; fi

$(TARGET_DIR)/tmp/buildcfg.scm:
	RS=$(RS) compiler/mkcfg $(TARGET_DIR) > $(TARGET_DIR)/tmp/buildcfg.scm

$(TARGET_DIR)/tmp/rsc.img: $(TARGET_DIR)/tmp/buildcfg.scm
	RS=$(RS) compiler/mkrsc \
		./compiler \
		$(TARGET_DIR)/tmp \
		$(TARGET_DIR)/tmp \
		$(TARGET_DIR)/install \
		$(TARGET_DIR)/tmp/buildcfg.scm

BCDEFS=$(TARGET_DIR)/install/resource/compiler/bytecode/bcgen.scm


$(MODULES_DIR)/primops.mif $(BCDEFS): $(TARGET_DIR)/tmp/rsc.img
	$(RS) -q -image $(TARGET_DIR)/tmp/rsc.img \
	  -e '(process-defs-file "$(SRC_DIR)/bytcodes/bcdefs.dat")' \
	  -e "(create-primop-module 'primops)" -exit

#
#  linking
#

$(TARGET_DIR)/tmp/system.bas: $(MODULES)
	$(RSC) -o $(TARGET_DIR)/tmp/system.bas

#
#  autoconf
#

$(TARGET_DIR)/configure: $(SRC_DIR)/handc/configure.in
	cd $(TARGET_DIR) ; autoconf

#
#  building in stages...
#
#  `make ship' after a plain make will construct a tree appropriate
#  for distribution packaging such that `make stage1' is required
#  after unpacking on the target machine
#

ship::
	mv src stage0
	cd stage0 ; rm -f tmp/rsc tmp/rsc.img
	cd stage0 ; rm -f install/resource/modules/*
	rm -rf stage0/pkg

distclean::
	rm -rf src .handc.list
	-cd stage0 ; $(MAKE) clean
	-cd packages/lss ; $(MAKE) clean
	-cd packages/rstore ; $(MAKE) clean
	-cd packages/general ; $(MAKE) clean
	-cd packages/threads/shell ; $(MAKE) clean
	xargs rm -rf < cleanfiles

#
#  this uses the stage0 C code set up by `ship' to build a
#  new src/ directory using an rsc running on top of the 
#  stage0 tree
#

CONFIG_OPTS=

stage1::
	cd stage0 ; ./configure --prefix=`pwd`/install $(CONFIG_OPTS)
	cd stage0 ; make
	mkdir -p stage0/install/bin
	cd stage0 ; make shell
	cd stage0 ; ln -fs ../../rshell/rs install/bin/rs
	cd stage0 ; ln -fs ../../system.img install/resource/system.img
	$(MAKE) RS=`pwd`/stage0/install/bin/rs RSC_FLAGS2=-ccode \
	    src src/tmp/rsc.img $(MODULES) src/tmp/system.bas
	# avoid the autoconf step ; don't want to depend on it
	cp -p stage0/configure src/configure

#
#  all the modules
#

precore $(MODULES_DIR)/precore.mif: $(BCDEFS)
	$(RSC) -precore $(RSC_FLAGS) modules/corelib/precore.mcf

corelib $(MODULES_DIR)/corelib.mif: $(BCDEFS)
	$(RSC) -corelib $(RSC_FLAGS) $(RSC_FLAGS2) modules/corelib/corelib.mcf

lowscm $(MODULES_DIR)/low_scheme.mif: $(BCDEFS)
	$(RSC) -lowscm $(RSC_FLAGS) $(RSC_FLAGS2) modules/lowscm/lowscm.mcf

objsys $(MODULES_DIR)/objsys.mif: $(BCDEFS)
	$(RSC_F) $(RSC_FLAGS2) modules/objsys/objsys.mcf

paths $(MODULES_DIR)/paths.mif: $(BCDEFS)
	$(RSC_F) modules/paths/paths.mcf

tables $(MODULES_DIR)/tables.mif: $(BCDEFS)
	$(RSC_F) modules/tables/tables.mcf

mathlib $(MODULES_DIR)/mathlib.mif: $(BCDEFS)
	$(RSC_F) modules/mathlib/mathlib.mcf

iolib $(MODULES_DIR)/iolib.mif: $(BCDEFS)
	$(RSC_F) $(RSC_FLAGS2) modules/iolib/iolib.mcf

highscm $(MODULES_DIR)/high_scheme.mif: $(BCDEFS)
	$(RSC_F) $(RSC_FLAGS2) modules/highscm/highscm.mcf

start $(MODULES_DIR)/start.mif: $(BCDEFS)
	$(RSC_F) modules/start/start.mcf

sort $(MODULES_DIR)/sort.mif: $(BCDEFS)
	$(RSC_F) modules/sort/sort.mcf

imageio $(MODULES_DIR)/imageio.mif: $(BCDEFS)
	$(RSC_F) modules/imageio/imageio.mcf

editinp $(MODULES_DIR)/editinp.mif: $(BCDEFS)
	$(RSC_F) modules/editinp/editinp.mcf

compiler $(MODULES_DIR)/compiler.mif: $(BCDEFS)
	$(RSC_F) $(RSC_FLAGS2) modules/compiler/compiler.mcf

codegen $(MODULES_DIR)/codegen.mif: $(BCDEFS)
	$(RSC_F) $(RSC_FLAGS2) modules/codegen/codegen.mcf

mlink $(MODULES_DIR)/mlink.mif: $(BCDEFS)
	$(RSC_F) modules/mlink/mlink.mcf

repl $(MODULES_DIR)/repl.mif: $(BCDEFS)
	$(RSC_F) modules/repl/repl.mcf

debugger $(MODULES_DIR)/debugger.mif: $(BCDEFS)
	$(RSC_F) modules/debugger/debugger.mcf

regex $(MODULES_DIR)/regex.mif: $(BCDEFS)
	$(RSC_F) modules/regex/regex.mcf

earley $(MODULES_DIR)/earley.mif: $(BCDEFS)
	$(RSC_F) modules/earley/earley.mcf

threads $(MODULES_DIR)/threads.mif: $(BCDEFS)
	$(RSC_F) modules/threads/threads.mcf

hacks $(MODULES_DIR)/hacks.mif: $(BCDEFS)
	$(RSC_F) modules/hacks/hacks.mcf

