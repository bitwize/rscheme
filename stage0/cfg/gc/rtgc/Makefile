
CCFILES=colorset.cc gc.cc gen.cc objmgr.cc gcserver.cc \
	allocator.cc igps.cc scheduler.cc pointers.cc scanroot.cc
CFILES=sizclass.c
PRODUCT=qgc.o

include ../postambl.mak

.SUFFIXES: .cc .o

.cc.o:
	$(CC) $(CFLAGS) -c $*.cc
.c.o:
	$(CC) $(CFLAGS) -c $*.c
