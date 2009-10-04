#
# File:    handc/cfg/rdln/gnu/Makefile
#
#          Copyright (C)1995 Donovan Kolbly <donovan@tkg.com>
#          as part of the RScheme project, licensed for free use
#
# Version: 1.2
# Date:    %G% 11:18:59
# Build:   v0.7.3.4-b7u, 2007-05-30
#
#


CFLAGS2=$(CFLAGS) -I$(HOME)/include
CFILES=rdln_gnu.c
PRODUCT=rdln.o

include ../postambl.mak

depend::
	$(CC) $(CFLAGS2) -MM -I. $(CFILES) > depends
.c.o:
	$(CC) $(CFLAGS2) -c $*.c
