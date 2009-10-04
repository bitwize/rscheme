CFILES=lghmain.c
PRODUCT=lgh.o

all: $(PRODUCT) sys.img
LIB_RS=../install/lib/librs.a

include ../postambl.mak

sys.img:
	rs lgh.scm -exit

learn0:
	$(CC) $(IFLAGS) learn0.c $(LIB_RS) $(LDX) -o learn0
