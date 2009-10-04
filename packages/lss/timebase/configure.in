dnl Process this file with autoconf to produce a configure script.

AC_INIT(timebase.h)

AC_SUBST(READ_C)
AC_SUBST(READ_S)

AC_PROG_CC
AC_PROG_CPP
AC_CANONICAL_HOST

echo "host is: $host"

case "$host" in
  rs6000-*-aix*)   ARCH=rs6000   ;;
  powerpc-*-aix*)  ARCH=rs6000   ;;
  powerpc-*-darwin*) ARCH=ppcdarwin ;;
  powerpc-*-*)     ARCH=ppc      ;;
  *)               ARCH=posix    ;;
esac

if test -f "$ARCH"/readtime.s
then READ_S="$ARCH"/readtime.s
     AC_DEFINE(HAS_NATIVE_TIMEBASE,1)
     echo " (using $ARCH/readtime.s)"
else if test -f "$ARCH"/readtime.c
     then READ_C="$ARCH"/readtime.c
          AC_DEFINE(HAS_NATIVE_TIMEBASE,1)
          echo " (using $ARCH/readtime.c)"
     else READ_C=""
          AC_DEFINE(HAS_NATIVE_TIMEBASE,0)
          echo " (no $ARCH/readtime.s; Using timeval)"
     fi
fi

AC_CONFIG_HEADER(config.h)
AC_OUTPUT(Makefile)
