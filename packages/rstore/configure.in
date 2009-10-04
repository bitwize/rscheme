dnl Process this file with autoconf to produce a configure script. -*-sh-*-

AC_INIT(swizzler.h)

AC_SUBST(rs_prefix)
AC_SUBST(prefix)
AC_SUBST(rs)

AC_PROG_CC
AC_PROG_CPP

AC_CANONICAL_HOST

if test "$with_rs" = ""
then with_rs=rs
fi
rs="$with_rs"

rs_prefix="`$rs --install`"

if test "$prefix" = "NONE"
then prefix="$rs_prefix"
fi

echo "RScheme `$rs --version` in $rs_prefix"

AC_CONFIG_HEADER(config.h)
AC_OUTPUT(Makefile lru_model/Makefile old_model/Makefile)
