dnl Process this file with autoconf to produce a configure script.

AC_INIT(README)

AC_PROG_CC()

install_dir=NONE

AC_SUBST(install_dir)
AC_SUBST(rs)

rs="$with_rs"
if test "$rs" = ""
then rs=rs
fi
echo "with rs... ($rs)"

if test "$install_dir" = NONE
then install_dir="`$rs --install`"
fi

echo "install directory... ($install_dir)"

# If we try to use AC_CONFIG_SUBDIRS(), then autoconf wants install-sh
# AC_CONFIG_SUBDIRS( graphics/gd )
(cd graphics/gd ; ./configure)

AC_OUTPUT(Makefile)
