dnl Process this file with autoconf to produce a configure script.

AC_INIT(gd.scm)

AC_PROG_CC()

AC_CHECK_LIB(gd,gdImageCreate)

AC_CHECK_MEMBER(struct gdIOCtx.free,
                [AC_DEFINE(LIBGD_IOCTX_HAS_FREE,1)],
                [AC_DEFINE(LIBGD_IOCTX_HAS_FREE,0)],
                [
#include <gd.h>
])

AC_CONFIG_HEADER(config.h)
