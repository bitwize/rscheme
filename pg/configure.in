dnl Process this file with autoconf to produce a configure script.

AC_INIT(gettuple.c)

pg_include=NONE
pg_lib=NONE

AC_SUBST(rs_prefix)
AC_SUBST(prefix)
AC_SUBST(rs)
AC_SUBST(pg_include)
AC_SUBST(pg_lib)
AC_SUBST(more_libs)

if test "$with_rs" = ""
then with_rs=rs
fi
rs="$with_rs"

if test "$with_pg" = ""
then :
else pg_include=$with_pg/include
     pg_lib=$with_pg/lib
fi

rs_prefix="`$rs --install`"

if test "$prefix" = "NONE"
then prefix="$rs_prefix"
fi

echo "prefix=$prefix"

if test "$pg_include" = NONE
then if test -f /usr/include/postgres95/libpq-fe.h
     then pg_include=/usr/include/postgres95
     else if test -f /usr/local/pgsql/include/libpq-fe.h
          then pg_include=/usr/local/pgsql/include
          else if test -f /usr/include/libpq-fe.h
               then pg_include=/usr/include
               else echo "** couln't find PostgresQL include directory (libpq-fe.h)" >&2
                    exit 1
               fi
          fi
     fi
fi

if test "$pg_lib" = NONE
then if test -f /usr/lib/libpq.a
     then pg_lib=/usr/lib
     else if test -d /usr/local/pgsql/lib/libpq.a
          then pg_lib=/usr/local/pgsql/lib
          else pg_lib=/usr/lib
               echo "** couln't find PostgresQL library (libpq.a)"
          fi
     fi
fi
more_libs="-lcrypt"

AC_OUTPUT(Makefile)
