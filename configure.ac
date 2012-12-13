#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(erlria, 0.0.1, alvin.msg@gmail.com)
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_DISABLE_STATIC

# Checks for programs.
AC_PROG_LIBTOOL
AC_ERLANG_NEED_ERLC
AC_ERLANG_NEED_ERL

AC_PROG_CPP
AC_PROG_CC
AC_PROG_CXX
AC_PROG_AWK
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_RANLIB
AC_PROG_MKDIR_P

# Checks for libraries.
dnl AC_CHECK_LIB([crypto], [main])
dnl AC_CHECK_LIB([db-4.2], [main])
dnl AC_CHECK_LIB([db_cxx-4.2], [main])

AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_ERTS_VER
AC_ERLANG_SUBST_INSTALL_LIB_SUBDIR(AC_PACKAGE_NAME, AC_PACKAGE_VERSION)

# erlcflags="+debug_info +bin_opt_info"
erlcflags="+debug_info"
AC_SUBST([ERLCFLAGS], [$erlcflags])

AC_ERLANG_CHECK_LIB(kernel)
AC_ERLANG_CHECK_LIB(stdlib)
AC_ERLANG_CHECK_LIB(sasl)
AC_ERLANG_CHECK_LIB(os_mon)
AC_ERLANG_CHECK_LIB(mnesia)
AC_ERLANG_CHECK_LIB(snmp)

# Checks for header files.
AC_HEADER_STDC
dnl AC_CHECK_HEADERS([stdlib.h string.h sys/systeminfo.h db_cxx.h])

# Checks for typedefs, structures, and compiler characteristics.
#AC_C_CONST
#AC_TYPE_SIZE_T

# Checks for library functions.
dnl AC_FUNC_MEMCMP
dnl AC_CHECK_FUNCS([memmove memset])

AC_CONFIG_FILES([
    Makefile
    c_src/Makefile
    ])

  dnl  mibs/Makefile
  dnl  src/Makefile
  dnl  ebin/Makefile
  dnl  priv/Makefile
  dnl bin/p2p.rel
  dnl  bin/p2prun
  dnl  bin/Makefile
    

    

AC_OUTPUT
