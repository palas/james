#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.68])
AC_INIT([james], [0.0.1], [P.Lamela-Seijas@kent.ac.uk])
AC_CONFIG_SRCDIR([james.cpp])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_AUX_DIR(config)
AC_LANG(C++)
AC_OUTPUT(Makefile)
AM_INIT_AUTOMAKE
LT_INIT
# Checks for programs.
AC_PROG_CXX
AC_CANONICAL_HOST
# Checks for libraries.
# Check for JNI
AC_CONFIG_MACRO_DIR([m4])
AX_JNI_INCLUDE_DIR

for JNI_INCLUDE_DIR in $JNI_INCLUDE_DIRS
do
  CPPFLAGS="$CPPFLAGS -I$JNI_INCLUDE_DIR"
done

# Check for JVMTI
AC_CHECK_HEADER(jvmti.h, [], [AC_MSG_FAILURE([The JVMTI include files are required. Try setting the variable \$JAVA_HOME.])])

# Check for boost library.
AC_MSG_CHECKING([for boost libraries])
LIBS="-lboost_system -lpthread"
AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <boost/asio.hpp>], [])],
               [AC_MSG_RESULT([yes])],
               [AC_MSG_RESULT([error])]
	       [AC_MSG_FAILURE([The boost librares are required. Try installing libboost-system-dev package.])])

# Checks for header files.
AC_CHECK_HEADERS([string.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([memset])

AC_ARG_ENABLE(debug,
AS_HELP_STRING([--enable-debug],
               [enable debugging, default: no]),
[case "${enableval}" in
             yes) debug=true ;;
             no)  debug=false ;;
             *)   AC_MSG_ERROR([bad value ${enableval} for --enable-debug]) ;;
esac],
[debug=false])

AM_CONDITIONAL(DEBUG, test x"$debug" = x"true")

AC_OUTPUT
