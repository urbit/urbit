#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# Initialize autoconf
AC_PREREQ([2.69])
AC_INIT([urcrypt], [1.0])
AC_CONFIG_SRCDIR([urcrypt/util.c])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([build-aux/m4])
AC_CANONICAL_HOST

# Initialize automake
AM_INIT_AUTOMAKE([foreign subdir-objects -Wall -Werror])

# Initialize libtool
AM_PROG_AR
LT_INIT

# Initialize pkgconfig
PKG_PROG_PKG_CONFIG
PKG_INSTALLDIR

# Checks for programs
AC_PROG_CC

# Checks for pkg-config capable libraries
PKG_CHECK_MODULES([libsecp256k1], [libsecp256k1])
PKG_CHECK_MODULES([libcrypto], [libcrypto])

AS_IF([test "$enable_shared" == "yes"],
  [# ensure crypto will be shared for shared object (see README.md)
   save_LDFLAGS=$LDFLAGS
   save_CLAGS=$CFLAGS
   LDFLAGS=$libcrypto_LIBS
   CFLAGS=$libcrypto_CFLAGS
   AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <openssl/err.h>],
                                   [ERR_load_crypto_strings()])],
     [AC_PROG_GREP
      AC_CHECK_TOOL([NM], [nm])
      AC_MSG_CHECKING([for shared libcrypto])
      AS_IF(
        [$NM -f posix conftest$EXEEXT |
         $GREP 'ERR_load_crypto_strings U' 2>&1 >/dev/null],
        [AC_MSG_RESULT([yes])],
        [AC_MSG_ERROR([cannot find shared object for libcrypto.])])],
     [AC_MSG_ERROR([unable to link libcrypto.])])
     LDFLAGS=$save_LDFLAGS
     CFLAGS=$save_CFLAGS])

# Checks for non pkg-config libraries
AC_CHECK_LIB([aes_siv], [AES_SIV_CTX_new],
             [libaes_siv_LIBS="-laes_siv"
              AC_SUBST([libaes_siv_LIBS])],
             [AC_MSG_ERROR([libaes_siv is required.])])

# Checks for header files.
AC_CHECK_HEADERS([limits.h stddef.h stdint.h stdlib.h string.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_SIZE_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T

# Checks for library functions.
AC_CHECK_FUNCS([memset])

# Finish and output
AC_CONFIG_FILES([Makefile liburcrypt.pc])
AC_OUTPUT
