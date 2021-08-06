#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# There are two versions numbers that must be updated on each public release.
#  1.  The libtool version (useful to the linker)
#  2.  The semantic version (useful to humans)

# Follow these instructions sequentially for the libtool version:
#  1. If the library source code has changed at all since the last update,
#     then increment revision (‘c:r:a’ becomes ‘c:r+1:a’).
#  2. If any interfaces have been added, removed, or changed since the last
#     update, increment current, and set revision to 0.
#  3. If any interfaces have been added since the last public release,
#     then increment age.
#  4. If any interfaces have been removed or changed since the last public
#     release, then set age to 0.
m4_define([urcrypt_lt_current],  [0])
m4_define([urcrypt_lt_revision], [0])
m4_define([urcrypt_lt_age],      [0])

# The package version uses semantic versioning (semver.org).
# In summary,increment the:
#  1. MAJOR version when you make incompatible API changes,
#  2. MINOR version when you add functionality in a backwards compatible manner, and
#  3. PATCH version when you make backwards compatible bug fixes.
m4_define([urcrypt_sv_major], [0])
m4_define([urcrypt_sv_minor], [1])
m4_define([urcrypt_sv_patch], [0])

# Initialize autoconf
AC_PREREQ([2.69])
AC_INIT([urcrypt], [urcrypt_sv_major.urcrypt_sv_minor.urcrypt_sv_patch])
AC_SUBST([URCRYPT_API_VERSION], [urcrypt_sv_major])
AC_SUBST([URCRYPT_LT_VERSION],
         [urcrypt_lt_current:urcrypt_lt_revision:urcrypt_lt_age])

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
PKG_CHECK_MODULES([LIBSECP256K1], [libsecp256k1])
AC_CHECK_HEADER([secp256k1_recovery.h], [],
                [AC_MSG_ERROR([libsecp256k1 must have recovery enabled.])])
PKG_CHECK_MODULES([LIBCRYPTO], [libcrypto])

AS_IF([test "$enable_shared" == "yes"],
  [# ensure crypto will be shared for shared object (see README.md)
   save_LDFLAGS=$LDFLAGS
   save_CLAGS=$CFLAGS
   LDFLAGS=$LIBCRYPTO_LIBS
   CFLAGS=$LIBCRYPTO_CFLAGS
   AC_LINK_IFELSE([AC_LANG_PROGRAM([#include <openssl/sha.h>],
                                   [[unsigned char sha[32];
                                     SHA256("hello", 5, sha);]])],
     [AC_PROG_GREP
      AC_CHECK_TOOL([NM], [nm])
      AC_MSG_CHECKING([for shared libcrypto])
      AS_IF(
        [$NM conftest$EXEEXT | $GREP 'U .*SHA256' 2>&1 >/dev/null],
        [AC_MSG_RESULT([yes])],
        [AC_MSG_ERROR([cannot find shared object for libcrypto.])])],
     [AC_MSG_ERROR([unable to link libcrypto.])])
     LDFLAGS=$save_LDFLAGS
     CFLAGS=$save_CFLAGS])

# Checks for non pkg-config libraries
AC_CHECK_LIB([aes_siv], [AES_SIV_CTX_new],
             [AC_SUBST([LIBAES_SIV_LIBS], "-laes_siv")],
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
AC_CONFIG_FILES([Makefile liburcrypt-$URCRYPT_API_VERSION.pc:liburcrypt.pc.in])
AC_OUTPUT
