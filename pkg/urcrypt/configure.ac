#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# Initialize autoconf
AC_PREREQ([2.69])
AC_INIT([urcrypt], [1.0])
AC_CONFIG_SRCDIR([urcrypt.c])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([build-aux/m4])

# Initialize automake
AM_INIT_AUTOMAKE([foreign subdir-objects -Wall -Werror])

# Initialize libtool
AM_PROG_AR
LT_INIT

# Checks for programs
AC_PROG_CC

# Checks for pkg-config capable libraries
PKG_CHECK_MODULES([SECP256K1], [libsecp256k1])
PKG_CHECK_MODULES([CRYPTO], [libcrypto])

# Checks for non pkg-config libraries
AC_CHECK_LIB([aes_siv], [AES_SIV_CTX_new])

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
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
