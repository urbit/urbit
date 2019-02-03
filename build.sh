#!/usr/bin/env bash

CFLAGS="$CFLAGS -funsigned-char -ffast-math"
CFLAGS="$CFLAGS '-DURBIT_VERSION=\"0.7.0\"' -funsigned-char -ffast-math"

[ -n "$MEMORY_DEBUG" ]     && CFLAGS="$CFLAGS -DU3_MEMORY_DEBUG=1"
[ -n "$CPU_DEBUG" ]        && CFLAGS="$CFLAGS -DU3_CPU_DEBUG=1"
[ -n "$EVENT_TIME_DEBUG" ] && CFLAGS="$CFLAGS -DU3_EVENT_TIME_DEBUG=1"

case $(sed 'y/A-Z/a-z/' <<<"$host") in
  *linux*)
     CFLAGS="$CFLAGS -DU3_OS_linux=1"
     ;;
  *darwin*)
     CFLAGS="$CFLAGS -DU3_OS_osx=1"
     ;;
  *freebsd*)
     CFLAGS="$CFLAGS -DU3_OS_bsd=1"
     LDFLAGS="$LDFLAGS -lkvm"
     ;;
  *openbsd*)
     CFLAGS="$CFLAGS -DU3_OS_bsd=1"
esac

CFLAGS="$CFLAGS -DU3_OS_ENDIAN_little=1"

# TODO Error if $host is a big-endian CPU.

rm -f include/config.h && touch include/config.h

# TODO Might need to change structure of these defines.
## #pragma once
##
## #define URBIT_VERSION
##
## #define U3_OS_linux
## #define U3_OS_bsd
## #define U3_OS_osx
##
## #define U3_OS_ENDIAN_little=0
## #define U3_OS_ENDIAN_big=1
##
## #define U3_MEMORY_DEBUG=0
## #define U3_CPU_DEBUG=0
## #define U3_EVENT_TIME_DEBUG=0

LDFLAGS="$LDFLAGS -lcurl -lgmp -lsigsegv"
LDFLAGS="$LDFLAGS -largon2 -led25519 -lent -lh2o -lscrypt -lsni -luv -lmurmur3"
LDFLAGS="$LDFLAGS -lsecp256k1 -lsoftfloat3 -lncurses -lssl -lcrypto -lz"

echo "CFLAGS='$CFLAGS'"

echo "LDFLAGS='$LDFLAGS'"

export CFLAGS
export LDFLAGS

make -j8
