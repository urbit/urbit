#!/usr/bin/env bash

set +e

PLATFORM=$(uname -s)
ARCH=$(uname -m)
SCRIPT=$(basename $0)

function require()
{
  set +e
  which $1 &>/dev/null
  while [ $? -ne 0 ]
  do
    if [ -z "$UNATTENDED" ]; then
      echo ""
      read -p "Please install '$1' then press enter"
    else
      echo "Required dependency '$1' is not installed" 1>&2
      exit 1
    fi
    which $1 &>/dev/null
  done
  set -e
}

if [[ $PLATFORM == *BSD ]] || [ $PLATFORM == "DragonFly" ]; then
  MAKE=gmake
  SED=gsed
else
  MAKE=make
  SED=sed
fi

require $SED
require $MAKE

function extract()
{
  test $# -ge 2 -a $# -lt 4 && test $2 -eq 2 && echo ""
  echo "extracting $(basename $1) ..."

  local tarflags

  tarflags="xf"
  test -n "$OCDEBUG" && tarflags+="v"

  case $1 in
    *.pkg)
      require cpio
      which xar &>/dev/null || exit 1
      xar -xf $1
      cat Payload | gunzip -dc | cpio -i 2>/dev/null && rm Payload
      ;;
    *.tar.xz)
      xz -dc $1 | tar $tarflags -
      ;;
    *.tar.gz)
      gunzip -dc $1 | tar $tarflags -
      ;;
    *.tar.bz2)
      bzip2 -dc $1 | tar $tarflags -
      ;;
    *)
      echo "Unhandled archive type" 2>&1
      exit 1
      ;;
  esac

  if [ $# -eq 2 -o $# -eq 4 ]; then
    echo ""
  fi
}

if [[ $PLATFORM == CYGWIN* ]]; then

function create_symlink()
{
  cp -f $1 $2
}

else

function create_symlink()
{
  ln -sf $1 $2
}

fi

function verbose_cmd()
{
  echo "$@"
  eval "$@"
}

function test_compiler()
{
  echo -ne "testing $1 ... "
  $1 $2 -v -O2 -Wall -o test
  rm test
  echo "works"
}

function test_compiler_cxx11()
{
  set +e
  echo -ne "testing $1 -stdlib=libc++ -std=c++11 ... "
  $1 $2 -O2 -stdlib=libc++ -std=c++11 -Wall -o test &>/dev/null
  if [ $? -eq 0 ]; then
    rm test
    echo "works"
  else
    echo "failed (ignored)"
  fi
  set -e
}

# exit on error
set -e
