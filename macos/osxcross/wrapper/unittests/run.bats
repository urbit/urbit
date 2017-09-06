#!/usr/bin/env bats

#
# Requirements:
#  * bats (https://github.com/sstephenson/bats)
#  * OSXCRoss >= 0.11 must be in PATH
#

export OSXCROSS_UNIT_TEST=2
export OCDEBUG=1

unset OSXCROSS_NO_EXTENSION_WARNINGS
unset OSXCROSS_NO_X86_64H_DEPLOYMENT_TARGET_WARNING
unset OSXCROSS_GCC_NO_STATIC_RUNTIME
unset MACOSX_DEPLOYMENT_TARGET
unset OSXCROSS_SDKROOT

GCC_INSTALLED=1

command -v o64-clang++ >/dev/null 2>&1 || { echo "OSXCross must be in PATH" 1>&2; exit 1; }
command -v o64-g++ >/dev/null 2>&1 || GCC_INSTALLED=0

if [ $GCC_INSTALLED -eq 1 ]; then
  if [ "$(o64-clang++ -foc-run-prog=osxcross-conf 2>/dev/null)" != \
      "$(o64-g++ -foc-run-prog=osxcross-conf 2>/dev/null)" ]; then
      true
    #echo "warning: o64-clang++ and o64-g++ come from different installations; aborting unit testing" 1>&2
    GCC_INSTALLED=0
  fi
fi

eval $(osxcross-conf)

@test "osxcross-cmp" {
  run osxcross-cmp
  [ "$status" -ne 0 ]

  run osxcross-cmp 10.5 '>' 10.6
  [ "$status" -eq 0 ]
  [ "$output" = "0" ]

  run osxcross-cmp 10.11 '>' 10.10
  [ "$status" -eq 0 ]
  [ "$output" = "1" ]

  run osxcross-cmp 10.11 '>=' 10.12
  [ "$status" -eq 0 ]
  [ "$output" = "0" ]

  run osxcross-cmp 10.11 '<=' 10.12
  [ "$status" -eq 0 ]
  [ "$output" = "1" ]

  run osxcross-cmp 10.10 '==' 10.10
  [ "$status" -eq 0 ]
  [ "$output" = "1" ]

  run osxcross-cmp 10.10 '!=' 10.10
  [ "$status" -eq 0 ]
  [ "$output" = "0" ]

  run osxcross-cmp 10.10 '?' 10.10
  [ "$status" -ne 0 ]

  run osxcross-cmp 10.10 '>'
  [ "$status" -ne 0 ]
}

@test "cross compiler arguments" {
  run o64-clang++
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == *\ *clang*\ \(clang++\)\ * ]]
  [[ "${lines[0]}" == *\ -target\ x86_64-apple-darwin*\ * ]]
  [[ "${lines[0]}" == *\ -mlinker-version=$OSXCROSS_LINKER_VERSION\ * ]]
  [[ "${lines[0]}" == *\ -isystem\ */clang/*.*/include\ * ]]
  [[ "${lines[0]}" == *\ -isysroot\ */MacOSX10.*.sdk\ * ]]
  [[ "${lines[0]}" == *\ -stdlib=*++*\ * ]]
  [[ "${lines[0]}" == *\ -mmacosx-version-min=10.* ]]
  [[ "${lines[0]}" == *\ -arch\ x86_64\ * ]]
  [[ "${lines[0]}" != *\ -Wl,-no_compact_unwind\ * ]]

  run o32-clang
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == *\ *clang*\ \(clang\)\ * ]]
  [[ "${lines[0]}" == *\ -target\ i386-apple-darwin*\ * ]]

  if [ $GCC_INSTALLED -eq 1 ]; then
    run o64-g++
    [ "$status" -eq 0 ]
    [[ "${lines[0]}" == *\ *x86_64-apple-darwin*-base-g++*\ \(x86_64-apple-darwin*-g++\)\ * ]]
    [[ "${lines[0]}" != *\ -target\ x86_64-apple-darwin*\ * ]]
    [[ "${lines[0]}" != *\ -mlinker-version=*\ * ]]
    [[ "${lines[0]}" != *\ -isystem\ */clang/*.*/include\ * ]]
    [[ "${lines[0]}" != *\ -isysroot\ */MacOSX10.*.sdk\ * ]]
    [[ "${lines[0]}" != *\ -stdlib=*++*\ * ]]
    [[ "${lines[0]}" == *\ -mmacosx-version-min=10.* ]]
    [[ "${lines[0]}" == *\ -m64\ * ]]
    [[ "${lines[0]}" == *\ -Wl,-no_compact_unwind\ * ]]

    run o32-g++
    [ "$status" -eq 0 ]
    [[ "${lines[0]}" == *\ *i386-apple-darwin*-base-g++*\ \(i386-apple-darwin*-g++\)\ * ]]
  fi
}

@test "OCDEBUG=2" {
  OCDEBUG=2 run o64-clang++
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == *debug:\ detected\ target\ triple:\ *-apple-darwin* ]]
  [[ "${lines[1]}" == *debug:\ detected\ compiler:\ * ]]
  [[ "${lines[2]}" == *debug:\ detected\ stdlib:\ * ]]
}

@test "argv[0] parsing" {
  OSXCROSS_PROG_NAME=x86_64-apple-darwin999999-clang++ run o64-clang++
  [ "$status" -ne 0 ]
  [[ "${lines[0]}" == *warning:\ this\ wrapper\ was\ built\ for\ target\ \'darwin*\' ]]
  [[ "${lines[1]}" == *error:\ cannot\ find\ Mac\ OS\ X\ SDK\ \(expected\ in:\ */SDK/MacOSX10.999995.sdk\) ]]

  OSXCROSS_PROG_NAME=x86_64-apple-$OSXCROSS_TARGET-gcc run o64-clang++
  [ "$status" -eq 0 ]
  [[ -z "$OUTPUT" ]]

  OSXCROSS_PROG_NAME=x86_64-apple-$OSXCROSS_TARGET-g++ run o64-clang++
  [ "$status" -eq 0 ]
  [[ -z "$OUTPUT" ]]

  OSXCROSS_PROG_NAME=x86_64-apple-$OSXCROSS_TARGET-cc run o64-clang++
  [ "$status" -eq 0 ]
  [[ -z "$OUTPUT" ]]

  OSXCROSS_PROG_NAME=x86_64-apple-$OSXCROSS_TARGET-c++ run o64-clang++
  [ "$status" -eq 0 ]
  [[ -z "$OUTPUT" ]]

  OSXCROSS_PROG_NAME=i386-apple-$OSXCROSS_TARGET-clang++ run o64-clang++
  [ "$status" -eq 0 ]

  OSXCROSS_PROG_NAME=i486-apple-$OSXCROSS_TARGET-clang++ run o64-clang++
  [ "$status" -eq 0 ]

  OSXCROSS_PROG_NAME=i586-apple-$OSXCROSS_TARGET-clang++ run o64-clang++
  [ "$status" -eq 0 ]

  OSXCROSS_PROG_NAME=i686-apple-$OSXCROSS_TARGET-clang++ run o64-clang++
  [ "$status" -eq 0 ]

  OSXCROSS_PROG_NAME=i786-apple-$OSXCROSS_TARGET-clang++ run o64-clang++
  [ "$status" -ne 0 ]

  OSXCROSS_PROG_NAME=armv6-apple-$OSXCROSS_TARGET-clang++ run o64-clang++
  [ "$status" -ne 0 ]

  OSXCROSS_PROG_NAME=x86_64-mapple-$OSXCROSS_TARGET-clang++ run o64-clang++
  [ "$status" -ne 0 ]

  OSXCROSS_PROG_NAME=x86_64-apple-ddarwin13-clang++ run o64-clang++
  [ "$status" -ne 0 ]

  OSXCROSS_PROG_NAME=x86_64-apple--clang++ run o64-clang++
  [ "$status" -ne 0 ]

  OSXCROSS_PROG_NAME=x86_64-apple-$OSXCROSS_TARGET-foo run o64-clang++
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == *warning:\ unknown\ compiler\ \'foo\' ]]

  OSXCROSS_PROG_NAME= run o64-clang++
  [ "$status" -ne 0 ]

  OSXCROSS_PROG_NAME=? run o64-clang++
  [ "$status" -ne 0 ]
}

@test "command line parsing" {
  run o64-clang++ -mmacosx-version-min=10.4 -g -I test1 -march=native -Itest2 -O2 \
                  -xc-header -x c++-header -stdlib=libstdc++ -stdlib=default -stdlib=libstdc++ \
                  -otest1 -o test2 foo.cpp -c
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == *\ -mmacosx-version-min=10.4*\ * ]]
  [[ "${lines[0]}" == *\ -g\ *\ * ]]
  [[ "${lines[0]}" == *\ -I\ test1\ * ]]
  [[ "${lines[0]}" == *\ -march=native\ * ]]
  [[ "${lines[0]}" == *\ -Itest2\ * ]]
  [[ "${lines[0]}" == *\ -O2\ * ]]
  [[ "${lines[0]}" == *\ -xc-header\ * ]]
  [[ "${lines[0]}" == *\ -x\ c++-header\ * ]]
  [[ "${lines[0]}" != *\ -stdlib=default\ * ]]
  [[ "${lines[0]}" == *\ -otest1\ * ]]
  [[ "${lines[0]}" == *\ -o\ test2\ * ]]
  [[ "${lines[0]}" == *\ foo.cpp\ * ]]
  [[ "${lines[0]}" == *\ -c\ * ]]
}

@test "-mmacosx-version-min/MACOSX_DEPLOYMENT_TARGET" {
  run o64-clang++ -mmacosx-version-min=10.10000
  [ "$status" -ne 0 ]
  [[ "${lines[0]}" == *error:\ targeted\ OS\ X\ version\ must\ be\ \<=\ 10.*.0\ \(SDK\) ]]

  run o64-clang++ -mmacosx-version-min=abc
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == *warning:\ \'-mmacosx-version-min=\'\ \(0.0.0\ !=\ abc\) ]]
  [[ "${lines[1]}" != *\ -mmacosx-version-min=abc\ * ]]
  [[ "${lines[1]}" != *\ -mmacosx-version-min=0.0*\ * ]]

  run o64-clang++ -mmacosx-version-min=10.0
  [ "$status" -ne 0 ]
  [[ "${lines[0]}" == *targeted\ OS\ X\ version\ must\ be\ \>=\ 10.4 ]]

  run o64-clang++ -mmacosx-version-min=10.0 -mmacosx-version-min=$OSXCROSS_SDK_VERSION
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == *\ -mmacosx-version-min=$OSXCROSS_SDK_VERSION.*\ * ]]

  MACOSX_DEPLOYMENT_TARGET=10.4 run o64-clang++ -mmacosx-version-min=10.0 -mmacosx-version-min=10.4
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == *\ -mmacosx-version-min=10.4.*\ * ]]

  MACOSX_DEPLOYMENT_TARGET=10.4 run o64-clang++
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == *\ -mmacosx-version-min=10.4.*\ * ]]
}

@test "-stdlib/-stdc++/-gstdc++/-foc-use-gcc-libstdc++/-libc++" {
  function libcxx()
  {
    run $@
    if [[ "$@" == *-mmacosx-version-min=10.4* ]]; then
      [ "$status" -ne 0 ]
      [[ "${lines[0]}" == *error:\ libc++\ requires\ \'-mmacosx-version-min=10.7\'\ \(or\ later\) ]]
    else
      [ "$status" -eq 0 ]
      if [[ $1 == *clang++* ]]; then
        [[ "${lines[0]}" == *\ -cxx-isystem\ */SDK/MacOSX10.*.sdk/usr/include/c++/v1\ * ]]
        [[ "${lines[0]}" == *\ -stdlib=libc++\ * ]]
        [[ "${lines[0]}" != *\ -stdlib=libstdc++\ * ]]
      else
        if [[ "$@" == *-stdlib* ]]; then
          [[ "${lines[0]}" == *warning:\ \'-stdlib=\'\ is\ an\ osxcross\ extension* ]]
        else
          [[ "${lines[0]}" == *\ -nostdinc++\ -nodefaultlibs\ -lc\ -lc++\ -lgcc_s.10.5\ * ]]
          [[ "${lines[0]}" == *\ -isystem\ */SDK/MacOSX10.*.sdk/usr/include/c++/v1\ * ]]
        fi
      fi
    fi
  }

  if [ $(osxcross-cmp $OSXCROSS_SDK_VERSION '>=' 10.7) -eq 1 ]; then
    libcxx "o64-clang++" "-stdlib=libc++"
    libcxx "o64-clang++-libc++"
    libcxx "o64-clang++-libc++" "-mmacosx-version-min=10.4"
    if [ $GCC_INSTALLED -eq 1 ]; then
      libcxx "o64-g++-libc++"
      libcxx "o64-g++" "-stdlib=libc++"
      libcxx "o64-g++" "-stdlib=libc++"
    fi
  fi

  function stdcxx()
  {
    run $@
    [ "$status" -eq 0 ]
    [[ "${lines[0]}" == *\ -stdlib=libstdc++\ * ]]
    [[ "${lines[0]}" != *\ -stdlib=libc++\ * ]]
    [[ "${lines[0]}" != *\ -cxx-isystem\ */SDK/MacOSX10.*.sdk/usr/include/c++/v1\ * ]]
    [[ "${lines[0]}" == *\ -cxx-isystem\ */SDK/MacOSX10.*.sdk/usr/include/c++/4.*\ * ]]
    [[ "${lines[0]}" == *\ -cxx-isystem\ */SDK/MacOSX10.*.sdk/usr/include/c++/4.*/*-apple-darwin*\ * ]]
    [[ "${lines[0]}" == *\ -cxx-isystem\ */SDK/MacOSX10.*.sdk/usr/include/c++/4.*/backward\ * ]]
  }

  stdcxx "o64-clang++" "-stdlib=libstdc++"
  stdcxx "o64-clang++-stdc++"

  run o64-clang++-stdc++ -stdlib=libc++
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == *warning:\ ignoring\ \'-stdlib=libc++\' ]]

  function gstdcxx()
  {
    run $@
    if [[ "${lines[0]}" == *error:\ \'-foc-use-gcc-libstdc++\'\ requires\ gcc\ to\ be\ installed\ \(./build_gcc.sh\) ]];
    then
      [ "$status" -ne 0 ]
    else
      [ "$status" -eq 0 ]
      if [[ $1 == *clang++* ]]; then
        [[ "${lines[0]}" == *\ -Qunused-arguments\ * ]]
      fi
      [[ "${lines[0]}" == *\ -cxx-isystem\ */x86_64-apple-darwin*/include/c++/*.*\ * ]]
      [[ "${lines[0]}" == *\ -cxx-isystem\ */x86_64-apple-darwin*/include/c++/*.*/x86_64-apple-darwin*\ *  ]]
      [[ "${lines[0]}" == *\ -cxx-isystem\ */x86_64-apple-darwin*/include/c++/*.*/backward\ * ]]
      if [[ $1 == *++  ]]; then
        [[ "${lines[0]}" == *\ -nostdinc++\ * ]]
        [[ "${lines[0]}" == *\ -stdlib=libstdc++\ * ]]
        [[ "${lines[0]}" == *\ -nodefaultlibs\ * ]]
        [[ "${lines[0]}" == *\ -Wl,-no_compact_unwind\ * ]]
        if [ -n "$OSXCROSS_GCC_NO_STATIC_RUNTIME" ]; then
          [[ "${lines[0]}" == *\ -L\ */lib/gcc/x86_64-apple-darwin*/*.*\ * ]]
          [[ "${lines[0]}" == *\ -L\ */x86_64-apple-darwin*/lib*\ * ]]
          [[ "${lines[0]}" == *\ -l\ stdc++\ * ]]
          [[ "${lines[0]}" == *\ -l\ supc++\ * ]]
          [[ "${lines[0]}" == *\ -l\ gcc\ * ]]
          [[ "${lines[0]}" == *\ -l\ gcc_eh\ * ]]
        else
          [[ "${lines[0]}" == *\ */x86_64-apple-darwin*/lib*/libstdc++.a\ * ]]
          [[ "${lines[0]}" == *\ */x86_64-apple-darwin*/lib*/libsupc++.a\ * ]]
          [[ "${lines[0]}" == *\ */lib/gcc/x86_64-apple-darwin*/*.*/libgcc.a\ * ]]
          [[ "${lines[0]}" == *\ */lib/gcc/x86_64-apple-darwin*/*.*/libgcc_eh.a\ * ]]
        fi
      else
        [[ "${lines[0]}" != *\ -l\ stdc++\ * ]]
        [[ "${lines[0]}" != *\ -l\ supc++\ * ]]
        [[ "${lines[0]}" != *\ *libstdc++.a\ * ]]
        [[ "${lines[0]}" != *\ *libsupc++.a\ * ]]
      fi
    fi
  }

  gstdcxx "o64-clang++" "-foc-use-gcc-libstdc++"
  gstdcxx "o64-clang" "-foc-use-gcc-libstdc++"
  gstdcxx "o64-clang++-gstdc++" "-foc-use-gcc-libstdc++"
  gstdcxx "o32-clang++-gstdc++" "-foc-use-gcc-libstdc++"
  OSXCROSS_GCC_NO_STATIC_RUNTIME=1 gstdcxx "o64-clang++" "-foc-use-gcc-libstdc++"
  OSXCROSS_GCC_NO_STATIC_RUNTIME=1 gstdcxx "o64-clang++" "-foc-use-gcc-libstdc++" "-m32"

  if [ $GCC_INSTALLED -eq 1 ]; then
    run o64-g++
    [ "$status" -eq 0 ]
    [[ "${lines[0]}" == *\ -static-libgcc\ * ]]
    [[ "${lines[0]}" == *\ -static-libstdc++\ * ]]

    OSXCROSS_GCC_NO_STATIC_RUNTIME=1 run o64-g++
    [[ "${lines[0]}" != *\ -static-libgcc\ * ]]
    [[ "${lines[0]}" != *\ -static-libstdc++\ * ]]
  fi

  run o64-clang++ -stdlib=default
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" != *\ -stdlib=default\ * ]]

  run o64-clang++ -stdlib=foo
  [ "$status" -eq 1 ]
  [[ "${lines[0]}" == *error:\ value\ of\ \'-stdlib=\'\ must\ be\ \'default\',\ \'libc++\'\ or\ \'libstdc++\' ]]

  if [ $(osxcross-cmp $OSXCROSS_SDK_VERSION "<" 10.7) -eq 1 ]; then
    run o64-clang++ -stdlib=libc++
    [ "$status" -ne 0 ]
    [[ "${lines[0]}" == *error:\ libc++\ requires\ Mac\ OS\ X\ SDK\ 10.7\ \(or\ later\) ]]
  fi
}

@test "precompiled headers" {
  function gch()
  {
    run $@
    [ "$status" -eq 0 ]
    [[ "${lines[0]}" != *\ -l* ]]
    [[ "${lines[0]}" != *\ -Wl,* ]]
    [[ "${lines[0]}" != *\ *.a* ]]
  }

  gch o64-clang -x c-header

  if [ $GCC_INSTALLED -eq 1 ]; then
    gch o64-clang++-gstdc++ -xc++-header
  fi

  if [ $GCC_INSTALLED -eq 1 ]; then
    gch o64-g++ -xc++-header
  fi
}

@test "-arch/-m32/-m64/-m16/-mx32" {
  run o64-clang++ -arch x86_64 -arch i386 -arch x86_64 -arch i386
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == *\ -arch\ x86_64\ \-arch\ i386\ * ]]

  run o64-clang++ -m16
  [ "$status" -eq 1 ]

  run o64-clang++ -mx32
  [ "$status" -eq 1 ]

  run o64-clang++ -arch x86_64h
  [ "$status" -eq 0 ] || [[ "${lines[0]}" == *error:\ \'x86_64\'\ requires\ Mac\ OS\ X\ SDK\ 10.8\ \(or\ later\) ]]

  run o64-clang++ -arch foo
  [ "$status" -ne 0 ]
  [[ "${lines[0]}" == *warning:\ \'-arch\':\ \unknown\ architecture\ \'foo\' ]]
  [[ "${lines[1]}" == *warning:\ \'-arch\':\ \'foo\'\ !=\ \'unknown\' ]]
  [[ "${lines[2]}" == *error:\ unsupported\ architecture:\ \'unknown\' ]]

  run o64-clang++ -arch armv7
  [ "$status" -ne 0 ]
  [[ "${lines[0]}" == *error:\ unsupported\ architecture:\ \'armv7\' ]]

  run o64-clang++ -arch i386
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" != *\ -arch\ x86_64\ * ]]

  run o64-clang++ -m32
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == *\ -arch\ i386\ * ]]

  if [ $(osxcross-cmp $OSXCROSS_SDK_VERSION '>=' 10.8) -eq 1 ]; then
    run o64-clang++ -arch x86_64h -mmacosx-version-min=10.7
    [ "$status" -eq 0 ]
    [[ "${lines[0]}" == *warning:\ \'-mmacosx-version-min=\'\ should\ be\ \'\>=\ 10.8\'\ for\ architecture\ \'x86_64h\' ]]

    OSXCROSS_NO_X86_64H_DEPLOYMENT_TARGET_WARNING=1 run o64-clang++ -arch x86_64h -mmacosx-version-min=10.7
    [ "$status" -eq 0 ]
    [[ "${lines[0]}" != *warning:\ * ]]
  fi

  if [ $GCC_INSTALLED -eq 1 ]; then
    run o64-g++ -arch x86_64 -arch i386 -arch x86_64 -arch i386
    [ "$status" -eq 0 ]
    [[ "${lines[0]}" == *\ -m32\ * ]]
    [[ "${lines[0]}" != *\ -m64\ * ]]
    [[ "${lines[0]}" != *\ -arch\ * ]]

    if [ $(osxcross-cmp $OSXCROSS_SDK_VERSION ">=" 10.8) -eq 1 ]; then
      run o64-g++ -arch x86_64h
      [ "$status" -eq 1 ]
      [[ "${lines[0]}" == *error:\ gcc\ does\ not\ support\ architecture\ \'x86_64h\' ]]
    fi
  fi
}

@test "bad include path warning" {
  if [ $(uname -s) == "Darwin" ]; then
    skip "Not supported"
  fi

  run o64-clang++ -I/usr/..////////usr//include
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == *warning:\ possibly\ dangerous\ include\ path\ specified:\ \'-I\ /usr/..////////usr//include\'\ \(/usr/include\) ]]
  [[ "${lines[1]}" == *info:\ you\ can\ silence\ this\ warning\ via\ \'OSXCROSS_NO_INCLUDE_PATH_WARNINGS=1\'\ \(env\)* ]]
  [[ "${lines[2]}" == *\ -I/usr/..////////usr//include\ * ]]

  if [ $GCC_INSTALLED -eq 1 ]; then
    OSXCROSS_NO_INCLUDE_PATH_WARNINGS=1 run o64-g++ -I/usr/include
    [ "$status" -eq 0 ]
    [[ "${lines[0]}" != *warning:\ * ]]
  fi
}

@test "OSXCROSS_MP_INC" {
  if [ -d "${OSXCROSS_TARGET_DIR}/macports" ]; then
    function macports()
    {
      run $@
      [ "$status" -eq 0 ]
      if [ -d "${OSXCROSS_TARGET_DIR}/macports/pkgs/opt/local/include" ]; then
        [[ "${lines[0]}" == *\ -isystem\ */macports/pkgs/opt/local/include\ * ]]
      fi
      if  [ -d "${OSXCROSS_TARGET_DIR}/macports/pkgs/opt/local/lib" ]; then
        [[ "${lines[0]}" == *\ -L\ */macports/pkgs/opt/local/lib\ * ]]
      fi
      if  [ -d "${OSXCROSS_TARGET_DIR}/macports/pkgs/opt/local/Library/Frameworks" ]; then
        [[ "${lines[0]}" == *\ -iframework\ */macports/pkgs/opt/local/Library/Frameworks\ * ]]
      fi

      if [[ $1 == *clang++* ]]; then
        [[ "${lines[0]}" == *\ -Qunused-arguments\ * ]]
      fi
    }

    OSXCROSS_MP_INC=1 macports o64-clang++

    if [ $GCC_INSTALLED -eq 1 ]; then
      OSXCROSS_MP_INC=1 macports o64-g++
    fi
  fi
}

@test "OSXCROSS_SDKROOT" {
  OSXCROSS_SDKROOT=/dev/null/MacOSX10.10.sdk run o64-clang++
  [ "$status" -ne 0 ]
  [[ "${lines[0]}" == *error:\ cannot\ find\ Mac\ OS\ X\ SDK\ \(expected\ in:\ /dev/null/MacOSX10.10.sdk\) ]]

  OSXCROSS_SDKROOT=$OSXCROSS_SDK run o64-clang++
  [ "$status" -eq 0 ]
}

@test "OSXCROSS_SDK_SEARCH_DIR" {
  OSXCROSS_SDK_SEARCH_DIR=/dev/null run o64-clang++
  [ "$status" -ne 0 ]
  [[ "${lines[0]}" == *error:\ no\ SDK\ found\ in\ \'/dev/null\' ]]

  OSXCROSS_SDK_SEARCH_DIR=$OSXCROSS_SDK/.. run o64-clang++
  [ "$status" -eq 0 ]
}

@test "xcrun" {
  run xcrun -sdk
  [ "$status" -ne 0 ]
  [[ "${lines[0]}" == *error:\ too\ few\ arguments\ for\ \'-sdk\' ]]

  run xcrun -sdk /
  [ "$status" -eq 0 ]

  run xcrun -sdk /dev/null
  [ "$status" -ne 0 ]
  [[ "${lines[0]}" == *error:\ xcrun:\ \'-sdk\':\ expected\ Mac\ OS\ X\ SDK ]]

  OSXCROSS_SDKROOT=/dev/null/MacOSX10.9.sdk SDKROOT=/dev/null/MacOSX10.10.sdk run xcrun
  [ "$status" -ne 0 ]
  [[ "${lines[0]}" == *error:\ xcrun:\ \'-sdk\':\ directory\ \'/dev/null/MacOSX10.10.sdk\'\ does\ not\ exist ]]

  run xcrun -sdk $OSXCROSS_SDK clang
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == *debug:\ \<--\ * ]]

  run xcrun -f clang
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == */bin/x86_64-apple-darwin*-clang ]]

  run xcrun -find /usr/bin/ld
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == */bin/x86_64-apple-darwin*-ld ]]

  run xcrun -show-sdk-path -show-sdk-version
  [ "$status" -eq 0 ]
  [ "${lines[0]}" == "$OSXCROSS_SDK" ]
  [ "${lines[1]}" == "$OSXCROSS_SDK_VERSION" ]

  xcrun_log=1 run xcrun clang
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == */bin/x86_64-apple-darwin*-clang ]]

  run xcrun -log -l clang
  [ "$status" -eq 0 ]
  [[ "${lines[1]}" == *debug:\ \<--\ * ]]

  run xcrun -run clang
  [ "$status" -eq 0 ]

  run xcrun -r clang
  [ "$status" -eq 0 ]

  run xcrun -foo
  [ "$status" -ne 0 ]

  run xcrun -k -kill-cache -n -no-cache -toolchain / -v -verbose -h -help
  [ "$status" -eq 0 ]
}

@test "sw_vers" {
  run sw_vers
  [ "$status" -eq 0 ]
  [ "${lines[0]}"  == "ProductName:    Mac OS X" ]
  [ "${lines[1]}"  == "ProductVersion: $OSXCROSS_OSX_VERSION_MIN" ]
  [[ "${lines[2]}" ==  BuildVersion:\ \ \ * ]]

  run sw_vers -productName
  [ "$status" -eq 0 ]
  [ "${lines[0]}"  == "Mac OS X" ]

  OSXCROSS_SW_VERS_OSX_VERSION=10.15 MACOSX_DEPLOYMENT_TARGET=10.16 run sw_vers -productVersion
  [ "$status" -eq 0 ]
  [ "${lines[0]}"  == "10.15" ]

  MACOSX_DEPLOYMENT_TARGET=10.16 run sw_vers -productVersion
  [ "$status" -eq 0 ]
  [ "${lines[0]}"  == "10.16" ]

  run sw_vers -buildVersion
  [ "$status" -eq 0 ]
  [[ "${lines[0]}" == * ]]

  run sw_vers -foo
  [ "$status" -ne 0 ]
}
