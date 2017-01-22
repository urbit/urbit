source $stdenv/setup

export NIX_FIXINC_DUMMY=$NIX_BUILD_TOP/dummy
mkdir $NIX_FIXINC_DUMMY

EXTRA_LDFLAGS="-Wl,-rpath,$lib/lib"
EXTRA_FLAGS="-O2 -g"

# CFLAGS_FOR_TARGET are needed for the libstdc++ configure script to find
# the startfiles.
# FLAGS_FOR_TARGET are needed for the target libraries to receive the -Bxxx
# for the startfiles.
makeFlagsArray+=( \
  CFLAGS_FOR_BUILD="$EXTRA_FLAGS $EXTRA_LDFLAGS" \
  CXXFLAGS_FOR_BUILD="$EXTRA_FLAGS $EXTRA_LDFLAGS" \
  CFLAGS_FOR_TARGET="$EXTRA_TARGET_CFLAGS $EXTRA_TARGET_LDFLAGS" \
  CXXFLAGS_FOR_TARGET="$EXTRA_TARGET_CFLAGS $EXTRA_TARGET_LDFLAGS" \
  FLAGS_FOR_TARGET="$EXTRA_TARGET_CFLAGS $EXTRA_TARGET_LDFLAGS" \
  LDFLAGS_FOR_BUILD="$EXTRA_FLAGS $EXTRA_LDFLAGS" \
  LDFLAGS_FOR_TARGET="$EXTRA_TARGET_LDFLAGS $EXTRA_TARGET_LDFLAGS" \
  )

providedPreConfigure="$preConfigure";
preConfigure() {
    if test -n "$newlibSrc"; then
        tar xvf "$newlibSrc" -C ..
        ln -s ../newlib-*/newlib newlib
        # Patch to get armvt5el working:
        sed -i -e 's/ arm)/ arm*)/' newlib/configure.host
    fi

    # Bug - they packaged zlib
    if test -d "zlib"; then
        # This breaks the build without-headers, which should build only
        # the target libgcc as target libraries.
        # See 'configure:5370'
        rm -Rf zlib
    fi

    if test -f "$NIX_CC/nix-support/orig-libc"; then
        # Patch the configure script so it finds glibc headers.  It's
        # important for example in order not to get libssp built,
        # because its functionality is in glibc already.
        sed -i \
            -e "s,glibc_header_dir=/usr/include,glibc_header_dir=$libc_dev/include", \
            gcc/configure
    fi

    # Eval the preConfigure script from nix expression.
    eval "$providedPreConfigure"

    # Perform the build in a different directory.
    mkdir ../build
    cd ../build
    configureScript=../$sourceRoot/configure
}


postConfigure() {
    # Don't store the configure flags in the resulting executables.
    sed -e '/TOPLEVEL_CONFIGURE_ARGUMENTS=/d' -i Makefile
}

postInstall() {
    # Move runtime libraries to $lib.
    moveToOutput "lib/lib*.so*" "$lib"
    moveToOutput "lib/lib*.la"  "$lib"
    moveToOutput "share/gcc-*/python" "$lib"

    for i in "$lib"/lib/*.{la,py}; do
        substituteInPlace "$i" --replace "$out" "$lib"
    done

    if [ -n "$enableMultilib" ]; then
        moveToOutput "lib64/lib*.so*" "$lib"
        moveToOutput "lib64/lib*.la"  "$lib"

        for i in "$lib"/lib64/*.{la,py}; do
            substituteInPlace "$i" --replace "$out" "$lib"
        done
    fi

    # Remove `fixincl' to prevent a retained dependency on the
    # previous gcc.
    rm -rf $out/libexec/gcc/*/*/install-tools
    rm -rf $out/lib/gcc/*/*/install-tools

    # More dependencies with the previous gcc or some libs (gccbug stores the build command line)
    rm -rf $out/bin/gccbug

    if type "patchelf"; then
        # Take out the bootstrap-tools from the rpath, as it's not needed at all having $out
        for i in $(find "$out"/libexec/gcc/*/*/* -type f -a \! -name '*.la'); do
            PREV_RPATH=`patchelf --print-rpath "$i"`
            NEW_RPATH=`echo "$PREV_RPATH" | sed 's,:[^:]*bootstrap-tools/lib,,g'`
            patchelf --set-rpath "$NEW_RPATH" "$i" && echo OK
        done

        # For some reason the libs retain RPATH to $out
        for i in "$lib"/lib/{libtsan,libasan,libubsan}.so.*.*.*; do
            PREV_RPATH=`patchelf --print-rpath "$i"`
            NEW_RPATH=`echo "$PREV_RPATH" | sed "s,:${out}[^:]*,,g"`
            patchelf --set-rpath "$NEW_RPATH" "$i" && echo OK
        done
    fi

    # Get rid of some "fixed" header files
    rm -rfv $out/lib/gcc/*/*/include-fixed/{root,linux}

    # Replace hard links for i686-pc-linux-gnu-gcc etc. with symlinks.
    for i in $out/bin/*-gcc*; do
        if cmp -s $out/bin/gcc $i; then
            ln -sfn gcc $i
        fi
    done

    for i in $out/bin/c++ $out/bin/*-c++* $out/bin/*-g++*; do
        if cmp -s $out/bin/g++ $i; then
            ln -sfn g++ $i
        fi
    done

    # Disable RANDMMAP on grsec, which causes segfaults when using
    # precompiled headers.
    # See https://bugs.gentoo.org/show_bug.cgi?id=301299#c31
    paxmark r $out/libexec/gcc/*/*/{cc1,cc1plus}

    eval "$postInstallGhdl"
}

genericBuild

# Make the lib output in case it does not exist
mkdir -p $lib/lib
