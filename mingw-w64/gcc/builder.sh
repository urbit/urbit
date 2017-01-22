source $stdenv/setup

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
  mkdir ../build
  cd ../build
  configureScript=../$sourceRoot/configure
}

postConfigure() {
  # Don't store the configure flags in the resulting executables.
  sed -e '/TOPLEVEL_CONFIGURE_ARGUMENTS=/d' -i Makefile
}

postInstall() {
    find > $out/tmphax_file_list

    # Remove "fixincl" to prevent a retained dependency on the
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
