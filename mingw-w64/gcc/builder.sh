source $stdenv/setup

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
  # Remove "install-tools" so we don't have a reference to bash.
  rm -r "$out/libexec/gcc/$target/$version/install-tools/"

    # Get rid of some "fixed" header files
    # rm -rfv $out/lib/gcc/*/*/include-fixed/{root,linux}

    # Replace hard links with symlinks.
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
