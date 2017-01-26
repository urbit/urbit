source $stdenv/setup

providedPreConfigure="$preConfigure";
preConfigure() {
  mkdir ../build
  cd ../build
  configureScript=../$sourceRoot/configure
}

postInstall() {
  # Remove "install-tools" so we don't have a reference to bash.
  rm -r "$out/libexec/gcc/$target/$version/install-tools/"

  # Disable RANDMMAP on grsec, which causes segfaults when using
  # precompiled headers.
  # See https://bugs.gentoo.org/show_bug.cgi?id=301299#c31
  paxmark r $out/libexec/gcc/*/*/{cc1,cc1plus}
}

genericBuild
