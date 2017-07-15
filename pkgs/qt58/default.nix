{ crossenv }:

let
  version = "5.8.0";

  # TODO: for people doing incremental builds with this package, try
  # to make the link step faster and not produce such a huge (400 MB)
  # executable.  Maybe strip the static libraries in the output or
  # disable the generation of debug symbols by GCC in the first place.
  # Qt has built-in support for compiling both debug and release
  # libraries and installing them side by side, which we could try.

  # TODO: patch qt to not use /bin/pwd, test building it in a sandbox

  base_src = crossenv.nixpkgs.fetchurl {
    url = https://download.qt.io/official_releases/qt/5.8/5.8.0/submodules/qtbase-opensource-src-5.8.0.tar.xz;
    sha256 = "01f07yjly7y24njl2h4hyknmi7pf8yd9gky23szcfkd40ap12wf1";
  };

  base_raw = crossenv.make_derivation {
    name = "qtbase-raw-${version}";
    inherit version;
    src = base_src;
    builder = ./builder.sh;

    patches = [
      # Settings we need to cross compile.
      ./qtbase-arch-patches/0001-Add-profile-for-cross-compilation-with-mingw-w64.patch

      # The .pc files have incorrect library names without this (e.g. Qt5Cored)
      ./qtbase-arch-patches/0007-Prevent-debug-library-names-in-pkg-config-files.patch

      # An #include statement in Qt contained uppercase letters, but
      # mingw-w64 headers are all lowercase.
      ./qtbase-arch-patches/0028-Include-uiviewsettingsinterop.h-correctly.patch

      # uxtheme.h test is broken, always returns false, and results in QtWidgets
      # apps looking bad on Windows.  https://stackoverflow.com/q/44784414/28128
      ./dont-test-uxtheme.patch
    ];

    configure_flags =
      "-opensource -confirm-license " +
      "-xplatform mingw-w64-g++ " +
      "-device-option CROSS_COMPILE=${crossenv.host}- " +
      "-release " +
      "-static " +
      "-nomake examples " +
      "-opengl desktop " +
      "-no-icu";
  };

  # This wrapper aims to make Qt easier to use by generating CMake package files
  # for it.  The existing support for CMake in Qt does not handle static
  # linking; other projects maintian large, messy patches to fix it, but we
  # prefer to generate the CMake files in a clean way from scratch.
  base = crossenv.make_derivation {
    name = "qtbase-${version}";
    inherit version;
    qtbase = base_raw;
    builder.builder = "${crossenv.nixpkgs.ruby}/bin/ruby";
    builder.args = [./wrapper_builder.rb];
  };

  base_examples = crossenv.make_derivation {
    name = "qtbase-examples-${version}";
    inherit version;
    src = base_src;
    qtbase = base;
    builder = ./examples_builder.sh;
  };

  base_license_fragment = crossenv.make_native_derivation {
    name = "qtbase-${version}-license-fragment";
    inherit version;
    src = base_src;
    builder = ./license_builder.sh;
  };
in
base // {
  recurseForDerivations = true;
  inherit base_src;
  inherit base_raw;
  inherit base;
  inherit base_examples;
  inherit base_license_fragment;
}
