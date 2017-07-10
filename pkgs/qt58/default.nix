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

  base-raw = crossenv.make_derivation rec {
    name = "qtbase-raw-${version}";

    src = crossenv.nixpkgs.fetchurl {
      url = https://download.qt.io/official_releases/qt/5.8/5.8.0/submodules/qtbase-opensource-src-5.8.0.tar.xz;
      sha256 = "01f07yjly7y24njl2h4hyknmi7pf8yd9gky23szcfkd40ap12wf1";
    };

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

    inherit version;

    inherit (crossenv) host;

    builder = ./builder.sh;

    configure_flags =
      "-opensource -confirm-license " +
      "-xplatform mingw-w64-g++ " +
      "-device-option CROSS_COMPILE=${host}- " +
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
  base = crossenv.make_derivation rec {
    name = "qtbase-${version}";

    inherit version;

    builder.builder = "${crossenv.nixpkgs.ruby}/bin/ruby";
    builder.args = [./wrapper_builder.rb];

    qtbase = base-raw;
  };

  base-examples = crossenv.make_derivation rec {
    name = "qtbase-examples-${version}";

    inherit version;

    inherit (base-raw) src;

    qtbase = base;

    builder = ./examples_builder.sh;
  };
in
{
  recurseForDerivations = true;
  inherit base-raw;
  inherit base;
  inherit base-examples;
}
