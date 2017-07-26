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

  platform =
    let
      os_code =
        if crossenv.os == "windows" then "win32"
        else if crossenv.os == "macos" then "macx"
        else crossenv.os;
      compiler_code =
        if crossenv.compiler == "gcc" then "g++"
        else crossenv.compiler;
    in "${os_code}-${compiler_code}";

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
      # The .pc files have incorrect library names without this (e.g. Qt5Cored)
      ./pc-debug-name.patch

      # An #include statement in Qt contained uppercase letters, but
      # mingw-w64 headers are all lowercase.
      ./header-caps.patch

      # uxtheme.h test is broken, always returns false, and results in QtWidgets
      # apps looking bad on Windows.  https://stackoverflow.com/q/44784414/28128
      ./dont-test-uxtheme.patch
    ];

    configure_flags =
      "-opensource -confirm-license " +
      "-xplatform ${platform} " +
      "-device-option CROSS_COMPILE=${crossenv.host}- " +
      "-device-option PKG_CONFIG=pkg-config-cross " +
      "-device-option QMAKE_PKG_CONFIG=pkg-config-cross " +
      "-release " +
      "-static " +
      "-nomake examples " +
      "-no-icu " +
      ( if crossenv.os == "windows" then "-opengl desktop"
        else "");
  };

  # This wrapper aims to make Qt easier to use by generating CMake package files
  # for it.  The existing support for CMake in Qt does not handle static
  # linking; other projects maintian large, messy patches to fix it, but we
  # prefer to generate the CMake files in a clean way from scratch.
  base = crossenv.make_derivation {
    name = "qtbase-${version}";
    inherit version;
    qtbase = base_raw;
    builder.ruby = ./wrapper_builder.rb;
  };

  examples = crossenv.make_derivation {
    name = "qtbase-examples-${version}";
    inherit version;
    src = base_src;
    qtbase = base;
    builder = ./examples_builder.sh;
  };

  license_fragment = crossenv.make_native_derivation {
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
  inherit examples;
  inherit license_fragment;
}
