{ crossenv }:

let
  version = "5.8.0";

  # TODO: patch qt to not use /bin/pwd, test building it in a sandbox

  base-raw = crossenv.make_derivation rec {
    name = "qtbase-raw-${version}";

    src = crossenv.nixpkgs.fetchurl {
      url = https://download.qt.io/official_releases/qt/5.8/5.8.0/submodules/qtbase-opensource-src-5.8.0.tar.xz;
      sha256 = "01f07yjly7y24njl2h4hyknmi7pf8yd9gky23szcfkd40ap12wf1";
    };

    patches = [
      ./qtbase-arch-patches/0001-Add-profile-for-cross-compilation-with-mingw-w64.patch
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

    inherit (base) src;

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
