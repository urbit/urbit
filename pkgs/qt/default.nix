# TODO: look into why we're compiling with this impure option on Linux:
#   -DDFLT_XKB_CONFIG_ROOT=\"/usr/share/X11/xkb\"

# TODO: patch qt to not use /bin/pwd, test building it in a sandbox

# TODO: Try to get 'Q_IMPORT_PLUGIN (QMacStylePlugin);' working.  I think the
# plugin was not compiled at all.  Figure out why.  (Though at Pololu we
# do not use this style, we use Fusion on macOS and Linux.)

# TODO: remove third-party X libraries from Qt sources to make sure they aren't
# used, and don't include their licenses (e.g. xkbcommon).

{ crossenv, libudev, libxall, at-spi2-headers, dejavu-fonts }:

let
  version = "5.12.4";

  name = "qtbase-${version}";

  platform =
    let
      os_code =
        if crossenv.os == "windows" then "win32"
        else if crossenv.os == "macos" then "macx"
        else if crossenv.os == "linux" then "devices/linux-generic"
        else crossenv.os;
      compiler_code =
        if crossenv.compiler == "gcc" then "g++"
        else crossenv.compiler;
    in "${os_code}-${compiler_code}";

  base_src = crossenv.nixpkgs.fetchurl {
    url = "https://download.qt.io/official_releases/qt/5.12/${version}/submodules/qtbase-everywhere-src-${version}.tar.xz";
    sha256 = "158i0apc3i8bbgrk9j1k34y2b03v0kwwv8m7aaaxpxsglppwgyr0";
  };

  base_raw = crossenv.make_derivation {
    name = "qtbase-raw-${version}";
    inherit version;
    src = base_src;
    builder = ./builder.sh;

    patches = [
      # Purity issue: Don't look at the build system using absolute paths.
      ./absolute-paths.patch

      # macOS configuration: Don't run tools from /usr/bin, use the right
      # compiler, and don't pass redundant options to it (-arch, -isysroot,
      # -mmacosx-version-min).
      ./macos-config.patch

      # Qt uses X11/cursorfont.h, which is from libx11, but it does not
      # search for it properly.
      ./find-x11.patch

      # Fix the build error caused by https://bugreports.qt.io/browse/QTBUG-63637
      ./win32-link-object-max.patch

      # The .pc files have incorrect library names without this (e.g. Qt5Cored)
      ./pc-debug-name.patch

      # uxtheme.h test is broken, always returns false, and results in QtWidgets
      # apps looking bad on Windows.  https://stackoverflow.com/q/44784414/28128
      ./dont-test-uxtheme.patch

      # When cross-compiling, Qt uses some heuristics about whether to trust the
      # pkg-config executable supplied by the PKG_CONFIG environment variable.
      # These heuristics are wrong for us, so disable them, making qt use
      # pkg-config-cross.
      ./pkg-config-cross.patch

      # When the DBus session bus is not available, Qt tries to dereference a
      # null pointer, so Linux applications can't start up.
      ./dbus-null-pointer.patch

      # Look for fonts in the same directory as the application by default if
      # the QT_QPA_FONTDIR environment variable is not present.  Without this
      # patch, Qt tries to look for a font directory in the nix store that does
      # not exist, and prints warnings.
      # You must ship a .ttf, .ttc, .pfa, .pfb, or .otf font file
      # with your application (e.g. https://dejavu-fonts.github.io/ ).
      # That list of extensions comes from qbasicfontdatabase.cpp.
      ./font-dir.patch
    ];

    configure_flags =
      "-opensource -confirm-license " +
      "-xplatform ${platform} " +
      "-device-option CROSS_COMPILE=${crossenv.host}- " +
      "-release " +  # change to -debug if you want debugging symbols
      "-static " +
      "-pkg-config " +
      "-nomake examples " +
      "-no-icu " +
      "-no-fontconfig " +
      "-no-reduce-relocations " +
      ( if crossenv.os == "windows" then
          "-opengl desktop"
        else if crossenv.os == "linux" then
          "-qpa xcb " +
          "-system-xcb " +
          "-no-opengl "       # TODO: support OpenGL in Linux
        else if crossenv.os == "macos" then
          "-device-option QMAKE_MAC_SDK.macosx.Path=" +
            "${crossenv.sdk} " +
          "-device-option QMAKE_MAC_SDK.macosx.PlatformPath=" +
            "${crossenv.sdk}/does-not-exist " +
          "-device-option QMAKE_MAC_SDK.macosx.SDKVersion=${crossenv.sdk.version} " +
          "-device-option QMAKE_XCODE_VERSION=10.0"
        else "" );

     cross_inputs =
       if crossenv.os == "linux" then [
           libudev  # not sure if this helps, but Qt does look for it
           libxall
           at-spi2-headers  # for accessibility
         ]
       else [];
  };

  # This wrapper aims to make Qt easier to use by generating CMake package files
  # for it.  The existing support for CMake in Qt does not handle static
  # linking; other projects maintian large, messy patches to fix it, but we
  # prefer to generate the CMake files in a clean way from scratch.
  base = crossenv.make_derivation {
    inherit version name;
    os = crossenv.os;
    qtbase = base_raw;
    cross_inputs = base_raw.cross_inputs;
    builder.ruby = ./wrapper_builder.rb;
    core_macros = ./core_macros.cmake;
  };

  examples = crossenv.make_derivation {
    name = "qtbase-examples-${version}";
    inherit version;
    os = crossenv.os;
    qtbase = base;
    cross_inputs = [ base ];
    dejavu = dejavu-fonts;
    builder = ./examples_builder.sh;
  };

  license_fragment = crossenv.native.make_derivation {
    name = "qtbase-${version}-license-fragment";
    inherit version;
    src = base_src;
    builder = ./license_builder.sh;
  };

  license_set =
    (
      if crossenv.os == "linux" then
        libudev.license_set //
        libxall.license_set //
        at-spi2-headers.license_set
      else
        {}
    ) //
    { "${name}" = license_fragment; };
in
  base // {
    recurseForDerivations = true;
    inherit base_src;
    inherit base_raw;
    inherit base;
    inherit examples;
    inherit license_set;
  }
