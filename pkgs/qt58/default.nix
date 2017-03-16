{ crossenv }:

let
  version = "5.8.0";

  patches = [
    # ./msys2-patches/0001-Add-link-time-optimization-support-for-Clang-GCC-and.patch
    # ./msys2-patches/0001-qt-5.8.0-fix-sql-libraries-mingw.patch
    # ./msys2-patches/0002-qt-5.8.0-configure-gcc-before-clang.patch
    # ./msys2-patches/0003-qt-5.8.0-autodetect-fontconfig.patch
    # ./msys2-patches/0005-qt-5.3.0-syncqt-fix.patch
    # ./msys2-patches/0006-qt-5.3.0-win_flex-replace.patch
    # ./msys2-patches/0007-qt-5.3.0-win32-g-Enable-static-builds.patch
    # ./msys2-patches/0008-qt-5.3.0-win32-g-Add-QMAKE_EXTENSION_IMPORTLIB-defaulting-to-.patch
    # ./msys2-patches/0009-qt-5.3.0-qmlimportscanner-Ensure-the-correct-variant-is-run.patch
    # ./msys2-patches/0010-qt-5.3.0-qdoc-increase-stack-size-for-win32-g-too.patch
    # ./msys2-patches/0011-qt-5.8.0-mingw-dbus-and-pkg-config.patch
    # ./msys2-patches/0016-qt-5.8.0-win32-g++-use-qpa-genericunixfontdatabase.patch
    # ./msys2-patches/0017-qt-5.3.0-fix-examples-building.patch
    # ./msys2-patches/0020-qt-5.3.0-use-external-angle-library.patch
    # ./msys2-patches/0023-qt-5.3.0-env-set-external-angle.patch
    # ./msys2-patches/0024-qt-5.3.0-icu-add-U_LIB_SUFFIX_C_NAME.patch
    # ./msys2-patches/0025-qt-5.8.0-force-using-make-on-msys.patch
    # ./msys2-patches/0028-qt-5.8.0-Revert-untangle-use-of-system-vs.-shell-path-list-se.patch
    # ./msys2-patches/0029-qt-5.8.0-Revert-fix-quoting-and-path-separators-in-qtPrepareT.patch
    # ./msys2-patches/0030-qt-5.3.1-workaround-ansidecl-h-PTR-define-conflict.patch
    # ./msys2-patches/0031-qt-5.5.0-workaround-BOOL-define-conflict.patch
    # ./msys2-patches/0034-qt-5.3.2-Use-QMAKE_PREFIX_STATICLIB-in-create_cmake-prf.patch
    # ./msys2-patches/0035-qt-5.3.2-dont-add-resource-files-to-qmake-libs.patch
    # ./msys2-patches/0036-qt-5.3.2-win32-qt5-static-cmake-link-ws2_32-and--static.patch
    # ./msys2-patches/0037-qt-5.4.0-Improve-cmake-plugin-detection-as-not-all-are-suffixed-Plugin.patch
    # ./msys2-patches/0038-qt-5.5.0-cmake-Rearrange-STATIC-vs-INTERFACE-targets.patch
    # ./msys2-patches/0039-qt-5.4.0-Make-it-possible-to-use-static-builds-of-Qt-with-CMa.patch
    # ./msys2-patches/0040-qt-5.4.0-Generate-separated-libraries-in-prl-files-for-CMake.patch
    # ./msys2-patches/0041-qt-5.4.0-Fix-mingw-create_cmake-prl-file-has-no-lib-prefix.patch
    # ./msys2-patches/0042-qt-5.4.0-static-cmake-also-link-plugins-and-plugin-deps.patch
    # ./msys2-patches/0043-qt-5.5.0-static-cmake-regex-QT_INSTALL_LIBS-in-QMAKE_PRL_LIBS_FOR_CMAKE.patch
    # ./msys2-patches/0044-qt-5.4.0-win32-g++-enable-qtwebengine-build.patch
    # ./msys2-patches/0045-qt-5.4.1-static-use-qminimal-platform-plugin-for-qcollectiongenerator.patch
    # ./msys2-patches/0046-qt-5.4.1-Revert-Revert-fix-NTFS-mount-points.patch
    # ./msys2-patches/0048-qt-5.4.2-win32-Avoid-platformNativeInterface-segfaults-with-minimal-platform.patch
    # ./msys2-patches/0049-windows-not-use-fontconfig.patch
    # ./msys2-patches/0050-qt-5.5.1-fix-the-trailing-backslash-in-DESTDIR-on-mingw-worka.patch
    # ./msys2-patches/0125-qt5-windeployqt-fixes.patch
    ./msys2-patches/0300-qt-5.8.0-cast-errors.patch
    # ./msys2-patches/qt-5.3.0-static-qmake-conf.patch
  ];

  # TODO: patch qt to not use /bin/pwd, test building it in a sandbox

  base = crossenv.nixpkgs.stdenv.mkDerivation rec {
    name = "qt-${version}-${crossenv.host}";

    src = crossenv.nixpkgs.fetchurl {
      url = https://download.qt.io/official_releases/qt/5.8/5.8.0/submodules/qtbase-opensource-src-5.8.0.tar.xz;
      sha256 = "01f07yjly7y24njl2h4hyknmi7pf8yd9gky23szcfkd40ap12wf1";
    };

    patches = [
      ./qtbase-patches/add-profile.patch

      # We can get rid of this patch once mingw-w64 is fixed.
      ./qtbase-patches/direct2d-const-error.patch

      ./qtbase-arch-patches/0002-Ensure-GLdouble-is-defined-when-using-dynamic-OpenGL.patch
      ./qtbase-arch-patches/0003-Use-external-ANGLE-library.patch
      ./qtbase-arch-patches/0004-Fix-too-many-sections-assemler-error-in-OpenGL-facto.patch
      ./qtbase-arch-patches/0005-Make-sure-.pc-files-are-installed-correctly.patch
      ./qtbase-arch-patches/0006-Don-t-add-resource-files-to-LIBS-parameter.patch
      ./qtbase-arch-patches/0007-Prevent-debug-library-names-in-pkg-config-files.patch
      ./qtbase-arch-patches/0008-Fix-linking-against-shared-static-libpng.patch
      ./qtbase-arch-patches/0009-Fix-linking-against-static-D-Bus.patch
      ./qtbase-arch-patches/0010-Fix-linking-against-static-freetype2.patch
      ./qtbase-arch-patches/0011-Fix-linking-against-static-harfbuzz.patch
      ./qtbase-arch-patches/0012-Fix-linking-against-static-pcre.patch
      ./qtbase-arch-patches/0013-Fix-linking-against-shared-static-MariaDB.patch
      ./qtbase-arch-patches/0014-Fix-linking-against-shared-static-PostgreSQL.patch
      ./qtbase-arch-patches/0015-Rename-qtmain-to-qt5main.patch
      ./qtbase-arch-patches/0016-Build-dynamic-host-libraries.patch
      ./qtbase-arch-patches/0017-Enable-rpath-for-build-tools.patch
      ./qtbase-arch-patches/0018-Use-system-zlib-for-build-tools.patch
      ./qtbase-arch-patches/0019-Disable-determing-default-include-and-lib-dirs-at-qm.patch
      ./qtbase-arch-patches/0020-Use-.dll.a-as-import-lib-extension.patch
      ./qtbase-arch-patches/0021-Merge-shared-and-static-library-trees.patch
      ./qtbase-arch-patches/0022-Allow-usage-of-static-version-with-CMake.patch
      ./qtbase-arch-patches/0023-Use-correct-pkg-config-static-flag.patch
      ./qtbase-arch-patches/0024-Fix-qt5_wrap_ui-macro.patch
      ./qtbase-arch-patches/0025-Ignore-errors-about-missing-feature-static.patch
      # ./qtbase-arch-patches/0026-Enable-anf-fix-use-of-iconv.patch
      ./qtbase-arch-patches/0027-Ignore-failing-pkg-config-test.patch
      ./qtbase-arch-patches/0028-Include-uiviewsettingsinterop.h-correctly.patch
      ./qtbase-arch-patches/0029-Hardcode-linker-flags-for-libqwindows.dll.patch
      ./qtbase-arch-patches/0030-Prevent-qmake-from-messing-static-lib-dependencies.patch
    ];

    inherit version;

    inherit (crossenv) host;

    buildInputs = [ crossenv.gcc crossenv.binutils ];

    builder = ./builder.sh;
  };
in
{
  recurseForDerivations = true;
  inherit base;
}
