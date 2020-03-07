# Note: To reduce clutter here, it might be nice to move clang to
# `native`, and also make `native` provide a function for building
# binutils.  So clang and binutils recipes could be shared by the
# different platforms we targets.

{ osx_sdk, native }:
let
  nixpkgs = native.nixpkgs;

  arch = "x86_64";

  # was darwin15, changed to darwin so that lld guesses flavor=Darwin correctly
  darwin_name = "darwin15";

  # Qt 5.12 expects macOS 10.12 or later
  # (see macx.conf and http://doc.qt.io/qt-5/macos.html).
  macos_version_min = "10.12";

  host = "${arch}-apple-${darwin_name}";

  clang = native.make_derivation rec {
    name = "clang-${version}";

    version = "7.0.1";

    src = nixpkgs.fetchurl {
      url = "http://releases.llvm.org/${version}/cfe-${version}.src.tar.xz";
      sha256 = "067lwggnbg0w1dfrps790r5l6k8n5zwhlsw7zb6zvmfpwpfn4nx4";
    };

    llvm_src = nixpkgs.fetchurl {
      url = "http://releases.llvm.org/${version}/llvm-${version}.src.tar.xz";
      sha256 = "16s196wqzdw4pmri15hadzqgdi926zln3an2viwyq0kini6zr3d3";
    };

    # Note: We aren't actually using lld for anything yet.
    lld_src = nixpkgs.fetchurl {
      url = "http://releases.llvm.org/${version}/lld-${version}.src.tar.xz";
      sha256 = "0ca0qygrk87lhjk6cpv1wbmdfnficqqjsda3k7b013idvnralsc8";
    };

    patches = [ ./clang.patch ];

    builder = ./clang_builder.sh;

    native_inputs = [ nixpkgs.python2 ];

    cmake_flags =
      "-DCMAKE_BUILD_TYPE=Release " +
      # "-DCMAKE_BUILD_TYPE=Debug " +
      "-DLLVM_TARGETS_TO_BUILD=X86\;ARM " +
      "-DLLVM_ENABLE_ASSERTIONS=OFF";
  };

  # Previously we used a port of Apple's TAPI library from here:
  #   https://github.com/DavidEGrayson/tapi/archive/f98d0c3.tar.gz
  # Another version of that libtrary is here:
  #   https://github.com/tpoechtrager/apple-libtapi
  # But those versions require LLVM and clang 5.0.0, so now we are using
  # tinytapi instead.
  tapi = native.make_derivation rec {
    name = "tinytapi-${version}";

    version = "1.0.0";

    src = nixpkgs.fetchurl {
      url = "https://github.com/DavidEGrayson/tinytapi/archive/${version}.tar.gz";
      sha256 = "1hipsnpnlmrg63q7c7mx07dgc7sn86agf3jh1gys1rri0cdn1w1b";
    };

    builder = ./tinytapi_builder.sh;
    libyaml = nixpkgs.libyaml;
    native_inputs = [ libyaml ];
  };

  cctools_commit = "c1cc758";
  cctools_apple_version = "274.2";  # from README.md
  cctools_port_src = nixpkgs.fetchurl {
    url = "https://github.com/tpoechtrager/cctools-port/archive/${cctools_commit}.tar.gz";
    sha256= "11bfcndzbdmjp2piabyqs34da617fh5fhirqvb9w87anfan15ffa";
  };

  ld = native.make_derivation rec {
    name = "cctools-ld64";
    apple_version = cctools_apple_version;
    src = cctools_port_src;
    patches = [
      ./cctools-format.patch
      ./cctools-ld64-registers.patch
    ];
    builder = ./ld_builder.sh;
    native_inputs = [ tapi ];
    inherit host;
  };

  ranlib = native.make_derivation rec {
    name = "cctools-ranlib";
    apple_version = cctools_apple_version;
    src = ld.src;
    builder = ./ranlib_builder.sh;
    patches = [
      ./cctools-format.patch
      ./cctools-bytesex.patch
    ];
    inherit host;
  };

  ar = native.make_derivation rec {
    name = "cctools-ar";
    apple_version = cctools_apple_version;
    src = cctools_port_src;
    builder = ./ar_builder.sh;
    patches = [
      ./cctools-format.patch
      ./cctools-libstuff-no-error.patch
    ];
    inherit host ranlib;
  };

  lipo = native.make_derivation rec {
    name = "cctools-lipo";
    apple_version = cctools_apple_version;
    src = cctools_port_src;
    builder = ./lipo_builder.sh;
    patches = [
      ./cctools-format.patch
    ];
    inherit host;
  };

  strip = native.make_derivation rec {
    name = "cctools-strip";
    apple_version = cctools_apple_version;
    src = cctools_port_src;
    builder = ./strip_builder.sh;
    patches = [
      ./cctools-format.patch
    ];
    inherit host;
  };

  sdk = native.make_derivation rec {
    name = "macos-sdk";
    builder = ./sdk_builder.sh;
    src = osx_sdk;
    native_inputs = [ nixpkgs.ruby ];
  } // {
    version = builtins.readFile "${sdk}/version.txt";
  };

  # Note: compiler-rt actually builds itself for three different architectures:
  # i386, x86_64, x86_64h.  It uses lipo to create fat archives that hold
  # binaries for all the different architectures.  We only want x86_64 but it's
  # not obvious how to achieve that.
  compiler_rt = native.make_derivation rec {
    name = "compiler-rt-${version}";

    version = clang.version;

    src = nixpkgs.fetchurl {
      url = "http://releases.llvm.org/7.0.1/compiler-rt-${version}.src.tar.xz";
      sha256 = "065ybd8fsc4h2hikbdyricj6pyv4r7r7kpcikhb2y5zf370xybkq";
    };

    builder = ./compiler_rt_builder.sh;

    patches = [ ./compiler_rt.patch ];

    native_inputs = [ clang ar ranlib lipo ld nixpkgs.python2 ];

    _cflags = "-target ${host} --sysroot ${sdk} " +
      "-I${sdk}/usr/include -mlinker-version=${ld.apple_version}";
    CC = "clang ${_cflags}";
    CXX = "clang++ ${_cflags} -stdlib=libc++ -cxx-isystem ${sdk}/usr/include/c++";

    cmake_flags =
      "-DCMAKE_BUILD_TYPE=Release " +
      "-DCMAKE_SYSTEM_NAME=Darwin " +
      "-DCMAKE_OSX_SYSROOT=${sdk} " +
      "-DDARWIN_osx_SYSROOT=${sdk} " +
      "-DCMAKE_LINKER=${ld}/bin/${host}-ld " +
      "-DCMAKE_AR=${ar}/bin/${host}-ar " +
      "-DCMAKE_RANLIB=${ranlib}/bin/${host}-ranlib " +
      "-DCOMPILER_RT_BUILD_XRAY=OFF";

    inherit host sdk;
  };

  toolchain = native.make_derivation rec {
    name = "macos-toolchain";
    builder = ./toolchain_builder.sh;
    src_file = ./wrapper.cpp;
    inherit host clang ld ranlib ar lipo strip;

    CXXFLAGS =
      "-std=c++11 " +
      "-Wall " +
      "-I. " +
      "-O2 -g " +
      "-DWRAPPER_OS_VERSION_MIN=\\\"${macos_version_min}\\\" " +
      "-DWRAPPER_HOST=\\\"${host}\\\" " +
      "-DWRAPPER_ARCH=\\\"${arch}\\\" " +
      "-DWRAPPER_SDK_PATH=\\\"${sdk}\\\" " +
      "-DWRAPPER_COMPILER_RT_PATH=\\\"${compiler_rt}\\\" " +
      "-DWRAPPER_LINKER_VERSION=\\\"${ld.apple_version}\\\"";
  };

  crossenv = rec {
    is_cross = true;

    # Target info.
    inherit host arch;
    os = "macos";
    inherit macos_version_min;
    compiler = "clang";
    exe_suffix = "";
    cmake_system = "Darwin";
    meson_system = "darwin";
    meson_cpu_family = "x86_64";
    meson_cpu = "x86_64";

    # Build tools.
    inherit nixpkgs native;
    wrappers = import ../wrappers crossenv;

    # License information that should be shipped with any software
    # compiled by this environment.
    global_license_set = { };  # TODO: compiler-rt

    # Handy shortcuts.
    inherit clang compiler_rt tapi ld ranlib ar lipo sdk toolchain;

    # Build tools available on the PATH for every derivation.
    default_native_inputs = native.default_native_inputs
      ++ [ clang toolchain wrappers ];

    make_derivation = import ../make_derivation.nix crossenv;
  };
in
  crossenv
