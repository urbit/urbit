{ native, arch, gcc_options ? "" }:
let
  nixpkgs = native.nixpkgs;

  host = "${arch}-linux-musleabi";

  os = "linux";

  compiler = "gcc";

  exe_suffix = "";

  binutils = import ./binutils { inherit native host; };

  linux_arch =
    if arch == "i686" || arch == "x86_64" then "x86"
    else if arch == "armv6" || arch == "armv7" then "arm"
    else if arch == "aarch64" then "arm64"
    else throw "not sure what Linux architecture code to use";

  headers = native.make_derivation rec {
    name = "linux-headers-${linux_arch}-${version}";
    inherit linux_arch;
    version = "4.4.10";
    src = nixpkgs.fetchurl {
      url = "https://cdn.kernel.org/pub/linux/kernel/v4.x/linux-${version}.tar.xz";
      sha256 = "1kpjvvd9q9wwr3314q5ymvxii4dv2d27295bzly225wlc552xhja";
    };
    builder = ./headers_builder.sh;
  };

  gcc = import ./gcc {
    inherit native host binutils headers gcc_options;
  };

  license = native.make_derivation {
    name = "linux-license";
    inherit (gcc) musl_src gcc_src;
    linux_src = headers.src;
    builder = ./license_builder.sh;
  };

  global_license_set = { _global = license; };

  cmake_toolchain = import ../cmake_toolchain {
    cmake_system_name = "Linux";
    inherit nixpkgs host;
  };

  crossenv = {
    is_cross = true;

    # Build tools available on the PATH for every derivation.
    default_native_inputs = native.default_native_inputs ++
      [ gcc binutils native.pkgconf native.wrappers ];

    # Target info environment variables.
    inherit host arch os compiler exe_suffix;

    # CMake toolchain file.
    inherit cmake_toolchain;

    # A wide variety of programs and build tools.
    inherit nixpkgs;

    # Some native build tools made by nixcrpkgs.
    inherit native;

    # License information that should be shipped with any software
    # compiled by this environment.
    inherit global_license_set;

    # Make it easy to refer to the build tools.
    inherit headers gcc binutils;

    make_derivation = import ../make_derivation.nix crossenv;
  };
in
  crossenv
