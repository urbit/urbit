{ native, arch, gcc_options ? "" }:
let
  nixpkgs = native.nixpkgs;

  host = "${arch}-linux-musleabi";

  os = "linux";

  compiler = "gcc";

  exe_suffix = "";

  binutils = import ./binutils { inherit nixpkgs host; };

  linux_arch =
    if arch == "i686" || arch == "x86_64" then "x86"
    else if arch == "armv6" || arch == "armv7" then "arm"
    else throw "not sure what Linux architecture code to use";

  headers = nixpkgs.stdenv.mkDerivation rec {
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
    inherit nixpkgs host binutils headers gcc_options;
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
    # Target info variables.
    inherit host arch os compiler exe_suffix;

    # Cross-compiling toolchain.
    inherit gcc binutils;
    toolchain_drvs = [ gcc binutils ];

    # Build tools and variables to support them.
    inherit cmake_toolchain;

    # nixpkgs: a wide variety of programs and build tools.
    inherit nixpkgs;

    # Some native build tools made by nixcrpkgs.
    inherit native;

    # License information that should be shipped with any software
    # compiled by this environment.
    inherit global_license_set;

    # Linux headers.  These are already a part of GCC though.
    inherit headers;

    make_derivation = import ../make_derivation.nix nixpkgs crossenv;
  };
in
  crossenv
