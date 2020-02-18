{ native, arch, gcc_options ? "" }:
let
  nixpkgs = native.nixpkgs;

  host = "${arch}-linux-musleabi";

  binutils = import ./binutils { inherit native host; };

  linux_arch =
    if arch == "i686" || arch == "x86_64" then "x86"
    else if arch == "armv6" || arch == "armv7" then "arm"
    else if arch == "aarch64" then "arm64"
    else throw "not sure what Linux architecture code to use";

  headers = native.make_derivation rec {
    name = "linux-headers-${linux_arch}-${version}";
    inherit linux_arch;
    version = "4.18.17";
    src = nixpkgs.fetchurl {
      url = "https://cdn.kernel.org/pub/linux/kernel/v4.x/linux-${version}.tar.xz";
      sha256 = "0353ns09i5y0fcygvly20z0qrp6gcqd453186ihm4r7ajgh43bz2";
    };
    builder = ./headers_builder.sh;
  };

  gcc = import ./gcc {
    inherit native host binutils headers gcc_options;
  };

  license = native.make_derivation {
    name = "linux-license";
    musl_src = gcc.musl_src;
    gcc_src = gcc.src;
    linux_src = headers.src;
    builder = ./license_builder.sh;
  };

  global_license_set = { _global = license; };

  crossenv = rec {
    is_cross = true;

    # Target info.
    inherit host arch;
    compiler = "gcc";
    exe_suffix = "";
    os = "linux";
    cmake_system = "Linux";
    meson_system = "linux";
    meson_cpu_family =
      if arch == "i686" then "x86"
      else if arch == "x86_64" then "x86_64"
      else if arch == "armv6" || arch == "armv7" then "arm"
      else if arch == "aarch64" then "aarch64"
      else throw "not sure what meson_cpu_family code to use";
    meson_cpu = arch;

    # Build tools.
    inherit nixpkgs native;
    wrappers = import ../wrappers crossenv;

    # License information that should be shipped with any software
    # compiled by this environment.
    inherit global_license_set;

    # Handy shortcuts.
    inherit headers gcc binutils;

    # Build tools available on the PATH for every derivation.
    default_native_inputs = native.default_native_inputs ++
      [ gcc binutils wrappers ];

    make_derivation = import ../make_derivation.nix crossenv;
  };
in
  crossenv
