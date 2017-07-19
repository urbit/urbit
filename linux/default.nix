{ nixpkgs, arch }:
let
  host = "${arch}-linux-musl";

  os = "linux";

  exe_suffix = "";

  binutils = import ./binutils { inherit nixpkgs host; };

  headers = nixpkgs.stdenv.mkDerivation rec {
    name = "linux-headers-${linux_arch}-${version}";
    linux_arch = "x86";  # TODO
    version = "4.4.10";
    src = nixpkgs.fetchurl {
      url = "https://cdn.kernel.org/pub/linux/kernel/v4.x/linux-${version}.tar.xz";
      sha256 = "1kpjvvd9q9wwr3314q5ymvxii4dv2d27295bzly225wlc552xhja";
    };
    builder = ./headers_builder.sh;
  };

  gcc = import ./gcc {
    inherit nixpkgs host binutils headers;
  };

  pkg-config = import ../pkgconf { inherit nixpkgs; };

  wrappers = import ../wrappers { inherit nixpkgs; };

  cmake_toolchain = import ../cmake_toolchain {
    cmake_system_name = "Linux";
    inherit nixpkgs host;
  };

  gyp_os = "linux";

  crossenv = {
    # Target info variables.
    inherit host arch os exe_suffix;

    # Cross-compiling toolchain.
    inherit gcc binutils;

    # Build tools and variables to support them.
    inherit pkg-config wrappers cmake_toolchain gyp_os;

    # nixpkgs: a wide variety of programs and build tools.
    inherit nixpkgs;

    inherit headers;

    make_derivation = import ../make_derivation.nix nixpkgs crossenv;

    make_native_derivation = import ../make_derivation.nix nixpkgs null;
  };
in
  crossenv
