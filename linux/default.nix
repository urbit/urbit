{ nixpkgs, arch }:
let
  host = "${arch}-linux-musl";

  os = "linux";

  exe_suffix = "";

  binutils = import ./binutils { inherit nixpkgs host; };

  gcc = import ./gcc {
    inherit nixpkgs host binutils;
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

    make_derivation = import ../make_derivation.nix nixpkgs crossenv;

    make_native_derivation = import ../make_derivation.nix nixpkgs null;
  };
in
  crossenv
