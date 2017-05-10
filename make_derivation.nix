crossenv: attrs:

let
  nixpkgs = crossenv.nixpkgs;

  default_native_inputs = [
    crossenv.gcc
    crossenv.binutils
    crossenv.pkg-config
    crossenv.pkg-config-cross
    nixpkgs.cmake
    nixpkgs.ninja
    nixpkgs.gcc
    nixpkgs.binutils
    nixpkgs.coreutils
    nixpkgs.findutils
    nixpkgs.diffutils
    nixpkgs.gnused
    nixpkgs.gnugrep
    nixpkgs.gawk
    nixpkgs.gnutar
    nixpkgs.gzip
    nixpkgs.bzip2
    nixpkgs.gnumake
    nixpkgs.bash
    nixpkgs.patch
    nixpkgs.xz
  ];

  native_inputs = (attrs.native_inputs or []) ++ default_native_inputs;

  cross_inputs = (attrs.cross_inputs or []);

  path_join = builtins.concatStringsSep ":";

  path_map = dir: inputs: (map (i: "${i}" + dir) inputs);

  auto_drv_attrs = rec {
    name = "${attrs.name}-${crossenv.host}";
    system = builtins.currentSystem;
    builder = "${nixpkgs.bash}/bin/bash";
    args = ["-ue" attrs.builder];

    inherit (crossenv) host arch os exe_suffix;
    inherit (crossenv) cmake_toolchain;

    setup = ./builder_setup.sh;
    SHELL = builder;

    NIXCRPKGS = true;

    PATH = path_join (
      (if attrs ? PATH then [attrs.PATH] else []) ++
      (path_map "/bin" native_inputs)
    );

    PKG_CONFIG_CROSS = "pkg-config-cross";

    # TODO: do the default native inputs really deserve to be in PKG_CONFIG_PATH?
    # Seems unnecessary.
    PKG_CONFIG_PATH = path_join (
      (if attrs ? PKG_CONFIG_PATH then [attrs.PKG_CONFIG_PATH] else []) ++
      (path_map "/lib/pkgconfig" native_inputs)
    );

    PKG_CONFIG_CROSS_PATH = path_join (
      (if attrs ? PKG_CONFIG_CROSS_PATH then [attrs.PKG_CONFIG_CROSS_PATH] else []) ++
      (path_map "/lib/pkgconfig" cross_inputs)
    );
  };

  pkg_config_attrs = {};

  drv_attrs = attrs // auto_drv_attrs // pkg_config_attrs;

in
  derivation drv_attrs
