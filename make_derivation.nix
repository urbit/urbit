nixpkgs: crossenv: attrs:

let
  cross_build_tools = if crossenv == null then [] else [
    crossenv.gcc
    crossenv.binutils
    crossenv.pkg-config
    crossenv.pkg-config-cross
  ];

  default_native_inputs = [
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

  native_inputs =
    (attrs.native_inputs or [])
    ++ cross_build_tools
    ++ default_native_inputs;

  cross_inputs = (attrs.cross_inputs or []);

  path_join = builtins.concatStringsSep ":";

  path_map = dir: inputs: (map (i: "${i}" + dir) inputs);

  default_attrs = {
    system = builtins.currentSystem;

    setup = ./builder_setup.sh;

    PATH = path_join (
      (if attrs ? PATH then [attrs.PATH] else []) ++
      (path_map "/bin" native_inputs)
    );

    PKG_CONFIG_PATH = path_join (
      (if attrs ? PKG_CONFIG_PATH then [attrs.PKG_CONFIG_PATH] else []) ++
      (path_map "/lib/pkgconfig" native_inputs)
    );
  };

  cross_attrs = if crossenv == null then {} else {
    NIXCRPKGS = true;

    inherit (crossenv) host arch os exe_suffix;
    inherit (crossenv) cmake_toolchain;

    PKG_CONFIG_CROSS = "pkg-config-cross";

    PKG_CONFIG_CROSS_PATH = path_join (
      (if attrs ? PKG_CONFIG_CROSS_PATH then [attrs.PKG_CONFIG_CROSS_PATH] else []) ++
      (path_map "/lib/pkgconfig" cross_inputs)
    );
  };

  name_attrs = {
    name = (attrs.name or "package")
      + (if crossenv == null then "" else "-${crossenv.host}");
  };

  builder_attrs =
    if builtins.isAttrs attrs.builder then attrs.builder
    else rec {
      SHELL = builder;
      builder = "${nixpkgs.bash}/bin/bash";
      args = ["-ue" attrs.builder];
    };

  drv_attrs = default_attrs // cross_attrs
    // attrs // name_attrs // builder_attrs;

in
  derivation drv_attrs
