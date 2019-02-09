env: attrs:

let
  nixpkgs = env.nixpkgs;

  native_inputs =
    (attrs.native_inputs or [])
    ++ env.default_native_inputs;

  cross_inputs = (attrs.cross_inputs or []);

  path_join = builtins.concatStringsSep ":";

  path_map = dir: inputs: (map (i: "${i}" + dir) inputs);

  # We can't just set PATH in our derivation because nix-shell will make the
  # derivation's PATH override the system PATH, meaning we can't use utilities
  # like "git" or "which" form the host system.  So we set _PATH instead, and we
  # use a setup script ($setup) that copies _PATH to PATH. And we provide
  # $stdenv/setup so that nix-shell can find our setup script.
  #
  # nixcrpkgs does not expose its users to this mess.  The user can specify a
  # PATH if they want, and it will be automatically moved to _PATH in the
  # derivation.
  filtered_attrs = nixpkgs.lib.filterAttrs (n: v: n != "PATH") attrs;

  path_attrs = {
    _PATH = path_join (
      (if attrs ? PATH then [attrs.PATH] else []) ++
      (path_map "/bin" native_inputs)
    );
  };

  default_attrs = {
    system = builtins.currentSystem;

    SHELL = "${nixpkgs.bashInteractive}/bin/bash";

    setup = ./pretend_stdenv/setup;

    # This allows nix-shell to find our setup script.
    stdenv = ./pretend_stdenv;

    PKG_CONFIG_PATH = path_join (
      (if attrs ? PKG_CONFIG_PATH then [attrs.PKG_CONFIG_PATH] else []) ++
      (path_map "/lib/pkgconfig" native_inputs)
    );
  };

  cross_attrs = if !env.is_cross then {} else {
    NIXCRPKGS = true;

    inherit (env) host arch os exe_suffix;

    PKG_CONFIG_CROSS_PATH = path_join (
      (if attrs ? PKG_CONFIG_CROSS_PATH then [attrs.PKG_CONFIG_CROSS_PATH] else []) ++
      (path_map "/lib/pkgconfig" cross_inputs)
    );

    CMAKE_CROSS_PREFIX_PATH = path_join (
      (if attrs ? CMAKE_CROSS_PREFIX_PATH then [attrs.CMAKE_CROSS_PREFIX_PATH] else []) ++
      cross_inputs
    );
  };

  name_attrs = {
    name = (attrs.name or "package")
      + (if env.is_cross then "-${env.host}" else "");
  };

  builder_attrs =
    if builtins.isAttrs attrs.builder then
      if attrs.builder ? ruby then
        {
          builder = "${nixpkgs.ruby}/bin/ruby";
          args = [attrs.builder.ruby];
        }
      else
        attrs.builder
    else
      rec {
        builder = "${nixpkgs.bashInteractive}/bin/bash";
        args = ["-ue" attrs.builder];
      };

  drv_attrs = default_attrs // cross_attrs
    // filtered_attrs // name_attrs // builder_attrs // path_attrs;

in
  derivation drv_attrs
