let

  nixpkgs   = import ./nixpkgs.nix;
  nixcrpkgs = import ./nixcrpkgs.nix;

  crossdeps = import ./crossdeps.nix;

  release =
    env_name: env: {
      inherit env env_name;
      deps = crossdeps env;
    };

  linux32 = release "linux32" nixcrpkgs.linux32;
  linux64 = release "linux64" nixcrpkgs.linux64;
  darwin  = release "darwin"  nixcrpkgs.mac;

  hello = env:
    import ./pkgs/hello/release.nix env;

  hellodep = env:
    import ./pkgs/hellodep/release.nix env {};

  ent = env:
    import ./pkgs/ent/cross.nix env;

  urbit = env:
    import ./pkgs/urbit/release.nix env
      { ent = ent env; debug = false; name = "urbit"; };

  urbit-debug = env:
    import ./pkgs/urbit/release.nix env
      { ent = ent env; debug = true; name = "urbit-debug"; };

in

{
  linux32-env = linux32.env;
  linux32 = linux32.deps // {
    ent         = ent         linux32;
    hello       = hello       linux32;
    hellodep    = hellodep    linux32;
    urbit       = urbit       linux32;
    urbit-debug = urbit-debug linux32;
  };

  linux64-env = linux64.env;
  linux64 = linux64.deps // {
    ent         = ent         linux64;
    hello       = hello       linux64;
    hellodep    = hellodep    linux64;
    urbit       = urbit       linux64;
    urbit-debug = urbit-debug linux64;
  };

  darwin-env = darwin.env;
  darwin = darwin.deps // {
    ent         = ent         darwin;
    hello       = hello       darwin;
    hellodep    = hellodep    darwin;
    urbit       = urbit       darwin;
    urbit-debug = urbit-debug darwin;
  };
}
