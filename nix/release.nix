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

  ent = env:
    import ./pkgs/ent/cross.nix env;

  urbit = env:
    import ./pkgs/urbit/release.nix env
      { ent = ent env; debug = false; name = "urbit"; };

  urbit-debug = env:
    import ./pkgs/urbit/release.nix env
      { ent = ent env; debug = true; name = "urbit-debug"; };

  urbit-worker = env:
    import ./pkgs/urbit/release.nix env
      { ent = ent env; debug = false; name = "urbit"; };

  urbit-worker-debug = env:
    import ./pkgs/urbit/release.nix env
      { ent = ent env; debug = true; name = "urbit-debug"; };

in

{
  linux32-env = linux32.env;
  linux32 = linux32.deps // {
    ent                = ent                linux32;
    urbit              = urbit              linux32;
    urbit-debug        = urbit-debug        linux32;
    urbit-worker       = urbit-worker       linux32;
    urbit-worker-debug = urbit-worker-debug linux32;
  };

  linux64-env = linux64.env;
  linux64 = linux64.deps // {
    ent                = ent                linux64;
    urbit              = urbit              linux64;
    urbit-debug        = urbit-debug        linux64;
    urbit-worker       = urbit-worker       linux64;
    urbit-worker-debug = urbit-worker-debug linux64;
  };

  darwin-env = darwin.env;
  darwin = darwin.deps // {
    ent                = ent                darwin;
    urbit              = urbit              darwin;
    urbit-debug        = urbit-debug        darwin;
    urbit-worker       = urbit-worker       darwin;
    urbit-worker-debug = urbit-worker-debug darwin;
  };
}
