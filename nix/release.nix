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

  builds-for-platform = plat:
    plat.deps // {
      inherit (plat.env) curl libgmp libsigsegv ncurses openssl zlib lmdb;

      ent         = ent         plat;
      urbit       = urbit       plat;
      urbit-debug = urbit-debug plat;
    };

in

{
  linux32 = builds-for-platform linux32;
  linux64 = builds-for-platform linux64;
  darwin  = builds-for-platform darwin;
}
