let

  nixpkgs   = import ./nixpkgs.nix;
  nixcrpkgs = import ./nixcrpkgs.nix;
  crossdeps = import ./crossdeps.nix;

  release =
    env_name: env: {
      inherit env env_name;
      deps = crossdeps env;
    };

  linux64 = release "linux64" nixcrpkgs.linux64;
  darwin  = release "darwin"  nixcrpkgs.mac;

  ent = env:
    import ./pkgs/ent/cross.nix env;

  ge-additions = env:
    import ./pkgs/ge-additions/cross.nix env;

  urbit = env:
    import ./pkgs/urbit/release.nix env
      { ent = ent env; ge-additions = ge-additions env; cacert = nixpkgs.cacert;
        xxd = nixpkgs.xxd; debug = false; name = "urbit"; };

  builds-for-platform = plat:
    plat.deps // {
      inherit (plat.env) curl libgmp libsigsegv ncurses openssl zlib lmdb;
      inherit (plat.env) cmake_toolchain;
      ent          = ent          plat;
      ge-additions = ge-additions plat;
      urbit        = urbit        plat;
    };

  darwin_extra = {
    inherit (darwin.env) ranlib ld sdk ar toolchain tapi strip;
  };

in

{
  linux64 = builds-for-platform linux64;
  darwin  = darwin_extra // builds-for-platform darwin;
}
