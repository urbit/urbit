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

  urbit = { env, debug }:
    import ./pkgs/urbit/release.nix env {
      inherit debug;
      name         = if debug then "urbit-debug" else "urbit";
      ent          = ent env;
      ge-additions = ge-additions env;
    };

  builds-for-platform = plat:
    plat.deps // {
      inherit (plat.env) curl libgmp libsigsegv ncurses openssl zlib lmdb;
      inherit (plat.env) cmake_toolchain;
      ent          = ent          plat;
      ge-additions = ge-additions plat;
      urbit        = urbit { env = plat; debug = false; };
      urbit-debug  = urbit { env = plat; debug = true;  };
    };

  darwin_extra = {
    inherit (darwin.env) ranlib ld sdk ar toolchain tapi strip;
  };

in

{
  linux64 = builds-for-platform linux64;
  darwin  = darwin_extra // builds-for-platform darwin;
}
