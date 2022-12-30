{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs self;
  inherit (nixpkgs) callPackage curlMinimal openssl zlib lib stdenv;
  sources = import "${self}/nix/sources.nix" {};
  pkgs = import sources.nixpkgs {
    system = "x86_64-linux";
  };
in {
  arvo = callPackage ./arvo {
    inherit (cell.packages) marsSources;
  };
  ca-bundle = callPackage ./ca-bundle {
    
  };
  ent = callPackage ./ent {
    inherit self;
  };
  libaes_siv = callPackage ./libaes_siv {
    inherit sources;
  };
  marsSources = callPackage ./marsSources {
    inherit self;
    inherit (cell.packages) arvo;
  };
  murmur3 = callPackage ./murmur3 {
    inherit sources;
  };
  ivory = callPackage ./pill/ivory.nix {
    inherit self;
    inherit (cell.packages) arvo solid urbit;
    inherit (cell.libraries) bootFakeShip fetchGitHubLFS;
  };
  openssl-static-osx = openssl;
  softfloat3 = callPackage ./softfloat3 {
    inherit sources;
  };
  solid = callPackage ./pill/solid.nix {
    inherit (cell.packages) arvo;
    inherit (cell.libraries) bootFakeShip fetchGitHubLFS;
  };
  urbit = let
    optionalList = xs: if xs == null then [ ] else xs;
    curlUrbit = curlMinimal.override {
      http2Support = false;
      scpSupport = false;
      gssSupport = false;
      ldapSupport = false;
      brotliSupport = false;
    };
    h2o = pkgs.h2o.overrideAttrs (_attrs: {
      version = sources.h2o.rev;
      src = sources.h2o;
      outputs = [ "out" "dev" "lib" ];
      meta.platforms = lib.platforms.linux ++ lib.platforms.darwin;
    });
    libsigsegv = nixpkgs.libsigsegv.overrideAttrs (attrs: {
      patches = optionalList attrs.patches ++ [
        ./libsigsegv/disable-stackvma_fault-linux-arm.patch
        ./libsigsegv/disable-stackvma_fault-linux-i386.patch
      ];
    });
    lmdb = nixpkgs.lmdb.overrideAttrs (attrs: {
      patches =
        optionalList attrs.patches ++ lib.optional stdenv.isDarwin [
          ./pkgs/lmdb/darwin-fsync.patch
        ];
    });
    zlib-static-osx = zlib;
  in callPackage ./urbit {
    inherit self libsigsegv curlUrbit zlib-static-osx h2o lmdb;
    inherit (cell.packages) ent openssl-static-osx ca-bundle ivory murmur3 softfloat3 urcrypt;
  };
  urcrypt = callPackage ./urcrypt {
    inherit self;
    inherit (cell.packages) openssl-static-osx libaes_siv;
  }; #TODO enableStatic
}
