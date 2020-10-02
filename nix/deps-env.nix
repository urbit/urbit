let

  pkgs = import ./nixpkgs.nix;
  tlon = import ./pkgs { pkgs=pkgs; };
  deps = import ./deps { pkgs=pkgs; };

  tools =
    with pkgs;
    [ cargo rustc meson ninja pkgconfig libtool gdb ];

  libs =
    with pkgs;
    [ openssl curl gmp scrypt libsigsegv openssl zlib lmdb ];

  osx =
    with pkgs;
    lib.optionals stdenv.isDarwin (
      with darwin.apple_sdk.frameworks;
      [ Cocoa CoreServices ]);

  vendor =
    with deps;
    [ argon2 ed25519 h2o murmur3 scrypt secp256k1 softfloat3 uv ent urcrypt ivory-header ca-header ];

in

pkgs.stdenv.mkDerivation rec {
  name        = "urbit-deps-env";
  env         = pkgs.buildEnv { name = name; paths = buildInputs; };
  buildInputs = tools ++ libs ++ osx ++ vendor;
}
