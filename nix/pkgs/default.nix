{ pkgs ? import ../nixpkgs.nix { } }:

let

  # Dependencies where we rely on which are provide by nixpkgs, but which we
  # may want to recompile during release to build static libraries.
  #
  # TODO: Only do this in release builds, otherwise set sysdeps to pkgs.
  #
  sysdeps = import ../sysdeps.nix { inherit pkgs; };
  #

  # Dependencies that we've packaged ourselves.
  deps = import ../deps { inherit pkgs; };

  ent          = import ./ent          { inherit pkgs; };
  arvo         = import ./arvo         { inherit pkgs; };
  arvo-ropsten = import ./arvo-ropsten { inherit pkgs; };
  herb         = import ./herb         { inherit pkgs; };

  ge-additions = import ./ge-additions {
    inherit pkgs;
    inherit (deps) ed25519;
  };

  libaes_siv = import ./libaes_siv {
    inherit pkgs;
  };

  mkUrbit = { debug }:
    import ./urbit {
      inherit pkgs ent debug ge-additions libaes_siv;
      inherit (sysdeps) curl gmp openssl zlib lmdb;
      inherit (deps) argon2 murmur3 uv ed25519 scrypt softfloat3;
      inherit (deps) secp256k1 h2o ivory-header ca-header;
    };

  urbit       = mkUrbit { debug = false; };
  urbit-debug = mkUrbit { debug = true; };

in

{ inherit ent ge-additions libaes_siv arvo arvo-ropsten herb urbit urbit-debug; }
