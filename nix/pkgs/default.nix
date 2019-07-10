{ pkgs ? import ../nixpkgs.nix }:

let

  deps = import ../deps { inherit pkgs; };

  ent          = import ./ent          { inherit pkgs; };
  arvo         = import ./arvo         { inherit pkgs; };
  herb         = import ../../pkg/herb { inherit pkgs; };

  ge-additions = import ./ge-additions {
    inherit pkgs;
    inherit (deps) ed25519;
  };

  mkUrbit = { debug ? false, testnet ? false }:
    import ./urbit {
      inherit pkgs ent debug testnet ge-additions;
      inherit (deps) argon2 murmur3 uv ed25519 sni scrypt softfloat3;
      inherit (deps) secp256k1 h2o;
    };

  urbit         = mkUrbit { };
  urbit-debug   = mkUrbit { debug = true; };
  urbit-testnet = mkUrbit { testnet = true; };

in

{ inherit ent ge-additions arvo herb urbit urbit-debug urbit-testnet; }
