{ pkgs ? import ../nixpkgs.nix }:

let

  deps = import ../deps { inherit pkgs; };

  ent          = import ./ent          { inherit pkgs; };
  arvo         = import ./arvo         { inherit pkgs; };
  arvo-ropsten = import ./arvo-ropsten { inherit pkgs; };
  herb         = import ../../pkg/herb { inherit pkgs; };

  ge-additions = import ./ge-additions {
    inherit pkgs;
    inherit (deps) ed25519;
  };

  mkUrbit = { debug }:
    import ./urbit {
      inherit pkgs ent debug ge-additions;
      inherit (deps) argon2 murmur3 uv ed25519 sni scrypt softfloat3;
      inherit (deps) secp256k1 h2o ivory-header ca-header;
    };

  urbit       = mkUrbit { debug = false; };
  urbit-debug = mkUrbit { debug = true; };

in

{ inherit ent ge-additions arvo arvo-ropsten herb urbit urbit-debug; }
