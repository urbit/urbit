{ pkgs ? import ../nixpkgs.nix }:

let

  deps = import ../deps { inherit pkgs; };

  ent          = import ./ent          { inherit pkgs; };
  arvo         = import ./arvo         { inherit pkgs; };
  arvo-ropsten = import ./arvo-ropsten { inherit pkgs; };
  herb         = import ../../pkg/herb { inherit pkgs; };

  libaes_siv = import ./libaes_siv {
    inherit pkgs;
  };

  urcrypt = import ./urcrypt {
    inherit pkgs;
    inherit deps;
  };

  mkUrbit = { debug }:
    import ./urbit {
      inherit pkgs ent debug urcrypt libaes_siv;
      inherit (deps) argon2 murmur3 uv ed25519 scrypt softfloat3;
      inherit (deps) secp256k1 h2o ivory-header ca-header;
    };

  urbit       = mkUrbit { debug = false; };
  urbit-debug = mkUrbit { debug = true; };

in

{ inherit ent urcrypt libaes_siv arvo arvo-ropsten herb urbit urbit-debug; }
