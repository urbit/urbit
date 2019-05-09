{ pkgs ? import ../nixpkgs.nix }:

let

  deps = import ../deps { inherit pkgs; };

  ent  = import ./ent          { inherit pkgs; };
  arvo = import ./arvo         { inherit pkgs; };
  herb = import ../../pkg/herb { inherit pkgs; };

  mkUrbit = { debug }:
    import ./urbit {
      inherit pkgs ent debug;
      inherit (deps) argon2 murmur3 uv ed25519 sni scrypt softfloat3;
      inherit (deps) secp256k1 h2o;
    };

  urbit       = mkUrbit { debug=false; };
  urbit-debug = mkUrbit { debug=true; };

in

{ inherit ent arvo herb urbit urbit-debug; }
