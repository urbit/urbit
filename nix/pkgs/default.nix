{ pkgs ? import ../nixpkgs.nix }:

let

  deps = import ../deps { inherit pkgs; };

in

rec {
  arvo         = import ./arvo         { inherit pkgs; };
  ent          = import ./ent          { inherit pkgs; };
  urb          = import ../../pkg/urb  { inherit pkgs; };

  urbit = import ./urbit {
    inherit pkgs ent;
    inherit (deps) argon2 murmur3 uv ed25519 sni scrypt softfloat3;
    inherit (deps) secp256k1 h2o;
    name = "urbit";
    debug = false;
  };

  urbit-debug = import ./urbit {
    inherit pkgs ent;
    inherit (deps) argon2 murmur3 uv ed25519 sni scrypt softfloat3;
    inherit (deps) secp256k1 h2o;
    name = "urbit-debug";
    debug = true;
  };
}
