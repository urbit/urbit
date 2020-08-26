{ crossenv, pills }:

let

  sources = crossenv.nixpkgs.sources;
  
in

rec {
  argon2       = import ./deps/argon2/cross.nix       { inherit crossenv sources; };
  murmur3      = import ./deps/murmur3/cross.nix      { inherit crossenv sources; };
  uv           = import ./deps/uv/cross.nix           { inherit crossenv sources; };
  ed25519      = import ./deps/ed25519/cross.nix      { inherit crossenv sources; };
  scrypt       = import ./deps/scrypt/cross.nix       { inherit crossenv sources; };
  softfloat3   = import ./deps/softfloat3/cross.nix   { inherit crossenv sources; };
  secp256k1    = import ./deps/secp256k1/cross.nix    { inherit crossenv sources; };
  h2o          = import ./deps/h2o/cross.nix          { inherit crossenv sources uv; };
  ivory-header = import ./deps/ivory-header/cross.nix { inherit crossenv; inherit (pills) ivory; };
  ca-header    = import ./deps/ca-header/cross.nix    { inherit crossenv; };
}
