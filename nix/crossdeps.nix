crossenv:

rec {
  argon2     = import ./deps/argon2/cross.nix     { inherit crossenv; };
  murmur3    = import ./deps/murmur3/cross.nix    { inherit crossenv; };
  uv         = import ./deps/uv/cross.nix         { inherit crossenv; };
  ed25519    = import ./deps/ed25519/cross.nix    { inherit crossenv; };
  sni        = import ./deps/sni/cross.nix        { inherit crossenv; };
  scrypt     = import ./deps/scrypt/cross.nix     { inherit crossenv; };
  softfloat3 = import ./deps/softfloat3/cross.nix { inherit crossenv; };
  secp256k1  = import ./deps/secp256k1/cross.nix  { inherit crossenv; };
  h2o        = import ./deps/h2o/cross.nix        { inherit crossenv uv; };
}
