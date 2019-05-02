# All the non-release builds that should be cached in `cachix`.

let

  pkgs = import ../pkgs {};
  deps = import ../deps {};

  # Cache the result of cloning source repos.
  repos = {
    argon2-src     = deps.argon2.src;
    ed25519-src    = deps.ed25519.src;
    h2o-src        = deps.h2o.src;
    murmur3-src    = deps.murmur3.src;
    scrypt-src     = deps.scrypt.src;
    secp256k1-src  = deps.secp256k1.src;
    sni-src        = deps.sni.src;
    softfloat3-src = deps.softfloat3.src;
    uv-src         = deps.uv.src;
  };

in

  deps // pkgs // repos
