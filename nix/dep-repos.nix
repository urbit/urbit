# This is a hack to give us access to the fetching of dependency repos
# as nix builds. We use this to cache those Cachix for faster rebuilds
# in CI.

with (import ./deps {});

{
  argon2-src     = argon2.src;
  ed25519-src    = ed25519.src;
  h2o-src        = h2o.src;
  murmur3-src    = murmur3.src;
  scrypt-src     = scrypt.src;
  secp256k1-src  = secp256k1.src;
  sni-src        = sni.src;
  softfloat3-src = softfloat3.src;
  uv-src         = uv.src;
}
