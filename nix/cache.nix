# All the builds that should be cached in `cachix`.

# Some utility functions; probably don't need these?
let

  # The inverse of builtins.listToAttrs
  attrsToList = o:
    map (a: { name=a; value=builtins.getAttr a o; })
      (builtins.attrNames o);

  # ∀o,x,y. produce o' such that o'.y == o.x.y (assuming no conflicts)
  flattenSet = o:
    builtins.foldl' (acc: v: acc // v) {}
      (builtins.attrValues o);

  prefixSetAttrs = prefix: o:
    builtins.listToAttrs
      (map ({name, value}: { name=prefix + name; value=value; })
        (attrsToList o));

  # ∀o,x,y. produce o' such that o'.x-y == o.x.y
  flattenSetPrefix = o:
    (builtins.foldl' (acc: o: acc // o) {}
      (map ({name, value}: prefixSetAttrs name value)
        (attrsToList o)));

in

let

  pkgs    = import ./pkgs {};
  deps    = import ./deps {};
  release = flattenSetPrefix (import ./release.nix);

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

  deps // pkgs // release // repos
