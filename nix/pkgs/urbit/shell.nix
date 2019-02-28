let

  pkgs = import ../../nixpkgs.nix;
  deps = import ../../deps.nix { inherit pkgs; };

in

import ./default.nix {
  inherit pkgs;
  inherit (deps)
    argon2 ed25519 ent h2o murmur3 scrypt secp256k1 sni softfloat3 uv;
}
