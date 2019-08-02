let

  pkgs = import ../../nixpkgs.nix;
  deps = import ../../deps { inherit pkgs; };
  tlon = import ../../pkgs { inherit pkgs; };

in

import ./default.nix {
  inherit pkgs;
  debug = false;
  inherit (tlon)
    ent ge-additions;
  inherit (deps)
    argon2 ed25519 h2o murmur3 scrypt secp256k1 sni softfloat3 uv ivory-header;
}
