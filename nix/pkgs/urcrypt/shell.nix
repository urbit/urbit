let
  pkgs = import ../../nixpkgs.nix;
  deps = import ../../deps { inherit pkgs; };
in

import ./default.nix {
  inherit (pkgs)
    stdenv openssl gmp;
  inherit (deps)
    libaes_siv argon2 scrypt secp256k1;
}
