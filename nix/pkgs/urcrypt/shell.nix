let
  pkgs = import ../../nixpkgs.nix;
  deps = import ../../deps { inherit pkgs; };
in

import ./default.nix {
  inherit (pkgs)
    stdenv openssl gmp pkgconfig;
  inherit (deps)
    libaes_siv scrypt secp256k1;
}
