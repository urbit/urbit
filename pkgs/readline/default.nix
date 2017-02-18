{ crossenv }:

let
  fetchurl = crossenv.nixpkgs.fetchurl;
in
crossenv.nixpkgs.stdenv.mkDerivation rec {
  name = "readline-${version}-${crossenv.host}";

  version = "7.0";

  src = fetchurl {
    url = "mirror://gnu/readline/readline-${version}.tar.gz";
    sha256 = "0d13sg9ksf982rrrmv5mb6a2p4ys9rvg9r71d6il0vr8hmql63bm";
  };

  patches = [
    (fetchurl {
      url = "mirror://gnu/readline/readline-7.0-patches/readline70-001";
      sha256 = "0xm3sxvwmss7ddyfb11n6pgcqd1aglnpy15g143vzcf75snb7hcs";
    })
    (fetchurl {
      url = "mirror://gnu/readline/readline-7.0-patches/readline70-002";
      sha256 = "0n1dxmqsbjgrfxb1hgk5c6lsraw4ncbnzxlsx7m35nym6lncjiw7";
    })
    (fetchurl {
      url = "mirror://gnu/readline/readline-7.0-patches/readline70-003";
      sha256 = "1027kmymniizcy0zbdlrczxfx3clxcdln5yq05q9yzlc6y9slhwy";
    })
  ];

  builder = ./builder.sh;

  buildInputs = [
    crossenv.gcc
    crossenv.binutils
  ];

  inherit (crossenv) host;
}
