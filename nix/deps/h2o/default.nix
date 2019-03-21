{ pkgs, uv }:

pkgs.stdenv.mkDerivation rec {
  name = "h2o-0ed9a";
  buildInputs = [ uv pkgs.openssl pkgs.zlib ];
  builder = ./builder.sh;
  src = pkgs.fetchFromGitHub {
    owner = "urbit";
    repo = "h2o";
    rev = "0ed9ac70757a16ec45f91b8a347850d9699c3fb1";
    sha256 = "16b5zbwdq371hhqga76dh7x4c0qr3xb5ah9r8hnm6rip460p6xpm";
  };
}
