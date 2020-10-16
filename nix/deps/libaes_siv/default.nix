{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  name = "libaes_siv-96812";
  buildInputs = [ pkgs.cmake pkgs.openssl ];
  builder = ./builder.sh;
  src = pkgs.fetchFromGitHub {
    owner = "dfoxfranke";
    repo = "libaes_siv";
    rev = "9681279cfaa6e6399bb7ca3afbbc27fc2e19df4b";
    sha256 = "1g4wy0m5wpqx7z6nillppkh5zki9fkx9rdw149qcxh7mc5vlszzi";
  };
}
