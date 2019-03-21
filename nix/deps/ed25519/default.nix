{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  name = "ed25519-76385";
  builder = ./builder.sh;
  src = pkgs.fetchFromGitHub {
    owner = "urbit";
    repo = "ed25519";
    rev = "76385f2ebbbc9580a9c236952d68d11d73a6135c";
    sha256 = "0s1spif4s9lgcwcny3fl2fvpbw6acqn3s8r6qxnrmkd9icgyw4cp";
  };
}
