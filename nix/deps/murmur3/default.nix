{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  name = "murmur3-71a75";
  builder = ./builder.sh;
  src = pkgs.fetchFromGitHub {
    owner = "urbit";
    repo = "murmur3";
    rev = "71a75d57ca4e7ca0f7fc2fd84abd93595b0624ca";
    sha256 = "0k7jq2nb4ad9ajkr6wc4w2yy2f2hkwm3nkbj2pklqgwsg6flxzwg";
  };
}
