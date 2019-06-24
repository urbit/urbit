{ crossenv }:

crossenv.make_derivation rec {
  name    = "murmur3-71a75";
  builder = ./builder.sh;

  CC = "${crossenv.host}-gcc";
  AR = "${crossenv.host}-ar";

  src = crossenv.nixpkgs.fetchFromGitHub {
    owner = "urbit";
    repo = "murmur3";
    rev = "71a75d57ca4e7ca0f7fc2fd84abd93595b0624ca";
    sha256 = "0k7jq2nb4ad9ajkr6wc4w2yy2f2hkwm3nkbj2pklqgwsg6flxzwg";
  };
}
