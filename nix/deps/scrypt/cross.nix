{ crossenv }:

crossenv.make_derivation rec {
  name    = "scrypt-02969";
  builder = ./builder.sh;

  CC = "${crossenv.host}-gcc";
  AR = "${crossenv.host}-ar";

  src = crossenv.nixpkgs.fetchFromGitHub {
    owner = "urbit";
    repo = "libscrypt";
    rev = "029693ff1cbe4f69d3a2da87d0f4f034f92cc0c2";
    sha256 = "17pcxypzjmmrvacw45cacvibm6mlr9ip30hy30l1appsnywx679n";
  };
}
