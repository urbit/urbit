{ crossenv }:

crossenv.make_derivation rec {
  name    = "softfloat3-ec4c7";
  builder = ./builder.sh;

  CC = "${crossenv.host}-gcc";
  AR = "${crossenv.host}-ar";

  src = crossenv.nixpkgs.fetchFromGitHub {
    owner = "urbit";
    repo = "berkeley-softfloat-3";
    rev = "ec4c7e31b32e07aad80e52f65ff46ac6d6aad986";
    sha256 = "1lz4bazbf7lns1xh8aam19c814a4n4czq5xsq5rmi9sgqw910339";
  };
}
