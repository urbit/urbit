{ crossenv }:

crossenv.make_derivation rec {
  name = "lmdb-${version}";
  version = "0.9.23";
  builder = ./builder.sh;

  src = crossenv.nixpkgs.fetchFromGitHub {
    owner = "LMDB";
    repo = "lmdb";
    rev = "LMDB_${version}";
    sha256 = "0ag7l5180ajvm73y59m7sn3p52xm8m972d08cshxhpwgwa4v35k6";
  };
}
