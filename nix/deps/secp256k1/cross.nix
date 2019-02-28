{ crossenv }:

crossenv.make_derivation rec {
  name    = "secp256k1-b4e87";
  builder = ./builder.sh;

  CFLAGS = "-fPIC";

  configureFlags = [
    "--disable-shared"
    "--enable-module-recovery"
  ];

  cross_inputs  = [ crossenv.libgmp ];
  native_inputs =
    with crossenv.nixpkgs;
    [ autoconf automake libtool m4 ];

  src = crossenv.nixpkgs.fetchFromGitHub {
    owner = "bitcoin-core";
    repo = "secp256k1";
    rev = "e34ceb333b1c0e6f4115ecbb80c632ac1042fa49";
    sha256 = "0as78s179hcr3ysk3fw98k5wzabgnwri7vkkc17wg31lyz6ids6c";
  };
}
