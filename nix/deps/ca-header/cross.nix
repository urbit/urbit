{ crossenv }:

crossenv.make_derivation rec {
  name              = "ca-bundle.h";
  builder           = ./builder.sh;
  native_inputs     = with crossenv.nixpkgs; [ cacert xxd ];
  SSL_CERT_FILE     = "${crossenv.nixpkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
}
