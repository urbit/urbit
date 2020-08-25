{ crossenv, sources }:

crossenv.make_derivation {
  name          = "uv";
  src           = sources.libuv;
  native_inputs = with crossenv.nixpkgs; [ autoconf automake libtool m4 ];
  builder       = ./builder.sh;

  configureFlags = [ "--disable-shared" ];
  CFLAGS         = "-fPIC";
}
