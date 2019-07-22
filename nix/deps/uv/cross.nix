{ crossenv }:

crossenv.make_derivation rec {
  name          = "uv-64294";
  native_inputs = with crossenv.nixpkgs; [ autoconf automake libtool m4 ];
  builder       = ./builder.sh;

  configureFlags = [ "--disable-shared" ];
  CFLAGS         = "-fPIC";

  src = crossenv.nixpkgs.fetchFromGitHub {
    owner = "urbit";
    repo = "libuv";
    rev = "6429495dc9a80aaf1c243038b381451f12bc7dcf";
    sha256 = "07m2m4v9mds0wihzjxjwswwfj3rnk2ycr3vgwfcrvnb5xjz7rs15";
  };
}
