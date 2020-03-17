{ crossenv }:

if crossenv.os != "windows" then "windows only" else

crossenv.make_derivation rec {
  name = "devcon-${version}";

  version = "2019-01-24";

  src = crossenv.nixpkgs.fetchFromGitHub {
    owner = "Microsoft";
    repo = "Windows-driver-samples";
    rev = "39d39e7485faddf601d51c897863ff2f876e391e";
    sha256 = "0nyw3wzlh68zxpb0ydzkgpf5c83scapfp6zsc5gahxlcgrhy3xwj";
  };

  patches = [];

  builder = ./builder.sh;
}
