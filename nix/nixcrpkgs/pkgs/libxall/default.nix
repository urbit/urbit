# Amalgamates all of our X libraries into one derivation to make it easier to
# build projects that use a lot of them.

{ crossenv, libs }:

let
  lib = crossenv.make_derivation {
    name = "libxall";
    builder.ruby = ./builder.rb;
    inherit libs;
  };

  license_set = builtins.foldl' (x: y: x // y) {} (map (x: x.license_set) libs);
in
  lib // { inherit license_set; }
