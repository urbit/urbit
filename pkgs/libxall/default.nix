# Amalgamates all of our X libraries into one derivation to make it easier to
# build projects like Qt that expect them all to be installed in one place.

{ crossenv, libs }:

crossenv.make_derivation {
  name = "libxall";
  builder.ruby = ./builder.rb;
  inherit libs;
}
