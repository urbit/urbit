{ env }:
env.make_derivation rec {
  name = "cross-wrappers";
  builder = ./builder.sh;
}
