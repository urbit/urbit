{ nixpkgs, host, cmake_system_name }:

nixpkgs.stdenv.mkDerivation {
  name = "cmake_toolchain-${host}.txt";
  builder = ./builder.sh;
  inherit host cmake_system_name;
}
