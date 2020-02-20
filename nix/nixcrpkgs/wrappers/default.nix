# Note: We use shebang lines in the wrapper scripts only because Meson gives
# an "exec format error" when it tries to run pkg-config-cross otherwise.

env:
env.native.make_derivation rec {
  name = "cross-wrappers-${env.host}";
  builder = ./builder.sh;
  inherit (env) host cmake_system meson_system meson_cpu_family meson_cpu;
}
