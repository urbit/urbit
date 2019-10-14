{ pkgs, urbit, pill }:

let

  name  = urbit.meta.name;
  debug = urbit.meta.debug;
  exe   = ''${urbit.meta.exe} "$@"'';

  coredump = ''
    ulimit -c unlimited

    ${exe} || \
      ${pkgs.gdb}/bin/gdb -ex "thread apply all bt" -ex "set pagination 0" -batch \
      ${urbit.meta.bin} \
      /tmp/cores/core*
  '';

  entrypoint = pkgs.writeScript "entrypoint.sh" ''
    #!${pkgs.stdenv.shell}

    set -euo pipefail

    ${pkgs.coreutils}/bin/ln -sf ${pill} /data/urbit.pill

    ${if debug then coredump else exe}
  '';

in

pkgs.dockerTools.buildImage {
  inherit name;

  runAsRoot = ''
    #!${pkgs.stdenv.shell}

    set -euo pipefail

    export PATH=/bin:/usr/bin:/sbin:/usr/sbin:$PATH

    ${pkgs.dockerTools.shadowSetup}

    mkdir -p /data /tmp/cores
  '';

  config = {
    Entrypoint = entrypoint;

    WorkingDir = "/data";

    Volumes = {
      "/data" = {};
      "/tmp" = {};
    };

    ExposedPorts = {
      "80/tcp" = {};
      "443/tcp" = {};
    };
 };
}
