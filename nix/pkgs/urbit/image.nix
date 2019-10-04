{ pkgs
, pill ? ../../../bin/solid.pill
, urbit
}:

let
  name  = urbit.meta.name;
  debug = urbit.meta.debug;

  entrypoint = "${urbit}/bin/${name}";

  coredump = pkgs.writeScript "entrypoint.sh" ''
    #!${pkgs.stdenv.shell}

    set -euo pipefail

    ulimit -c unlimited

    ${entrypoint} -B ${pill} "$@" || \
      $${pkgs.gdb}/bin/gdb -ex "thread apply all bt" -ex "set pagination 0" -batch \
      ${entrypoint} \
      /tmp/cores/core*
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
    Entrypoint = if debug then [ coredump ] else [ entrypoint ];

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
