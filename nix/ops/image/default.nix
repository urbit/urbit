{ pkgs, urbit, pill }:

pkgs.dockerTools.buildImage {
  name = urbit.meta.name;

  runAsRoot = ''
    #!${pkgs.stdenv.shell}

    set -euo pipefail

    export PATH=/bin:/usr/bin:/sbin:/usr/sbin:$PATH

    ${pkgs.dockerTools.shadowSetup}

    mkdir -p /bin /share /data /tmp

    ${pkgs.coreutils}/bin/ln -sf ${pill} /share/urbit.pill
    ${pkgs.coreutils}/bin/ln -sf ${entrypoint} /bin/urbit
  '';

  config = {
    Entrypoint = [ "urbit" ];

    WorkingDir = "/data";

    Env = [ "PATH=/bin" ];

    Volumes = {
      "/data" = {};
    };

    ExposedPorts = {
      "80/tcp" = {};
      "443/tcp" = {};
    };
 };
}
