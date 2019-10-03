{ pkgs
, name
, urbit
}:

pkgs.dockerTools.buildImage {
  inherit name;

  runAsRoot = ''
    #!${pkgs.stdenv.shell}

    set -euo pipefail

    export PATH=/bin:/usr/bin:/sbin:/usr/sbin:$PATH

    ${pkgs.dockerTools.shadowSetup}

    mkdir /data /tmp
  '';

  config = {
    Entrypoint = [ "${urbit}/bin/urbit" ];

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
