{ pkgs, urbit, pill }:

pkgs.dockerTools.buildImage {
  name = urbit.meta.name;

  runAsRoot = ''
    #!${pkgs.stdenv.shell}

    set -euo pipefail

    ${pkgs.dockerTools.shadowSetup}

    mkdir -p /share /data /tmp

    ${pkgs.coreutils}/bin/ln -sf ${pill} /share/urbit.pill
  '';

  contents = [ urbit ];

  config = {
    Entrypoint = [ urbit.meta.name ];

    WorkingDir = "/data";

    Volumes = {
      "/data" = {};
    };

    ExposedPorts = {
      "80/tcp" = {};
      "443/tcp" = {};
    };
 };
}
