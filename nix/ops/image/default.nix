{ pkgs
, herb
, urbit
, solid ? null
, brass ? null
, ivory ? null
}:

let
  link = pill: path:
    if pill == null then ""
                    else "${pkgs.coreutils}/bin/ln -sf ${pill} ${path}";

in pkgs.dockerTools.buildImage {
  name = urbit.meta.name;

  runAsRoot = ''
    #!${pkgs.stdenv.shell}

    set -euo pipefail

    ${pkgs.dockerTools.shadowSetup}

    mkdir -p /share /data /tmp

    ${link solid "/share/solid.pill"}
    ${link brass "/share/brass.pill"}
    ${link ivory "/share/ivory.pill"}
  '';

  contents = [ urbit herb ];

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
