{ urbit, libcap, coreutils, bashInteractive, dockerTools, writeScriptBin, amesPort ? 34343 }:
let
  startUrbit = writeScriptBin "start-urbit" ''
    #!${bashInteractive}/bin/bash

    set -eu

    # set defaults
    amesPort=${toString amesPort}

    # check args
    for i in "$@"
    do
    case $i in
        -p=*|--port=*)
            amesPort="''${i#*=}"
            shift
            ;;
    esac
    done

    # If the container is not started with the `-i` flag
    # then STDIN will be closed and we need to start
    # Urbit/vere with the `-t` flag.
    ttyflag=""
    if [ ! -t 0 ]; then
      echo "Running with no STDIN"
      ttyflag="-t"
    fi

    # Check if there is a keyfile, if so boot a ship with its name, and then remove the key
    if [ -e *.key ]; then
      # Get the name of the key
      keynames="*.key"
      keys=( $keynames )
      keyname=''${keys[0]}
      mv $keyname /tmp

      # Boot urbit with the key, exit when done booting
      urbit $ttyflag -w $(basename $keyname .key) -k /tmp/$keyname -c $(basename $keyname .key) -p $amesPort -x

      # Remove the keyfile for security
      rm /tmp/$keyname
      rm *.key || true
    elif [ -e *.comet ]; then
      cometnames="*.comet"
      comets=( $cometnames )
      cometname=''${comets[0]}
      rm *.comet

      urbit $ttyflag -c $(basename $cometname .comet) -p $amesPort -x
    fi

    # Find the first directory and start urbit with the ship therein
    dirnames="*/"
    dirs=( $dirnames )
    dirname=''${dirnames[0]}

    exec urbit $ttyflag -p $amesPort $dirname
    '';
    
    
in dockerTools.buildImage {
  name = "urbit";
  tag = "v${urbit.version}";
  contents = [ bashInteractive urbit startUrbit coreutils ];
  runAsRoot = ''
    #!${bashInteractive}
    mkdir -p /urbit
    mkdir -p /tmp
    ${libcap}/bin/setcap 'cap_net_bind_service=+ep' /bin/urbit
    '';
  config = {
    Cmd = [ "/bin/start-urbit" ];
    Env = [ "PATH=/bin" ];
    WorkingDir = "/urbit";
    Volumes = {
      "/urbit" = {};
    };
    Expose = [ "80/tcp" "${toString amesPort}/udp" ];
  };
}
