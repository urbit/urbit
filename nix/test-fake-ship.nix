{ click, pier, pkgs }:

let
  poke = ''
    =>
    |%
    ++  take-poke-ack
      |=  =wire
      =/  m  (strand ,?)
      ^-  form:m
      |=  tin=strand-input:strand
      ?+  in.tin  `[%skip ~]
          ~  `[%wait ~]
          [~ %agent * %poke-ack *]
        ?.  =(wire wire.u.in.tin)
          `[%skip ~]
        ?~  p.sign.u.in.tin
          `[%done %.y]
        `[%done %.n]
      ==
    ++  poke
      |=  [=dock =cage]
      =/  m  (strand ,?)
      ^-  form:m
      =/  =card:agent:gall  [%pass /poke %agent dock %poke cage]
      ;<  ~  bind:m  (send-raw-card card)
      (take-poke-ack /poke)
    -- 
    
  '';
  testThread = dojoCommand:
    pkgs.writeTextFile {
      name = "${dojoCommand}.hoon";
      text = ''
        ${poke}
        =/  m  (strand ,vase)
        ;<  [=ship =desk =case]  bind:m  get-beak
        ;<  ok=?  bind:m  (poke [ship %dojo] %lens-command !>([%$ [%dojo '${dojoCommand}'] [%stdout ~]]))
        (pure:m !>(ok))
      '';
    };
  appThread = generator: app:
    pkgs.writeTextFile {
      name = ":${app}|${generator}.hoon";
      text = ''
        ${poke}
        =/  m  (strand ,vase)
        ;<  [=ship =desk =case]  bind:m  get-beak
        ;<  ok=?  bind:m  (poke [ship %dojo] %lens-command !>([%$ [%dojo '+${app}/${generator}'] [%app %${app}]]))
        (pure:m !>(ok))
      '';
    };
  pokeApp = hoon: mark: app:
    pkgs.writeTextFile {
      name = ":${app} &${mark} ${hoon}.hoon";
      text = ''
        ${poke}
        =/  m  (strand ,vase)
        ;<  [=ship =desk =case]  bind:m  get-beak
        ;<  ok=?  bind:m  (poke [ship %${app}] %${mark} !>(${hoon}))
        (pure:m !>(ok))
      '';
    };
in pkgs.stdenvNoCC.mkDerivation {
  name = "test-urbit";

  src = pier;

  phases = [ "unpackPhase" "buildPhase" "checkPhase" ];

  nativeBuildInputs = [ pkgs.netcat ];

  unpackPhase = ''
    cp -R $src ./pier
    chmod -R u+rw ./pier
  '';

  buildPhase = ''
    set -x
    set -e

    ${../urbit} -d ./pier 1>&2 2> $out

    tail -F $out >&2 &
 
    ${click} -k -p -i ${appThread "mass" "hood"} ./pier

    sleep 2

    ${click} -k -p -i ${testThread "-test %/tests ~"} ./pier

    ${click} -k -p -i ${pokeApp "%agents" "noun" "test"} ./pier
    ${click} -k -p -i ${pokeApp "%generators" "noun" "test"} ./pier
    ${click} -k -p -i ${pokeApp "%marks" "noun" "test"} ./pier

    ${click} -k -p -i ${appThread "mass" "hood"} ./pier
    sleep 2

    ${click} -k -p -i ${pokeApp "~" "helm-pack" "hood"} ./pier

    ${click} -k -p -i ${appThread "trim" "hood"} ./pier

    ${click} -k -p -i ${appThread "mass" "hood"} ./pier

    ${click} -k -p -i ${appThread "meld" "hood"} ./pier

    ${click} -k -p -i ${appThread "mass" "hood"} ./pier

    ${click} -k -p -i ${appThread "exit" "hood"} ./pier

    set +x
  '';

  checkPhase = ''
    if egrep "((FAILED|CRASHED|Failed)|warn:)" $out >/dev/null; then
      exit 1
    fi
  '';

  doCheck = true;

  # Fix 'bind: operation not permitted' when nix.useSandbox = true on darwin.
  # See https://github.com/NixOS/nix/blob/5f6840fbb49ae5b534423bd8a4360646ee93dbaf/src/libstore/build.cc#L2961
  __darwinAllowLocalNetworking = true;
}
