{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    tools = {
      flake = false;
      url = "github:urbit/tools";
    };
  };

  outputs = { self, nixpkgs, flake-utils, tools }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        usableTools = pkgs.runCommand "patched-tools" { } ''
          cp -r ${tools} $out
          chmod +w -R $out
          patchShebangs $out
        '';
        pkgs = import nixpkgs { inherit system; };
        bootFakeShip = { pill }:
          pkgs.runCommand "fake-pier" { } ''
            ${pkgs.urbit}/bin/urbit --pier $out -F zod -B ${pill} -l -x -t -A ${
              ./pkg
            }/arvo
          '';
        fakePier = bootFakeShip { pill = ./bin/solid.pill; };
        buildPillThread = pill:
          pkgs.writeTextFile {
            name = "";
            text = ''
              =/  m  (strand ,vase)
              ;<  [=ship =desk =case]  bind:m  get-beak
              ;<  ~  bind:m  (poke [ship %dojo] %lens-command !>([%$ [%dojo '+${pill}'] [%output-pill '${pill}/pill']]))
              ;<  ~  bind:m  (poke [ship %hood] %drum-exit !>(~))
              (pure:m !>(~))
            '';
          };
        buildPill = pill:
          pkgs.runCommand ("${pill}.pill") { buildInputs = [ pkgs.netcat ]; } ''
            cp -r ${fakePier} pier
            chmod +w -R pier
            ${pkgs.urbit}/bin/urbit -d pier
            ${usableTools}/pkg/click/click -k -p -i ${buildPillThread pill} pier

            # Sleep to let urbit spin down properly
            sleep 5

            cp pier/.urb/put/${pill}.pill $out
          '';

      in {
        checks = {
          testFakeShip = import ./nix/test-fake-ship.nix {
            inherit (pkgs) stdenvNoCC curl python3 urbit;
            pier = fakePier;
          };
        };
        packages = {
          inherit fakePier;
          brass = buildPill "brass";
          ivory = buildPill "ivory";
          solid = buildPill "solid";
        };
      });
}
