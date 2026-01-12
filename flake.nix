{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    tools = {
      flake = false;
      url = "github:urbit/tools/d454e2482c3d4820d37db6d5625a6d40db975864";
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
        bootFakeShip = { pill, arvo }:
          pkgs.runCommand "fake-pier" { } ''
            ${./urbit} --pier $out -F zod -B ${pill} -l -x -t -A ${arvo}
          '';
        fakePier = bootFakeShip {
          pill = ./bin/brass.pill;
          arvo = "${./pkg}/arvo";
        };
        testPier = bootFakeShip {
          pill = ./bin/brass.pill;
          arvo = pkgs.runCommand "test-arvo" {} ''
            cp -r ${./pkg} $out
            chmod +w -R $out
            cp -r ${./tests} $out/arvo/tests
            cp -r ${./test-desk.bill} $out/arvo/desk.bill
          '' + "/arvo";
        };
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
            ${./urbit} -d pier
            ${usableTools}/pkg/click/click -k -p -i ${buildPillThread pill} pier

            # Sleep to let urbit spin down properly
            sleep 5

            cp pier/.urb/put/${pill}.pill $out
          '';

      in {
        checks = {
          testFakeShip = import ./nix/test-fake-ship.nix {
            inherit pkgs;
            pier = testPier;
            click = usableTools + "/pkg/click/click";
          };
        };
        packages = {
          inherit fakePier testPier;
          brass = buildPill "brass";
          ivory = buildPill "ivory";
          solid = buildPill "solid";
        };
      });
}
