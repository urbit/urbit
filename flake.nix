{
  nixConfig = {
    extra-trusted-substituters =  "https://ares.cachix.org";
    extra-trusted-public-keys = "ares.cachix.org-1:oBV5cWH+gdNxZQdHCTIJXudjEnxihveslHHXB53GhKk=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let

        pkgs = nixpkgs.legacyPackages.${system};

      in
      {
        devShell = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            bazel_5
            pkg-config
            cmake
            autoconf
            autogen
            automake
            libtool
          ];
        };
      });
}
