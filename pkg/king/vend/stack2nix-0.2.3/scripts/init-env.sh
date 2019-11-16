#!/usr/bin/env bash

readlink=$(nix-instantiate --eval -E "/. + (import <nix/config.nix>).coreutils")/readlink
export scriptDir=$(dirname -- "$($readlink -f -- "${BASH_SOURCE[0]}")")
NIXPKGS=$(nix-build "${scriptDir}/../fetch-nixpkgs.nix" --no-out-link)
export NIX_PATH="nixpkgs=$NIXPKGS"


set -ex

STACK=$(nix-build -A stack $NIXPKGS)/bin
NIX_PREFETCH=$(nix-build -A nix-prefetch-git $NIXPKGS)/bin
GIT=$(nix-build -A git $NIXPKGS)/bin
CABAL_INSTALL=$(nix-build -A cabal-install $NIXPKGS)/bin
export PATH="$STACK:$NIX_PREFETCH:$GIT:$CABAL_INSTALL:$HOME/.local/bin:$PATH"
