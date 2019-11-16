#!/usr/bin/env bash

readlink=$(nix-instantiate --eval -E "/. + (import <nix/config.nix>).coreutils")/readlink
scriptDir=$(dirname -- "$($readlink -f -- "${BASH_SOURCE[0]}")")
source $scriptDir/init-env.sh

# build and install
\time stack --nix install --test --fast --ghc-options="+RTS -A128m -n2m -RTS"

# SMOKE TESTS

export NIX_PATH=nixpkgs=https://github.com/NixOS/nixpkgs/archive/ef802feaceba073e3e5178af1b5d65f27f5cc93b.tar.gz

# basic remote
\time stack2nix --verbose -o /tmp/haskell-dummy-project1.nix \
	  --revision 31aac4dcc7b87d5cb62dafe9b9402346fdf449a6 \
	  https://github.com/jmitchell/haskell-dummy-project1.git
\time nix-build -A haskell-dummy-package1 /tmp/haskell-dummy-project1.nix

# multi remote
\time stack2nix --verbose -o /tmp/haskell-multi-package-demo1.nix \
	  --revision e3d9bd6d6066dab5222ce53fb7d234f28eafa2d5 \
	  https://github.com/jmitchell/haskell-multi-package-demo1.git
\time nix-build -A haskell-multi-proj-demo1 /tmp/haskell-multi-package-demo1.nix

# multi local and ../ relpath
TMP_REPO="$(mktemp -d)"
git clone https://github.com/jmitchell/haskell-multi-package-demo1.git "$TMP_REPO"
cd "$TMP_REPO"
git checkout e3d9bd6d6066dab5222ce53fb7d234f28eafa2d5
cd src
\time stack2nix --verbose -o hmpd.nix ../
\time nix-build -A haskell-multi-proj-demo1 hmpd.nix
test $(grep "src = ../dep1" hmpd.nix | wc -l) -eq 1
