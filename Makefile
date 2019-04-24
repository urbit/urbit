PKGS = minima extrema rusthello prog

################################################################################

.PHONY: build build-all install release test clean

build:
	nix-build -A urbit-worker -A urbit -A urb --no-out-link

build-all:
	nix-build --no-out-link

install:
	nix-env -f . -iA urbit -iA urbit-debug -iA urb

release:
	sh/release urbit linux32
	sh/release urbit linux64
	sh/release urbit darwin

test:
	sh/test

clean:
	rm -rf ./out ./work
	rm -f result result-*
