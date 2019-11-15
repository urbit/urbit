.PHONY: build build-all install cross release test pills clean

build:
	nix-build -A urbit -A herb -A hello --no-out-link

build-all:
	nix-build --no-out-link

install:
	nix-env -f . -iA urbit -iA urbit-debug -iA herb -iA hello

cross:
	sh/cross

release:
	sh/release

test:
	sh/test

pills:
	sh/update-solid-pill
	sh/update-brass-pill
	sh/update-ivory-pill

interface:
	sh/build-interface

clean:
	rm -rf ./out ./work
	rm -f result result-*
