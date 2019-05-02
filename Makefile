.PHONY: build build-all install cross release test clean

build:
	nix-build -A urbit -A herb --no-out-link

build-all:
	nix-build --no-out-link

install:
	nix-env -f . -iA urbit -iA urbit-debug -iA herb

cross:
	sh/cross

release:
	sh/release

test:
	sh/test

clean:
	rm -rf ./cross ./release
	rm -f result result-*
