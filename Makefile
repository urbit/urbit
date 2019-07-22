.PHONY: build build-all install cross release test pills clean

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

pills:
	sh/update-solid-pill
	sh/update-brass-pill
	sh/update-ivory-pill

clean:
	rm -rf ./out ./work
	rm -f result result-*
