.PHONY: build build-all install testnet cross release test pills clean

build:
	nix-build -A urbit -A herb --no-out-link

build-all:
	nix-build --no-out-link

install:
	nix-env -f . -iA urbit -iA urbit-debug -iA herb

testnet:
	nix-env -f . -iA urbit-testnet

cross:
	sh/cross

release:
	sh/release

test:
	sh/test

pills:
	sh/update-solid-pill
	sh/update-brass-pill

clean:
	rm -rf ./out ./work
	rm -f result result-*
