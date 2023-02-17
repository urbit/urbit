.PHONY: build build-all install cross release test pills goerli-pills clean

build:
	nix-build -A urbit --no-out-link

install:
	nix-env -f . -iA urbit

release:
	sh/release

test:
	nix-build -A urbit-tests --no-out-link

pills:
	sh/update-solid-pill
	sh/update-brass-pill
	sh/update-ivory-pill

goerli-pills:
	sh/create-goerli-pills

interface:
	sh/build-interface

clean:
	rm -rf ./out ./work
	rm -f result result-*

fmt:
	sh/fmt
