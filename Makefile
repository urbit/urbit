.PHONY: build build-all install cross release test pills ropsten-pills clean

build:
	nix-build -A urbit -A herb --no-out-link

install:
	nix-env -f . -iA urbit -iA herb

release:
	sh/release

test:
	nix-build -A urbit-tests --no-out-link

pills:
	sh/update-solid-pill
	sh/update-brass-pill
	sh/update-ivory-pill

solid:
	sh/update-solid-pill

ropsten-pills:
	sh/create-ropsten-pills

interface:
	sh/build-interface

clean:
	rm -rf ./out ./work
	rm -f result result-*

fmt:
	sh/fmt
