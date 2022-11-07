.PHONY: build build-all clean cross doc install pills release ropsten-pills test

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

ropsten-pills:
	sh/create-ropsten-pills

interface:
	sh/build-interface

clean:
	rm -rf ./out ./work
	rm -f result result-*

doc:
	doxygen pkg/urbit/Doxyfile

fmt:
	sh/fmt
