PKGS = minima extrema rusthello prog

################################################################################

.PHONY: build build-all install release test clean ctags

build:
	nix-build -A urbit -A urb --no-out-link

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

ctags:
	ctags $(shell find pkg/urbit -type f -name '*.[ch]')
