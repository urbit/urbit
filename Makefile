PKGS = minima extrema rusthello prog

################################################################################

.PHONY: build all install release test clean

build:
	nix-build --no-out-link

all: build release test

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
