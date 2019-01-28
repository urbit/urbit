pkgname ?= urbit
workdir ?= ../../../work
outdir  ?= ../../../out

b    := ${workdir}/${pkgname}
hack := $(shell mkdir -p $b)

mesonFlags     ?= "-Dgc=false -Dprof=false -Deventtime=false"
mesonBuildType ?= release

################################################################################

.PHONY: all install test clean

all: $b/urbit

install: all
	mkdir -p ${outdir}/bin
	cp "$b/urbit" ${outdir}/bin

test:
	meson . "$b" --buildtype=${mesonBuildType} ${mesonFlags}
	ninja -C "$b" test

clean:
	rm -rf "$b"

################################################################################

$b/urbit:
	meson . "$b" --buildtype=${mesonBuildType} ${mesonFlags}
	ninja -C "$b" urbit
