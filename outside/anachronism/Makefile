SHELL = sh

CC     = gcc
FLAGS  = -c -fPIC -Iinclude/
CFLAGS = --pedantic -Wall -Wextra -march=native -std=gnu99
INCLUDE = include/anachronism

VERSION_MAJOR = 0
VERSION = $(VERSION_MAJOR).3.1

SO = libanachronism.so
SOFILE = $(SO).$(VERSION)
SONAME = $(SO).$(VERSION_MAJOR)


all: static shared
shared: build/ build/$(SOFILE)
static: build/ build/libanachronism.a

build/:
	mkdir build

build/$(SOFILE): build/nvt.o build/parser.o
	$(CC) -shared -Wl,-soname,$(SONAME) -o build/$(SOFILE) build/nvt.o build/parser.o

build/libanachronism.a: build/nvt.o build/parser.o
	ar rcs build/libanachronism.a build/nvt.o build/parser.o

build/nvt.o: src/nvt.c $(INCLUDE)/nvt.h $(INCLUDE)/common.h
	$(CC) $(FLAGS) $(CFLAGS) src/nvt.c -o build/nvt.o

build/parser.o: src/parser.c $(INCLUDE)/parser.h $(INCLUDE)/common.h
	$(CC) $(FLAGS) $(CFLAGS) src/parser.c -o build/parser.o

src/parser.c: src/parser.rl src/parser_common.rl
	ragel -C -G2 src/parser.rl -o src/parser.c


graph: doc/parser.png

doc/parser.png: src/parser.rl src/parser_common.rl
	ragel -V -p src/parser.rl | dot -Tpng > doc/parser.png

install: all
	install -D -d /usr/local/include/anachronism/ /usr/local/lib
	install -D include/anachronism/* /usr/local/include/anachronism/
	install -D build/$(SOFILE) /usr/local/lib/$(SOFILE)
	install -D build/libanachronism.a /usr/local/lib/libanachronism.a
	ln -s -f /usr/local/lib/$(SOFILE) /usr/local/lib/$(SONAME)
	ln -s -f /usr/local/lib/$(SOFILE) /usr/local/lib/$(SO)

uninstall:
	-rm -rf /usr/local/include/anachronism
	-rm /usr/local/lib/libanachronism.a
	-rm /usr/local/lib/$(SOFILE)
	-rm /usr/local/lib/$(SONAME)
	-rm /usr/local/lib/$(SO)

clean:
	-rm -f build/nvt.o build/router.o build/parser.o

distclean: clean
	-rm -f build/libanachronism.a build/$(SOFILE)

.PHONY: all static shared clean distclean install uninstall
