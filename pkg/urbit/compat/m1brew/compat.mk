# paths to brew packages
CFLAGS  := $(CFLAGS)  -I/opt/homebrew/include
LDFLAGS := $(LDFLAGS) -L/opt/homebrew/lib
# force linker to use static libraries
LDFLAGS := $(shell compat/m1brew/use-static-libs.sh $(LDFLAGS))
# add extra osx libraries
LDFLAGS := $(LDFLAGS) -framework SystemConfiguration

ifdef debug
CFLAGS  := $(CFLAGS)  -O0 -g
else
# clang hangs on noun/allocate.c if -g is specified with -O3
CFLAGS  := $(CFLAGS)  -O3
endif
