override CXXFLAGS=-std=c++11 -Wall -Wextra -pedantic
override CXXFLAGS+=-Wno-missing-field-initializers
override CXXFLAGS+=-I. -O2 -g
override CXXFLAGS+=-DOSXCROSS_VERSION="\"$(OSXCROSS_VERSION)\""
override CXXFLAGS+=-DOSXCROSS_TARGET="\"$(OSXCROSS_TARGET)\""
override CXXFLAGS+=-DOSXCROSS_OSX_VERSION_MIN="\"$(OSXCROSS_OSX_VERSION_MIN)\""
override CXXFLAGS+=-DOSXCROSS_LINKER_VERSION="\"$(OSXCROSS_LINKER_VERSION)\""
override CXXFLAGS+=-DOSXCROSS_LIBLTO_PATH="\"$(OSXCROSS_LIBLTO_PATH)\""
override CXXFLAGS+=-DOSXCROSS_BUILD_DIR="\"$(OSXCROSS_BUILD_DIR)\""
override CXXFLAGS+=-DOSXCROSS_SDK="\"$(OSXCROSS_SDK)\""
override CXXFLAGS+=-DOSXCROSS_SDK_VERSION="\"$(OSXCROSS_SDK_VERSION)\""

SRCS= \
 main.cpp \
 tools.cpp \
 target.cpp \
 programs/osxcross-version.cpp \
 programs/osxcross-env.cpp \
 programs/osxcross-conf.cpp \
 programs/osxcross-cmp.cpp \
 programs/osxcross-man.cpp \
 programs/sw_vers.cpp \
 programs/pkg-config.cpp \
 programs/xcrun.cpp \
 programs/dsymutil.cpp

OBJS=$(subst .cpp,.o,$(SRCS))

all:
	g++ $(CXXFLAGS) -o wrapper $(SRCS)
