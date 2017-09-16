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
