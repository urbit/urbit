# increase default thread stack size and link Windows implibs
LDFLAGS := $(LDFLAGS) -static -Wl,--stack,67108864 -lbcrypt -lntdll -lws2_32
# libcurl
CFLAGS  := $(CFLAGS)  -DCURL_STATICLIB
LDFLAGS := $(LDFLAGS) -lzstd -lcrypt32
# libh2o
CFLAGS  := $(CFLAGS)  -DH2O_NO_UNIX_SOCKETS
# libuv
LDFLAGS := $(LDFLAGS) -luserenv -liphlpapi -lpsapi

ifdef debug
CFLAGS  := $(CFLAGS)  -O0 -g
else
CFLAGS  := $(CFLAGS)  -O3 -g
endif
