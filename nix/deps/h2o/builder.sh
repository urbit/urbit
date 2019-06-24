source $stdenv/setup

sources=" \
  deps/cloexec/cloexec.c \
  deps/libgkc/gkc.c \
  deps/libyrmcds/close.c \
  deps/libyrmcds/connect.c \
  deps/libyrmcds/recv.c \
  deps/libyrmcds/send.c \
  deps/libyrmcds/send_text.c \
  deps/libyrmcds/socket.c \
  deps/libyrmcds/strerror.c \
  deps/libyrmcds/text_mode.c \
  deps/picohttpparser/picohttpparser.c \
  lib/common/cache.c \
  lib/common/file.c \
  lib/common/filecache.c \
  lib/common/hostinfo.c \
  lib/common/http1client.c \
  lib/common/memcached.c \
  lib/common/memory.c \
  lib/common/multithread.c \
  lib/common/serverutil.c \
  lib/common/socket.c \
  lib/common/socketpool.c \
  lib/common/string.c \
  lib/common/time.c \
  lib/common/timeout.c \
  lib/common/url.c \
  lib/core/config.c \
  lib/core/configurator.c \
  lib/core/context.c \
  lib/core/headers.c \
  lib/core/logconf.c \
  lib/core/proxy.c \
  lib/core/request.c \
  lib/core/token.c \
  lib/core/util.c \
  lib/handler/access_log.c \
  lib/handler/chunked.c \
  lib/handler/compress.c \
  lib/handler/compress/gzip.c \
  lib/handler/errordoc.c \
  lib/handler/expires.c \
  lib/handler/fastcgi.c \
  lib/handler/file.c \
  lib/handler/headers.c \
  lib/handler/mimemap.c \
  lib/handler/proxy.c \
  lib/handler/redirect.c \
  lib/handler/reproxy.c \
  lib/handler/throttle_resp.c \
  lib/handler/status.c \
  lib/handler/headers_util.c \
  lib/handler/status/events.c \
  lib/handler/status/requests.c \
  lib/handler/http2_debug_state.c \
  lib/handler/status/durations.c \
  lib/handler/configurator/access_log.c \
  lib/handler/configurator/compress.c \
  lib/handler/configurator/errordoc.c \
  lib/handler/configurator/expires.c \
  lib/handler/configurator/fastcgi.c \
  lib/handler/configurator/file.c \
  lib/handler/configurator/headers.c \
  lib/handler/configurator/proxy.c \
  lib/handler/configurator/redirect.c \
  lib/handler/configurator/reproxy.c \
  lib/handler/configurator/throttle_resp.c \
  lib/handler/configurator/status.c \
  lib/handler/configurator/http2_debug_state.c \
  lib/handler/configurator/headers_util.c \
  lib/http1.c \
  lib/tunnel.c \
  lib/http2/cache_digests.c \
  lib/http2/casper.c \
  lib/http2/connection.c \
  lib/http2/frame.c \
  lib/http2/hpack.c \
  lib/http2/scheduler.c \
  lib/http2/stream.c \
  lib/http2/http2_debug_state.c \
"

CFLAGS=" \
  -O3 \
  -Wall -Wno-unused-value -Wno-unused-function \
  -I$src/include \
  -I$src/deps/cloexec \
  -I$src/deps/brotli/enc \
  -I$src/deps/golombset \
  -I$src/deps/libgkc \
  -I$src/deps/libyrmcds \
  -I$src/deps/klib \
  -I$src/deps/neverbleed \
  -I$src/deps/picohttpparser \
  -I$src/deps/picotest \
  -I$src/deps/yaml/include \
  -I$src/deps/yoml
"

for s in $sources
do cc $CFLAGS -c $src/$s -o $(sed 's|/|_|g; s/.c$/.o/' <<< $s)
done

mkdir -p $out/{lib,include}
ar rcs $out/lib/libh2o.a *.o
cp -r $src/include/* $out/include
cp $src/deps/picohttpparser/picohttpparser.h $out/include
