source $setup

o=$out/usr/include
s=$sdk/usr/include

headers="
libunwind.h
Availability.h
AvailabilityMacros.h
AvailabilityInternal.h
sys/_types.h
sys/_pthread/_pthread_types.h
sys/_types/_mach_port_t.h
sys/_types/_os_inline.h
sys/appleapiopts.h
"

header_dirs="
architecture
i386
libkern
mach
machine
mach-o
mach_debug
"

for dir in $header_dirs; do
  d=$out/usr/include/$(dirname $dir)/
  mkdir -p $d
  cp -r --no-preserve=mode $sdk/usr/include/$dir $d
done

for header in $headers; do
  mkdir -p $out/usr/include/$(dirname $header)
  cp $sdk/usr/include/$header $out/usr/include/$header
done

gcc -print-search-dirs
