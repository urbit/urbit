## COMPILER-RT ##

### WHAT IS COMPILER-RT? ###

Please see http://compiler-rt.llvm.org.

### INSTALLATION: ###

Ensure you have finished `build.sh`,

then run: `./build_compiler_rt.sh`.

You can verify compiler-rt is working by invoking the following command:

    echo "int main(void){return 0;}" | xcrun clang -xc -o/dev/null -v - 2>&1 | \
      grep "libclang_rt" 1>/dev/null && echo "Success"

If you see "Success", then everything went well.

### USAGE: ###

You do not need to do anything, clang's doing the job for you.

However, `-fsanitize=address` is a bit annoying because the address sanitizer library is linked  
dynamically, and thus requires you to copy the ASAN runtime library onto the target system. 

\[See [README.DEBUGGING](README.DEBUGGING.md) in how to get a backtrace with line numbers and symbol names]

The following example illustrates how to achieve this:

    # Example source code.
    $ cat test.c
    #include <stdlib.h>
    #include <string.h>
    #include <stdio.h>

    int main(void) {
      char buf[2];
      strcpy(buf, "Hello World"); /* Buffer overflow. */
      puts(buf);
      return 0;
    }

    # Compile the source file.
    $ o64-clang test.c -fsanitize=address -o test

    # Ensure the ASAN library is linked in.
    $ xcrun otool -L test
    test:
            /usr/lib/libstdc++.6.dylib (compatibility version 7.0.0, current version 104.1.0) # ASAN dep.
            @rpath/libclang_rt.asan_osx_dynamic.dylib (compatibility version 0.0.0, current version 0.0.0)
            /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1213.0.0)

    # Copy 'test' and 'libclang_rt.asan_osx_dynamic.dylib' onto the target system.

    # You can find the ASAN library path easily with this one-liner:
    $ echo "int main(void){return 0;}" | xcrun clang -fsanitize=address -xc -o/dev/null -v - 2>&1 | \
      tr ' ' '\n' | grep libclang_rt.asan_osx_dynamic.dylib
    [...]/bin/../lib/clang/3.6.2/lib/darwin/libclang_rt.asan_osx_dynamic.dylib

    # Run ./test on the target system:
    $ DYLD_LIBRARY_PATH=. ./test
    =================================================================
    ==410==ERROR: AddressSanitizer: stack-buffer-overflow on address 0x7fff58c3ec72 at pc [...]
    WRITE of size 12 at 0x7fff58c3ec72 thread T0
        #0 0x1070029ac  ([...]/tmp/libclang_rt.asan_osx_dynamic.dylib+0x3a9ac)
        #1 0x106fc1d3a  ([...]/tmp/./test+0x100000d3a)
        #2 0x106fc1bd3  ([...]/tmp/./test+0x100000bd3)
        #3 0x0  (<unknown module>)

    Address 0x7fff58c3ec72 is located in stack of thread T0 at offset 50 in frame
        #0 0x106fc1bef  ([...]/tmp/./test+0x100000bef)

      This frame has 2 object(s):
        [32, 36) ''
        [48, 50) 'buf' <== Memory access at offset 50 overflows this variable
    HINT: this may be a false positive if your program uses some custom stack unwind mechanism or swapcontext
          (longjmp and C++ exceptions *are* supported)
    Shadow bytes around the buggy address:
      0x1fffeb187d30: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
      0x1fffeb187d40: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
      0x1fffeb187d50: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
      0x1fffeb187d60: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
      0x1fffeb187d70: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    =>0x1fffeb187d80: 00 00 00 00 00 00 00 00 f1 f1 f1 f1 04 f2[02]f3
      0x1fffeb187d90: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
      0x1fffeb187da0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
      0x1fffeb187db0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
      0x1fffeb187dc0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
      0x1fffeb187dd0: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
    Shadow byte legend (one shadow byte represents 8 application bytes):
      Addressable:           00
      Partially addressable: 01 02 03 04 05 06 07 
      Heap left redzone:       fa
      Heap right redzone:      fb
      Freed heap region:       fd
      Stack left redzone:      f1
      Stack mid redzone:       f2
      Stack right redzone:     f3
      Stack partial redzone:   f4
      Stack after return:      f5
      Stack use after scope:   f8
      Global redzone:          f9
      Global init order:       f6
      Poisoned by user:        f7
      Container overflow:      fc
      Array cookie:            ac
      Intra object redzone:    bb
      ASan internal:           fe
      Left alloca redzone:     ca
      Right alloca redzone:    cb
    ==410==ABORTING

