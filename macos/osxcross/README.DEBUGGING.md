### Requirements: ###

* llvm-dsymutil (>= 3.8)
* A Mac OS X system with lldb / gdb installed

### Setting up llvm-dsymutil: ###

First of all, you **really** need llvm-dsymutil from llvm 3.8 (trunk as of writing),  
llvm 3.7 or earlier **is not** sufficient.

Run `./build_llvm_dsymutil.sh` to build and install llvm-dsymutil to target/bin.

`dsymutil` is a no-op if you do not have [osxcross-]llvm-dsymutil >= 3.8 in PATH.

### Debug Example: ###

* Build your application with `-g`
* [LTO only] Add `-Wl,-object_path_lto,lto.o` to the linker flags
* After linking run: `dsymutil binary`
* [Optional] Strip the binary: `x86_64-apple-darwinXX-strip binary`
* Copy the binary **and** the created `<binary>.dSYM` "folder" onto the target Mac OS X system
* Debug the binary as usual
