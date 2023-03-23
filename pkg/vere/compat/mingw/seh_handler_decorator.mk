# This include file injects a step that transforms vere C source to register
# a SEH exception handler for each function that is declared in a .c file.
# It inserts a .seh_handler directive into each function body with __asm__.
# This directive affects the x64 unwind tables (.pdata and .xdata sections)
# emitted by the compiler.
#
# See gas/config/obj-coff-seh.h in binutils source for .seh_handler, and
# https://docs.microsoft.com/en-us/cpp/build/exception-handling-x64
# for a description of how stack unwinding and SEH work in x64 Windows.
#
# When this file sets CCEXTRA, the first invocation of $(CC) in Makefile
# writes preprocessor output to stdout (-E -o -), which is piped to a simple
# parser that inserts __asm__ statements to each function in the .c file
# being compiled. The second invocation of $(CC) reads transformed source
# from stdin (-x cpp-output -). $< argument to $(sehdexe) tells the parser
# which .c file is being compiled.

sehdexe := build/seh_handler_decorator.exe
CCDEPS  := $(CCDEPS) $(sehdexe)
CCEXTRA  = -E -o -|$(sehdexe) $<|$(CC) $(CFLAGS) -x cpp-output -

$(sehdexe): compat/mingw/seh_handler_decorator.cc
	@mkdir -p ./build
	@$(CC) $< -o $@
