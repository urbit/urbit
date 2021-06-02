sehdexe := build/seh_handler_decorator.exe
CCDEPS  := $(CCDEPS) $(sehdexe)
CCEXTRA  = -E -o -|$(sehdexe) $<|$(CC) $(CFLAGS) -x cpp-output -

$(sehdexe): compat/mingw/seh_handler_decorator.cc
	@$(CC) $< -o $@
