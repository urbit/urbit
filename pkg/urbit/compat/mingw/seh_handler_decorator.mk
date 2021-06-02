sehdexe := build/seh_handler_decorator
CCDEPS  := $(CCDEPS) $(sehdexe)
CCEXTRA  = -E -o -|$(sehdexe) $<|$(CC) $(CFLAGS) -x cpp-output -

$(sehdexe): compat/mingw/seh_handler_decorator.cc
	@mkdir -p ./build
	@$(CC) $< -o $@
