include config.mk

jets_base = $(wildcard jets/c/*.c) # $(wildcard jets/e/*.c)

jets = jets/tree.c $(wildcard jets/*/*.c)
noun = $(wildcard noun/*.c)
vere = $(wildcard vere/*.c)
king = $(wildcard king/*.c)
serf = $(wildcard serf/*.c)

common  = $(jets) $(noun) $(vere)
headers = $(shell find include -type f)

common_objs = $(shell echo $(common) | sed 's/\.c/.o/g')
king_objs   = $(shell echo $(king) | sed 's/\.c/.o/g')
serf_objs   = $(shell echo $(serf) | sed 's/\.c/.o/g')

all_objs = $(common_objs) $(king_objs) $(serf_objs)
all_srcs = $(common) $(king) $(serf)

################################################################################

.PHONY: all clean mkproper reformat

################################################################################

all: $(shell echo $(jets_base) | sed 's/\.c/.o/g')

clean:
	rm -f $(all_objs) ./urbit ./urbit-worker ./tags

mrproper: clean
	rm -f config.mk include/config.h

reformat:
	ARTISTIC_STYLE_OPTIONS=.astylerc astyle jets/c/*.c


################################################################################

urbit: $(common_objs) $(king_objs)
	@echo CC -o $@
	@$(CC) $^ $(LDFLAGS) -o $@

urbit-worker: $(common_objs) $(serf_objs)
	@echo CC -o $@
	@$(CC) $^ $(LDFLAGS) -o $@

%.o: %.c $(headers)
	@echo CC $<
	@$(CC) -I./include $(CFLAGS) -c $< -o $@

tags: $(all_srcs) $(headers)
	ctags $^
