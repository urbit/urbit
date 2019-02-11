include config.mk

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

################################################################################

all: urbit

clean:
	rm -f $(objs) ./urbit ./urbit-worker

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
