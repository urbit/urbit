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
all_srcs = $(common) $(king) $(serf)

################################################################################

.PHONY: all clean mkproper

################################################################################

all: jets/c/can.o jets/c/bex.o jets/c/ripn.o \
     jets/c/cat.o jets/c/cap.o jets/c/con.o  \
     jets/c/dis.o jets/c/dor.o jets/c/dvr.o  \
     jets/c/end.o jets/c/gor.o jets/c/lsh.o  \
     jets/c/mas.o jets/c/met.o jets/c/mix.o  \
     jets/c/mor.o jets/c/mug.o jets/c/muk.o  \
     jets/c/peg.o jets/c/po.o  jets/c/pow.o  \
     jets/c/rap.o jets/c/rep.o jets/c/rev.o jets/c/rip.o  \

clean:
	rm -f $(all_objs) ./urbit ./urbit-worker ./tags

mrproper: clean
	rm -f config.mk include/config.h

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
