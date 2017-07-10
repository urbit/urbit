
SOURCES = sources

CONFIG_SUB_REV = 3d5db9ebe860
BINUTILS_VER = 2.27
GCC_VER = 6.3.0
MUSL_VER = 1.1.16
GMP_VER = 6.1.1
MPC_VER = 1.0.3
MPFR_VER = 3.1.4
LINUX_VER = 4.4.10

GNU_SITE = https://ftp.gnu.org/pub/gnu
GCC_SITE = $(GNU_SITE)/gcc
BINUTILS_SITE = $(GNU_SITE)/binutils
GMP_SITE = $(GNU_SITE)/gmp
MPC_SITE = $(GNU_SITE)/mpc
MPFR_SITE = $(GNU_SITE)/mpfr
ISL_SITE = http://isl.gforge.inria.fr/

MUSL_SITE = https://www.musl-libc.org/releases
MUSL_REPO = git://git.musl-libc.org/musl

LINUX_SITE = https://cdn.kernel.org/pub/linux/kernel

ifneq ($(NATIVE),)
HOST := $(TARGET)
endif

ifneq ($(HOST),)
BUILD_DIR = build/$(HOST)/$(TARGET)
OUTPUT = $(CURDIR)/output-$(HOST)
else
BUILD_DIR = build/local/$(TARGET)
OUTPUT = $(CURDIR)/output
endif

REL_TOP = ../../..

-include config.mak

SRC_DIRS = gcc-$(GCC_VER) binutils-$(BINUTILS_VER) musl-$(MUSL_VER) \
	$(if $(GMP_VER),gmp-$(GMP_VER)) \
	$(if $(MPC_VER),mpc-$(MPC_VER)) \
	$(if $(MPFR_VER),mpfr-$(MPFR_VER)) \
	$(if $(ISL_VER),isl-$(ISL_VER)) \
	$(if $(LINUX_VER),linux-$(LINUX_VER))

all:

clean:
	rm -rf gcc-* binutils-* musl-* gmp-* mpc-* mpfr-* isl-* build build-* linux-*

distclean: clean
	rm -rf sources

# Rules for building.

ifeq ($(TARGET),)

all:
	@echo TARGET must be set via config.mak or command line.
	@exit 1

else

$(BUILD_DIR)/Makefile: | $(BUILD_DIR)
	ln -sf $(REL_TOP)/litecross/Makefile $@

all: | $(SRC_DIRS) $(BUILD_DIR) $(BUILD_DIR)/Makefile $(BUILD_DIR)/config.mak
	cd $(BUILD_DIR) && $(MAKE) $@

install: | $(SRC_DIRS) $(BUILD_DIR) $(BUILD_DIR)/Makefile $(BUILD_DIR)/config.mak
	cd $(BUILD_DIR) && $(MAKE) OUTPUT=$(OUTPUT) $@

endif
