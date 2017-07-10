
XGCC_DIR = ../obj_toolchain/gcc
XGCC = $(XGCC_DIR)/xgcc -B $(XGCC_DIR)

-include config.mak

MAKE += MULTILIB_OSDIRNAMES=
MAKE += INFO_DEPS= infodir=
MAKE += ac_cv_prog_lex_root=lex.yy.c
MAKE += MAKEINFO=false

FULL_TOOLCHAIN_CONFIG = --enable-languages=c,c++ \
	--disable-werror \
	--target=$(TARGET) --prefix= \
	--libdir=/lib --disable-multilib \
	--with-sysroot=$(SYSROOT) \
	--enable-tls \
	--disable-libmudflap --disable-libsanitizer \
	--disable-gnu-indirect-function \
	--disable-libmpx \
	--enable-deterministic-archives \
	--enable-libstdcxx-time

FULL_MUSL_CONFIG = --prefix= --host=$(TARGET)

SYSROOT = /$(TARGET)
FULL_TOOLCHAIN_CONFIG += --with-build-sysroot=$(CURDIR)/obj_sysroot
FULL_MUSL_CONFIG += CC="$(XGCC)" LIBCC="../obj_toolchain/$(TARGET)/libgcc/libgcc.a" 
MUSL_VARS = AR=../obj_toolchain/binutils/ar RANLIB=../obj_toolchain/binutils/ranlib
obj_musl/.lc_configured: | obj_toolchain/gcc/.lc_built
obj_musl/.lc_built: | obj_toolchain/$(TARGET)/libgcc/libgcc.a
obj_toolchain/gcc/.lc_built: | obj_sysroot/usr obj_sysroot/lib32 obj_sysroot/lib64 obj_sysroot/include
obj_toolchain/.lc_built: | obj_sysroot/.lc_libs obj_sysroot/.lc_headers

all: musl toolchain

install: install-musl install-toolchain

musl: obj_musl/.lc_built

toolchain: obj_toolchain/.lc_built

.PHONY: all musl toolchain install-musl install-toolchain clean

src_binutils: | $(BINUTILS_SRCDIR)
	ln -sf $(BINUTILS_SRCDIR) $@

src_gcc: | $(GCC_SRCDIR)
	ln -sf $(GCC_SRCDIR) $@

src_musl: | $(MUSL_SRCDIR)
	ln -sf $(MUSL_SRCDIR) $@

ifneq ($(GMP_SRCDIR),)
src_toolchain: src_gmp
src_gmp: | $(GMP_SRCDIR)
	ln -sf "$(GMP_SRCDIR)" $@
endif

ifneq ($(MPC_SRCDIR),)
src_toolchain: src_mpc
src_mpc: | $(MPC_SRCDIR)
	ln -sf "$(MPC_SRCDIR)" $@
endif

ifneq ($(MPFR_SRCDIR),)
src_toolchain: src_mpfr
src_mpfr: | $(MPFR_SRCDIR)
	ln -sf "$(MPFR_SRCDIR)" $@
endif

ifneq ($(ISL_SRCDIR),)
src_toolchain: src_isl
src_isl: | $(ISL_SRCDIR)
	ln -sf "$(ISL_SRCDIR)" $@
endif

src_toolchain: src_binutils src_gcc
	rm -rf $@ $@.tmp
	mkdir $@.tmp
	cd $@.tmp && ln -sf ../src_binutils/* .
	cd $@.tmp && ln -sf ../src_gcc/* .
	$(if $(GMP_SRCDIR),cd $@.tmp && ln -sf ../src_gmp gmp)
	$(if $(MPC_SRCDIR),cd $@.tmp && ln -sf ../src_mpc mpc)
	$(if $(MPFR_SRCDIR),cd $@.tmp && ln -sf ../src_mpfr mpfr)
	$(if $(ISL_SRCDIR),cd $@.tmp && ln -sf ../src_isl isl)
	mv $@.tmp $@

obj_%:
	mkdir -p $@

obj_sysroot/include:
	mkdir -p $@

obj_sysroot/usr: | obj_sysroot
	ln -sf . $@

obj_sysroot/lib32: | obj_sysroot
	ln -sf lib $@

obj_sysroot/lib64: | obj_sysroot
	ln -sf lib $@

obj_toolchain/.lc_configured: | obj_toolchain src_toolchain
	cd obj_toolchain && ../src_toolchain/configure $(FULL_TOOLCHAIN_CONFIG)
	touch $@

obj_toolchain/gcc/.lc_built: | obj_toolchain/.lc_configured
	cd obj_toolchain && $(MAKE) MAKE="$(MAKE)" all-gcc
	touch $@

obj_musl/.lc_configured: | obj_musl src_musl
	cd obj_musl && ../src_musl/configure $(FULL_MUSL_CONFIG)
	touch $@

obj_sysroot/.lc_headers: | obj_musl/.lc_configured obj_sysroot
	cd obj_musl && $(MAKE) DESTDIR=$(CURDIR)/obj_sysroot install-headers
	touch $@

obj_toolchain/$(TARGET)/libgcc/libgcc.a: | obj_sysroot/.lc_headers
	cd obj_toolchain && $(MAKE) MAKE="$(MAKE) enable_shared=no" all-target-libgcc

obj_musl/.lc_built: | obj_musl/.lc_configured
	cd obj_musl && $(MAKE) $(MUSL_VARS)
	touch $@

obj_sysroot/.lc_libs: | obj_musl/.lc_built
	cd obj_musl && $(MAKE) $(MUSL_VARS) DESTDIR=$(CURDIR)/obj_sysroot install
	touch $@

obj_toolchain/.lc_built: | obj_toolchain/.lc_configured obj_toolchain/gcc/.lc_built
	cd obj_toolchain && $(MAKE) MAKE="$(MAKE)"
	touch $@

install-musl: | obj_musl/.lc_built
	cd obj_musl && $(MAKE) $(MUSL_VARS) DESTDIR=$(DESTDIR)$(OUTPUT)$(SYSROOT) install

install-toolchain: | obj_toolchain/.lc_built
	cd obj_toolchain && $(MAKE) MAKE="$(MAKE)" DESTDIR=$(DESTDIR)$(OUTPUT) install
	ln -sf $(TARGET)-gcc $(DESTDIR)$(OUTPUT)/bin/$(TARGET)-cc

all: kernel-headers
install: install-kernel-headers
kernel-headers: | obj_kernel_headers/.lc_built
src_kernel_headers: | $(LINUX_SRCDIR)
	ln -sf "$(LINUX_SRCDIR)" $@
obj_kernel_headers/.lc_built: | src_kernel_headers
	mkdir -p $(CURDIR)/obj_kernel_headers/staged
	cd src_kernel_headers && $(MAKE) ARCH=$(LINUX_ARCH) O=$(CURDIR)/obj_kernel_headers INSTALL_HDR_PATH=$(CURDIR)/obj_kernel_headers/staged headers_install
	find obj_kernel_headers/staged/include '(' -name .install -o -name ..install.cmd ')' -exec rm {} +
	touch $@
install-kernel-headers: | obj_kernel_headers/.lc_built
	mkdir -p $(DESTDIR)$(OUTPUT)$(SYSROOT)/include
	cp -R obj_kernel_headers/staged/include/* $(DESTDIR)$(OUTPUT)$(SYSROOT)/include

clean:
	rm -rf src_* obj_*
