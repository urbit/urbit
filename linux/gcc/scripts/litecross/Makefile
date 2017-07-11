
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
        --with-build-sysroot=$(CURDIR)/obj_sysroot \
	--enable-tls \
	--disable-libmudflap --disable-libsanitizer \
	--disable-gnu-indirect-function \
	--disable-libmpx \
	--enable-deterministic-archives \
	--enable-libstdcxx-time

FULL_MUSL_CONFIG = --prefix= --host=$(TARGET)

SYSROOT = /$(TARGET)
FULL_MUSL_CONFIG += CC="$(XGCC)" LIBCC="../obj_toolchain/$(TARGET)/libgcc/libgcc.a" 
MUSL_VARS = AR=../obj_toolchain/binutils/ar RANLIB=../obj_toolchain/binutils/ranlib
obj_musl/.lc_configured: | obj_toolchain/gcc/.lc_built
obj_musl/.lc_built: | obj_toolchain/$(TARGET)/libgcc/libgcc.a
obj_toolchain/.lc_built: | obj_sysroot/.lc_libs obj_sysroot/.lc_headers

.PHONY: all musl toolchain install-musl install-toolchain clean

obj_toolchain/.lc_configured: | obj_toolchain
	cd obj_toolchain && ../src_toolchain/configure $(FULL_TOOLCHAIN_CONFIG)
	touch $@

obj_toolchain/gcc/.lc_built: | obj_toolchain/.lc_configured
	cd obj_toolchain && $(MAKE) MAKE="$(MAKE)" all-gcc
	touch $@

obj_musl/.lc_configured: | obj_musl
	cd obj_musl && ../src_musl/configure $(FULL_MUSL_CONFIG)
	touch $@

obj_sysroot/.lc_headers: | obj_musl/.lc_configured
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

install: obj_musl/.lc_built obj_toolchain/.lc_built
	mkdir -p $(CURDIR)/obj_kernel_headers/staged
	cd src_kernel_headers && $(MAKE) ARCH=$(LINUX_ARCH) O=$(CURDIR)/obj_kernel_headers INSTALL_HDR_PATH=$(CURDIR)/obj_kernel_headers/staged headers_install
	find obj_kernel_headers/staged/include '(' -name .install -o -name ..install.cmd ')' -exec rm {} +
	cd obj_musl && $(MAKE) $(MUSL_VARS) DESTDIR=$(DESTDIR)$(OUTPUT)$(SYSROOT) install
	cd obj_toolchain && $(MAKE) MAKE="$(MAKE)" DESTDIR=$(DESTDIR)$(OUTPUT) install
	mkdir -p $(DESTDIR)$(OUTPUT)$(SYSROOT)/include
	cp -R obj_kernel_headers/staged/include/* $(DESTDIR)$(OUTPUT)$(SYSROOT)/include

