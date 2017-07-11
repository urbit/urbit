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

install:
	cd obj_toolchain && ../src_toolchain/configure $(FULL_TOOLCHAIN_CONFIG)
	cd obj_toolchain && $(MAKE) MAKE="$(MAKE)" all-gcc
	cd obj_musl && ../src_musl/configure $(FULL_MUSL_CONFIG)
	cd obj_musl && $(MAKE) DESTDIR=$(CURDIR)/obj_sysroot install-headers
	cd obj_toolchain && $(MAKE) MAKE="$(MAKE) enable_shared=no" all-target-libgcc
	cd obj_musl && $(MAKE) $(MUSL_VARS)
	cd obj_musl && $(MAKE) $(MUSL_VARS) DESTDIR=$(CURDIR)/obj_sysroot install
	cd obj_toolchain && $(MAKE) MAKE="$(MAKE)"
	mkdir -p $(CURDIR)/obj_kernel_headers/staged
	cd src_kernel_headers && $(MAKE) ARCH=$(LINUX_ARCH) O=$(CURDIR)/obj_kernel_headers INSTALL_HDR_PATH=$(CURDIR)/obj_kernel_headers/staged headers_install
	find obj_kernel_headers/staged/include '(' -name .install -o -name ..install.cmd ')' -exec rm {} +
	cd obj_musl && $(MAKE) $(MUSL_VARS) DESTDIR=$(DESTDIR)$(OUTPUT)$(SYSROOT) install
	cd obj_toolchain && $(MAKE) MAKE="$(MAKE)" DESTDIR=$(DESTDIR)$(OUTPUT) install
	mkdir -p $(DESTDIR)$(OUTPUT)$(SYSROOT)/include
	cp -R obj_kernel_headers/staged/include/* $(DESTDIR)$(OUTPUT)$(SYSROOT)/include

