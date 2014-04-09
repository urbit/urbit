## Process this file with automake to produce Makefile.in

ACLOCAL_AMFLAGS		= -I autotools
EXTRA_DIST		= INSTALL configure.sh prepare.sh
dist_doc_DATA		= README COPYING LICENSE.re2

## --------------------------------------------------------------------

cre2_CURRENT		= @cre2_VERSION_INTERFACE_CURRENT@
cre2_REVISION		= @cre2_VERSION_INTERFACE_REVISION@
cre2_AGE		= @cre2_VERSION_INTERFACE_AGE@

include_HEADERS		= src/cre2.h

lib_LTLIBRARIES		= libcre2.la
libcre2_la_LDFLAGS	= -version-info $(cre2_CURRENT):$(cre2_REVISION):$(cre2_AGE)
libcre2_la_SOURCES	= src/cre2.cpp

## --------------------------------------------------------------------

AM_MAKEINFOFLAGS	= --no-split

info_TEXINFOS		= doc/cre2.texi
doc_cre2_TEXINFOS	= doc/fdl-1.3.texi

## --------------------------------------------------------------------

check_PROGRAMS		= \
	tests/test-version			\
	tests/test-options			\
	tests/test-rex-alloc			\
	tests/test-matching			\
	tests/test-easy-matching		\
	tests/test-full-match			\
	tests/test-partial-match		\
	tests/test-consume-match		\
	tests/test-find-and-consume-match	\
	tests/test-replace			\
	tests/test-misc

AM_CPPFLAGS		= -I$(top_srcdir)/src
LDADD			= libcre2.la
TESTS			= $(check_PROGRAMS)

installcheck-local: $(check_PROGRAMS)
	for f in $(check_PROGRAMS); do $$f; done

### end of file
