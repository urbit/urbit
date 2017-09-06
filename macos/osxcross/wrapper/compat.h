#ifdef __clang__
#if __has_include(<__config>)
#include <__config>
#endif

#ifndef _LIBCPP_CONFIG
// NetBSD's libstdc++ 4.5.3
// requires this
#undef __GXX_EXPERIMENTAL_CXX0X__
#endif
#endif

#ifndef __has_builtin
#define __has_builtin(x) 0
#endif
