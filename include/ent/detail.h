#ifndef _LIBENT_DETAIL_H
#define _LIBENT_DETAIL_H

#if defined _WIN32 || defined __CYGWIN__
# ifdef WIN_EXPORT
#  ifdef __GNUC__
#   define ENT_EXPORT __attribute__ ((dllexport))
#  else
#   define ENT_EXPORT __declspec(dllexport)
#  endif
# else
#  ifdef __GNUC__
#   define ENT_EXPORT __attribute__ ((dllimport))
#  else
#   define ENT_EXPORT __declspec(dllimport)
#  endif
# endif
#else
# if __GNUC__ >= 4
#  define ENT_EXPORT __attribute__ ((visibility ("default")))
# else
#  define ENT_EXPORT
# endif
#endif

#endif /* _LIBENT_DETAIL_H */
