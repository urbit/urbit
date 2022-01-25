//! @file peek.h

#ifndef U3_VERE_PEEK_H
#define U3_VERE_PEEK_H

#include "c/portable.h"
#include "c/types.h"
#include "noun/aliases.h"

//! Namespace read response callback.
typedef void (*u3_peek_cb)(void*, u3_noun);

//! Kinds of proto-peek.
typedef enum {
  u3_pico_full = 0,
  u3_pico_once = 1,
} u3_pico_type;

//! proto-peek.
typedef struct _u3_pico {
  struct _u3_pico* nex_u;  //!< next in queue
  void*            ptr_v;  //!< context
  u3_peek_cb       fun_f;  //!< callback
  u3_noun            gan;  //!< leakset
  u3_pico_type     typ_e;  //!< type-tagged
  union {
    u3_noun          ful;  //!< (each path [%beam term beam])
    struct {               //!< once:
      c3_m         car_m;  //!<   care
      u3_atom        des;  //!<   desk
      u3_noun        pax;  //!<   /path
    } las_u;
  };
} u3_pico;

//! Namespace read request.
typedef struct _u3_peek {
  void*            ptr_v;  //!< context
  u3_peek_cb       fun_f;  //!< callback
  u3_pico_type     typ_e;  //!< type
  u3_noun            sam;  //!< +peek sample
} u3_peek;

#endif /* ifndef U3_VERE_PEEK_H */
