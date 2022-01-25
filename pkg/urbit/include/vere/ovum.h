//! @file ovum.h

#ifndef U3_VERE_OVUM_H
#define U3_VERE_OVUM_H

#include "c/portable.h"
#include "c/types.h"
#include "noun/aliases.h"

//! u3_ovum lifecycle events
typedef enum {
  u3_ovum_drop = 0,  //!< unplanned
  u3_ovum_work = 1,  //!< begun
  u3_ovum_done = 2,  //!< complete
} u3_ovum_news;

struct _u3_ovum;

//! News callback.
typedef void (*u3_ovum_peer)(struct _u3_ovum*, u3_ovum_news);

//! Failure callback.
typedef void (*u3_ovum_bail)(struct _u3_ovum*, u3_noun);

//! Potential event.
typedef struct _u3_ovum {
  void*            ptr_v;  //!< context
  c3_w             try_w;  //!< retry count
  c3_w             mil_w;  //!< timeout ms
  u3_noun            tar;  //!< target (in arvo)
  u3_noun            wir;  //!< wire
  u3_noun            cad;  //!< card
  struct {                 //!< spinner:
    u3_atom          lab;  //!<   label
    c3_o           del_o;  //!<   delay (c3y)
  } pin_u;
  struct {                 //!< optional callbacks:
    u3_ovum_peer  news_f;  //!<   progress
    u3_ovum_bail  bail_f;  //!<   failure
  } cb_u;
  struct _u3_ovum* pre_u;  //!< previous ovum
  struct _u3_ovum* nex_u;  //!< next ovum
  struct _u3_auto* car_u;  //!< backpointer to i/o driver
} u3_ovum;

//! Initialize an unlinked potential event.
u3_ovum*
u3_ovum_init(c3_w     mil_w,
             u3_noun    tar,
             u3_noun    wir,
             u3_noun    cad);

//! Dispose of an unlinked potential event.
void
u3_ovum_free(u3_ovum *egg_u);


#endif /* ifndef U3_VERE_OVUM_H */
