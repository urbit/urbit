//! @file disk.h

#ifndef U3_VERE_DISK_H
#define U3_VERE_DISK_H

#include "c/portable.h"
#include "c/types.h"
#include "noun/aliases.h"
#include "vere/foil.h"

//! Completed event
typedef struct _u3_fact {
  c3_d             eve_d;               //  event number
  c3_l             mug_l;               //  kernel mug after
  u3_noun            job;               //  (pair date ovum)
  struct _u3_fact* nex_u;               //  next in queue
} u3_fact;

//! Serialized fact
typedef struct _u3_feat {
  c3_d             eve_d;
  size_t           len_i;
  c3_y*            hun_y;
  struct _u3_feat* nex_u;
} u3_feat;

//! Disk sync callbak.
typedef void (*u3_disk_news)(void*, c3_d, c3_o);

//! Manage event persistence.
typedef struct _u3_disk {
  u3_dire*         dir_u;       //!< main pier directory
  u3_dire*         urb_u;       //!< urbit system data
  u3_dire*         com_u;       //!< log directory
  c3_o             liv_o;       //!< live
  void*            mdb_u;       //!< lmdb environment.
  c3_d             sen_d;       //!< commit requested
  c3_d             dun_d;       //!< committed
  c3_w             hit_w[100];  //!< batch histogram
  struct {                      //!< new write queue
    u3_feat*       ent_u;       //!< queue entry (highest)
    u3_feat*       ext_u;       //!< queue exit (lowest)
  } put_u;
  struct {                      //!< write control
    union {                     //!< thread/request
      uv_work_t    ted_u;
      uv_req_t     req_u;
    };
    void*          ptr_v;       //!< async context
    u3_disk_news   don_f;       //!< async write cb
    c3_o           ted_o;       //!< c3y == active
    c3_o           ret_o;       //!< result
    c3_d           eve_d;       //!< first event
    c3_d           len_w;       //!< number of events
    c3_y*          byt_y[100];  //!< array of bytes
    size_t         siz_i[100];  //!< array of lengths
  } sav_u;
} u3_disk;

//! Pier metadata.
typedef struct _u3_meta {
  c3_d who_d[2];                    //  identity
  c3_o fak_o;                       //  fake bit
  c3_w lif_w;                       //  lifecycle length
} u3_meta;

//! Opaque event log iterator.
typedef struct _u3_disk_walk u3_disk_walk;

//! Load or create pier directories and event log.
u3_disk*
u3_disk_init(c3_c* pax_c);

//! Serialize an event for persistence.
c3_w
u3_disk_etch(u3_disk* log_u,
             u3_noun    eve,
             c3_l     mug_l,
             c3_y**   out_y);

//! Parse a persisted event buffer.
c3_o
u3_disk_sift(u3_disk* log_u,
             size_t   len_i,
             c3_y*    dat_y,
             c3_l*    mug_l,
             u3_noun*   job);

//! Print status info.
void
u3_disk_info(u3_disk* log_u);

//! Close [log_u] and dispose.
void
u3_disk_exit(u3_disk* log_u);

//! Read metadata.
c3_o
u3_disk_read_meta(u3_disk* log_u, u3_meta* met_u);

//! Save metadata.
c3_o
u3_disk_save_meta(u3_disk* log_u, const u3_meta* met_u);

//! Synchronously read a cons list of events.
u3_weak
u3_disk_read_list(u3_disk* log_u, c3_d eve_d, c3_d len_d, c3_l* mug_l);

//! Enqueue completed event list, without autocommit.
void
u3_disk_plan_list(u3_disk* log_u, u3_noun lit);

//! Commit planned events.
c3_o
u3_disk_sync(u3_disk* log_u);

//! Active autosync with callbacks.
void
u3_disk_async(u3_disk*     log_u,
              void*        ptr_v,
              u3_disk_news don_f);

//! Enqueue completed event for persistence.
void
u3_disk_plan(u3_disk* log_u, u3_fact* tac_u);

//! Init iterator.
u3_disk_walk*
u3_disk_walk_init(u3_disk* log_u,
                  c3_d     eve_d,
                  c3_d     len_d);

//! Check if live.
c3_o
u3_disk_walk_live(u3_disk_walk* wok_u);

//! Get next fact.
c3_o
u3_disk_walk_step(u3_disk_walk* wok_u, u3_fact* tac_u);

//! Close iterator.
void
u3_disk_walk_done(u3_disk_walk* wok_u);


#endif /* ifndef U3_VERE_DISK_H */
