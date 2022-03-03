//! @file lmdb.h

#ifndef U3_VERE_DB_LMDB_H
#define U3_VERE_DB_LMDB_H

#include <lmdb.h>

#include "c/all.h"

  /* lmdb api wrapper
  */

    /* u3_lmdb_init(): open lmdb at [pax_c], mmap up to [siz_i].
    */
      MDB_env*
      u3_lmdb_init(const c3_c* pax_c, size_t siz_i);

    /* u3_lmdb_exit(): close lmdb.
    */
      void
      u3_lmdb_exit(MDB_env* env_u);


    /* u3_lmdb_stat(): print env stats.
    */
      void
      u3_lmdb_stat(MDB_env* env_u, FILE* fil_u);

    /* u3_lmdb_gulf(): read first and last event numbers.
    */
      c3_o
      u3_lmdb_gulf(MDB_env* env_u, c3_d* low_d, c3_d* hig_d);

    /* u3_lmdb_read(): read [len_d] events starting at [eve_d].
    */
      c3_o
      u3_lmdb_read(MDB_env* env_u,
                   void*    ptr_v,
                   c3_d     eve_d,
                   c3_d     len_d,
                   c3_o  (*read_f)(void*, c3_d, size_t  , void*));

    /* u3_lmdb_save(): save [len_d] events starting at [eve_d].
    */
      c3_o
      u3_lmdb_save(MDB_env* env_u,
                   c3_d     eve_d,
                   c3_d     len_d,
                   void**   byt_p,
                   size_t*  siz_i);

    /* u3_lmdb_read_meta(): read by string from the META db.
    */
      void
      u3_lmdb_read_meta(MDB_env*    env_u,
                        void*       ptr_v,
                        const c3_c* key_c,
                        void     (*read_f)(void*, size_t, void*));

    /* u3_lmdb_save_meta(): save by string into the META db.
    */
      c3_o
      u3_lmdb_save_meta(MDB_env*    env_u,
                        const c3_c* key_c,
                        size_t      val_i,
                        void*       val_p);

#endif /* ifndef U3_VERE_DB_LMDB_H */
