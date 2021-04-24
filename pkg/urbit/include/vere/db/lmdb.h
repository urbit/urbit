/* include/vere/db/lmdb-impl.h
*/

#include <lmdb.h>

  /* lmdb api wrapper
  */

    /* u3_lmdb_iter: event iterator
    */
    typedef struct _u3_lmdb_walk {
      MDB_txn*    txn_u;  //  transaction handle
      MDB_dbi     mdb_u;  //  db handle
      MDB_cursor* cur_u;  //  db cursor
      c3_o        red_o;  //  have we read from this yet?
      c3_d        nex_d;  //  next event number
      c3_d        las_d;  //  final event number, inclusive
    } u3_lmdb_walk;

    /* u3_lmdb_init(): open lmdb at [pax_c], mmap up to [siz_i].
    */
      MDB_env*
      u3_lmdb_init(const c3_c* pax_c, size_t siz_i);

    /* u3_lmdb_exit(): close lmdb.
    */
      void
      u3_lmdb_exit(MDB_env* env_u);

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

    /* u3_lmdb_walk_init(): initialize db iterator.
    */
      c3_o
      u3_lmdb_walk_init(MDB_env*      env_u,
                        u3_lmdb_walk* itr_u,
                        c3_d          nex_d,
                        c3_d          las_d);

    /* u3_lmdb_walk_next(): synchronously read next event from iterator.
    */
      c3_o
      u3_lmdb_walk_next(u3_lmdb_walk* itr_u, size_t* len_i, void** buf_v);

    /* u3_lmdb_walk_done(): close iterator.
    */
      void
      u3_lmdb_walk_done(u3_lmdb_walk* itr_u);