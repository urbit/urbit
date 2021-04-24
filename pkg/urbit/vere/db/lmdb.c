/* vere/db/lmdb.c
*/

#include <lmdb.h>

#include "c/portable.h"
#include "c/types.h"
#include "c/defs.h"

#include <vere/db/lmdb.h>

//  lmdb api wrapper
//
//    this module implements a simple persistence api on top of lmdb.
//    outside of its use of c3 type definitions, this module has no
//    dependence on anything u3, or on any library besides lmdb itself.
//
//    urbit requires very little from a persist store -- it merely
//    needs to store variable-length buffers in:
//
//      - a metadata store with c3_c (unsigned char) keys
//      - an event store with contiguous c3_d (uint64_t) keys
//
//    supported operations are as follows
//
//      - open/close an environment
//      - read/save metadata
//      - read the first and last event numbers
//      - read/save ranges of events
//

/* u3_lmdb_init(): open lmdb at [pax_c], mmap up to [siz_i].
*/
MDB_env*
u3_lmdb_init(const c3_c* pax_c, size_t siz_i)
{
  MDB_env* env_u;
  c3_w     ret_w;

  if ( (ret_w = mdb_env_create(&env_u)) ) {
    fprintf(stderr, "lmdb: init fail: %s\r\n", mdb_strerror(ret_w));
    return 0;
  }

  //  Our databases have two tables: META and EVENTS
  //
  if ( (ret_w = mdb_env_set_maxdbs(env_u, 2)) ) {
    fprintf(stderr, "lmdb: failed to set number of databases: %s\r\n",
                    mdb_strerror(ret_w));
    //  XX dispose env_u
    //
    return 0;
  }

  if ( (ret_w = mdb_env_set_mapsize(env_u, siz_i)) ) {
    fprintf(stderr, "lmdb: failed to set database size: %s\r\n",
                    mdb_strerror(ret_w));
    //  XX dispose env_u
    //
    return 0;
  }

  if ( (ret_w = mdb_env_open(env_u, pax_c, 0, 0664)) ) {
    fprintf(stderr, "lmdb: failed to open event log: %s\r\n",
                     mdb_strerror(ret_w));
    //  XX dispose env_u
    //
    return 0;
  }

  return env_u;
}

/* u3_lmdb_exit(): close lmdb.
*/
void
u3_lmdb_exit(MDB_env* env_u)
{
  mdb_env_close(env_u);
}

/* u3_lmdb_gulf(): read first and last event numbers.
*/
c3_o
u3_lmdb_gulf(MDB_env* env_u, c3_d* low_d, c3_d* hig_d)
{
  MDB_txn* txn_u;
  MDB_dbi  mdb_u;
  c3_w     ret_w;

  //  create a read-only transaction.
  //
  //    XX why no MDB_RDONLY?
  //
  if ( (ret_w = mdb_txn_begin(env_u, 0, 0, &txn_u)) ) {
    fprintf(stderr, "lmdb: gulf: txn_begin fail: %s\r\n", mdb_strerror(ret_w));
    return c3n;
  }

  //  open the database in the transaction
  //
  {
    c3_w ops_w = MDB_CREATE | MDB_INTEGERKEY;

    if ( (ret_w = mdb_dbi_open(txn_u, "EVENTS", ops_w, &mdb_u)) ) {
      fprintf(stderr, "lmdb: gulf: dbi_open fail: %s\r\n", mdb_strerror(ret_w));
      //  XX confirm
      //
      mdb_txn_abort(txn_u);
      return c3n;
    }
  }

  {
    MDB_cursor* cur_u;
    MDB_val     key_u;
    MDB_val     val_u;
    c3_d        fir_d, las_d;

    //  creates a cursor to point to the last event
    //
    if ( (ret_w = mdb_cursor_open(txn_u, mdb_u, &cur_u)) ) {
      fprintf(stderr, "lmdb: gulf: cursor_open fail: %s\r\n",
                      mdb_strerror(ret_w));
      //  XX confirm
      //
      mdb_txn_abort(txn_u);
      return c3n;
    }

    //  read with the cursor from the start of the database
    //
    ret_w = mdb_cursor_get(cur_u, &key_u, &val_u, MDB_FIRST);

    if ( MDB_NOTFOUND == ret_w ) {
      *low_d = 0;
      *hig_d = 0;
      mdb_cursor_close(cur_u);
      mdb_txn_abort(txn_u);
      return c3y;
    }
    else if ( ret_w ) {
      fprintf(stderr, "lmdb: gulf: head fail: %s\r\n",
                      mdb_strerror(ret_w));
      mdb_cursor_close(cur_u);
      mdb_txn_abort(txn_u);
      return c3n;
    }
    else {
      fir_d = *(c3_d*)key_u.mv_data;
    }

    //  read with the cursor from the end of the database
    //
    ret_w = mdb_cursor_get(cur_u, &key_u, &val_u, MDB_LAST);

    if ( !ret_w ) {
      las_d = *(c3_d*)key_u.mv_data;
    }

    //  clean up unconditionally, we're done
    //
    mdb_cursor_close(cur_u);
    mdb_txn_abort(txn_u);

    if ( ret_w ) {
      fprintf(stderr, "lmdb: gulf: last fail: %s\r\n", mdb_strerror(ret_w));
      return c3n;
    }
    else {
      *low_d = fir_d;
      *hig_d = las_d;
      return c3y;
    }
  }
}

/* u3_lmdb_walk_init(): initialize db iterator.
*/
c3_o
u3_lmdb_walk_init(MDB_env*      env_u,
                  u3_lmdb_walk* itr_u,
                  c3_d          nex_d,
                  c3_d          las_d)
{
  //  XX assumes little-endian
  //
  MDB_val key_u = { .mv_size = sizeof(c3_d), .mv_data = &nex_d };
  MDB_val val_u;
  c3_w    ops_w, ret_w;

  itr_u->red_o = c3n;
  itr_u->nex_d = nex_d;
  itr_u->las_d = las_d;

  //  create a read-only transaction.
  //
  ops_w = MDB_RDONLY;
  if ( (ret_w = mdb_txn_begin(env_u, 0, ops_w, &itr_u->txn_u)) ) {
    fprintf(stderr, "lmdb: read txn_begin fail: %s\r\n", mdb_strerror(ret_w));
    return c3n;
  }
  //  open the database in the transaction
  //
  ops_w = MDB_CREATE | MDB_INTEGERKEY;
  if ( (ret_w = mdb_dbi_open(itr_u->txn_u, "EVENTS", ops_w, &itr_u->mdb_u)) ) {
    fprintf(stderr, "lmdb: read: dbi_open fail: %s\r\n", mdb_strerror(ret_w));
    //  XX confirm
    //
    mdb_txn_abort(itr_u->txn_u);
    return c3n;
  }

  //  creates a cursor to iterate over keys starting at [eve_d]
  //
  if ( (ret_w = mdb_cursor_open(itr_u->txn_u, itr_u->mdb_u, &itr_u->cur_u)) ) {
    fprintf(stderr, "lmdb: read: cursor_open fail: %s\r\n",
                    mdb_strerror(ret_w));
    //  XX confirm
    //
    mdb_txn_abort(itr_u->txn_u);
    return c3n;
  }

  //  set the cursor to the position of [eve_d]
  //
  ops_w = MDB_SET_KEY;
  if ( (ret_w = mdb_cursor_get(itr_u->cur_u, &key_u, &val_u, ops_w)) ) {
    fprintf(stderr, "lmdb: read: initial cursor_get failed at %" PRIu64
                    ": %s\r\n",
                    nex_d,
                    mdb_strerror(ret_w));
    mdb_cursor_close(itr_u->cur_u);
    //  XX confirm
    //
    mdb_txn_abort(itr_u->txn_u);
    return c3n;
  }

  return c3y;
}

/* u3_lmdb_walk_next(): synchronously read next event from iterator.
*/
c3_o
u3_lmdb_walk_next(u3_lmdb_walk* itr_u, size_t* len_i, void** buf_v)
{
  MDB_val key_u, val_u;
  c3_w    ret_w, ops_w;

  c3_assert( itr_u->nex_d <= itr_u->las_d );

  ops_w = ( c3y == itr_u->red_o ) ? MDB_NEXT : MDB_GET_CURRENT;
  if ( (ret_w = mdb_cursor_get(itr_u->cur_u, &key_u, &val_u, ops_w)) ) {
    fprintf(stderr, "lmdb: walk error: %s\r\n", mdb_strerror(ret_w));
    return c3n;
  }

  //  sanity check: ensure contiguous event numbers
  //
  if ( *(c3_d*)key_u.mv_data != itr_u->nex_d ) {
    fprintf(stderr, "lmdb: read gap: expected %" PRIu64
                    ", received %" PRIu64 "\r\n",
                    itr_u->nex_d,
                    *(c3_d*)key_u.mv_data);
    return c3n;
  }

  *len_i = val_u.mv_size;
  *buf_v = val_u.mv_data;

  itr_u->nex_d++;
  itr_u->red_o = c3y;

  return c3y;
}

/* u3_lmdb_walk_done(): close iterator.
*/
void
u3_lmdb_walk_done(u3_lmdb_walk* itr_u)
{
  mdb_cursor_close(itr_u->cur_u);
  mdb_txn_abort(itr_u->txn_u);
}

/* u3_lmdb_read(): read [len_d] events starting at [eve_d].
*/
c3_o
u3_lmdb_read(MDB_env* env_u,
             void*    ptr_v,
             c3_d     eve_d,
             c3_d     len_d,
             c3_o   (*read_f)(void*, c3_d, size_t, void*))
{
  MDB_txn* txn_u;
  MDB_dbi  mdb_u;
  c3_w     ret_w;

  //  create a read-only transaction.
  //
  if ( (ret_w = mdb_txn_begin(env_u, 0, MDB_RDONLY, &txn_u)) ) {
    fprintf(stderr, "lmdb: read txn_begin fail: %s\r\n", mdb_strerror(ret_w));
    return c3n;
  }

  //  open the database in the transaction
  //
  {
    c3_w ops_w = MDB_CREATE | MDB_INTEGERKEY;

    if ( (ret_w = mdb_dbi_open(txn_u, "EVENTS", ops_w, &mdb_u)) ) {
      fprintf(stderr, "lmdb: read: dbi_open fail: %s\r\n", mdb_strerror(ret_w));
      //  XX confirm
      //
      mdb_txn_abort(txn_u);
      return c3n;
    }
  }


  {
    MDB_cursor* cur_u;
    MDB_val     val_u;
    //  set the initial key to [eve_d]
    //
    MDB_val     key_u = { .mv_size = sizeof(c3_d), .mv_data = &eve_d };

    //  creates a cursor to iterate over keys starting at [eve_d]
    //
    if ( (ret_w = mdb_cursor_open(txn_u, mdb_u, &cur_u)) ) {
      fprintf(stderr, "lmdb: read: cursor_open fail: %s\r\n",
                      mdb_strerror(ret_w));
      //  XX confirm
      //
      mdb_txn_abort(txn_u);
      return c3n;
    }

    //  set the cursor to the position of [eve_d]
    //
    if ( (ret_w = mdb_cursor_get(cur_u, &key_u, &val_u, MDB_SET_KEY)) ) {
      fprintf(stderr, "lmdb: read: initial cursor_get failed at %" PRIu64 ": %s\r\n",
                      eve_d,
                      mdb_strerror(ret_w));
      mdb_cursor_close(cur_u);
      //  XX confirm
      //
      mdb_txn_abort(txn_u);
      return c3n;
    }

    //  load up to [len_d] events, iterating forward across the cursor.
    //
    {
      c3_o ret_o = c3y;
      c3_d   i_d;

      for ( i_d = 0; (ret_w != MDB_NOTFOUND) && (i_d < len_d); ++i_d) {
        c3_d cur_d = (eve_d + i_d);
        if ( sizeof(c3_d) != key_u.mv_size ) {
          fprintf(stderr, "lmdb: read: invalid key size\r\n");
          ret_o = c3n;
          break;
        }

        //  sanity check: ensure contiguous event numbers
        //
        if ( *(c3_d*)key_u.mv_data != cur_d ) {
          fprintf(stderr, "lmdb: read gap: expected %" PRIu64
                          ", received %" PRIu64 "\r\n",
                          cur_d,
                          *(c3_d*)key_u.mv_data);
          ret_o = c3n;
          break;
        }

        //  invoke read callback with [val_u]
        //
        if ( c3n == read_f(ptr_v, cur_d, val_u.mv_size, val_u.mv_data) ) {
          ret_o = c3n;
          break;
        }

        //  read the next event from the cursor
        //
        if (  (ret_w = mdb_cursor_get(cur_u, &key_u, &val_u, MDB_NEXT))
           && (MDB_NOTFOUND != ret_w) )
        {
          fprintf(stderr, "lmdb: read: error: %s\r\n",
                          mdb_strerror(ret_w));
          ret_o = c3n;
          break;
        }
      }

      mdb_cursor_close(cur_u);

      //  read-only transactions are aborted when complete
      //
      mdb_txn_abort(txn_u);

      return ret_o;
    }
  }
}

/* u3_lmdb_save(): save [len_d] events starting at [eve_d].
*/
c3_o
u3_lmdb_save(MDB_env* env_u,
             c3_d     eve_d,               //  first event
             c3_d     len_d,               //  number of events
             void**   byt_p,               //  array of bytes
             size_t*  siz_i)               //  array of lengths
{
  MDB_txn* txn_u;
  MDB_dbi  mdb_u;
  c3_w     ret_w;

  //  create a write transaction
  //
  if ( (ret_w = mdb_txn_begin(env_u, 0, 0, &txn_u)) ) {
    fprintf(stderr, "lmdb: write: txn_begin fail: %s\r\n", mdb_strerror(ret_w));
    return c3n;
  }

  //  opens the database in the transaction
  //
  {
    c3_w ops_w = MDB_CREATE | MDB_INTEGERKEY;

    if ( (ret_w = mdb_dbi_open(txn_u, "EVENTS", ops_w, &mdb_u)) ) {
      fprintf(stderr, "lmdb: write: dbi_open fail: %s\r\n", mdb_strerror(ret_w));
      mdb_txn_abort(txn_u);
      return c3n;
    }
  }

  //  write every event in the batch
  //
  {
    c3_w ops_w = MDB_NOOVERWRITE;
    c3_d las_d = (eve_d + len_d);
    c3_d key_d, i_d;

    for ( i_d = 0; i_d < len_d; ++i_d) {
      key_d = eve_d + i_d;

      {
        MDB_val key_u = { .mv_size = sizeof(c3_d), .mv_data = &key_d };
        MDB_val val_u = { .mv_size = siz_i[i_d],   .mv_data = byt_p[i_d] };

        if ( (ret_w = mdb_put(txn_u, mdb_u, &key_u, &val_u, ops_w)) ) {
          fprintf(stderr, "lmdb: write failed on event %" PRIu64 "\r\n", key_d);
          mdb_txn_abort(txn_u);
          return c3n;
        }
      }
    }
  }

  //  commit transaction
  //
  if ( (ret_w = mdb_txn_commit(txn_u)) ) {
    fprintf(stderr, "lmdb: write failed: %s\r\n", mdb_strerror(ret_w));
    return c3n;
  }

  return c3y;
}

/* u3_lmdb_read_meta(): read by string from the META db.
*/
void
u3_lmdb_read_meta(MDB_env*    env_u,
                  void*       ptr_v,
                  const c3_c* key_c,
                  void (*read_f)(void*, size_t, void*))
{
  MDB_txn* txn_u;
  MDB_dbi  mdb_u;
  c3_w     ret_w;

  //  create a read transaction
  //
  if ( (ret_w = mdb_txn_begin(env_u, 0, MDB_RDONLY, &txn_u)) ) {
    fprintf(stderr, "lmdb: meta read: txn_begin fail: %s\r\n",
                    mdb_strerror(ret_w));
    return read_f(ptr_v, 0, 0);
  }

  //  open the database in the transaction
  //
  if ( (ret_w =  mdb_dbi_open(txn_u, "META", 0, &mdb_u)) ) {
    fprintf(stderr, "lmdb: meta read: dbi_open fail: %s\r\n",
                    mdb_strerror(ret_w));
    mdb_txn_abort(txn_u);
    return read_f(ptr_v, 0, 0);
  }

  //  read by string key, invoking callback with result
  {
    MDB_val key_u = { .mv_size = strlen(key_c), .mv_data = (void*)key_c };
    MDB_val val_u;

    if ( (ret_w = mdb_get(txn_u, mdb_u, &key_u, &val_u)) ) {
      fprintf(stderr, "lmdb: read failed: %s\r\n", mdb_strerror(ret_w));
      mdb_txn_abort(txn_u);
      return read_f(ptr_v, 0, 0);
    }
    else {
      read_f(ptr_v, val_u.mv_size, val_u.mv_data);

      //  read-only transactions are aborted when complete
      //
      mdb_txn_abort(txn_u);
    }
  }
}

/* u3_lmdb_save_meta(): save by string into the META db.
*/
c3_o
u3_lmdb_save_meta(MDB_env*    env_u,
                  const c3_c* key_c,
                  size_t      val_i,
                  void*       val_p)
{
  MDB_txn* txn_u;
  MDB_dbi  mdb_u;
  c3_w     ret_w;

  //  create a write transaction
  //
  if ( (ret_w = mdb_txn_begin(env_u, 0, 0, &txn_u)) ) {
    fprintf(stderr, "lmdb: meta write: txn_begin fail: %s\r\n",
                    mdb_strerror(ret_w));
    return c3n;
  }

  //  opens the database in the transaction
  //
  if ( (ret_w = mdb_dbi_open(txn_u, "META", MDB_CREATE, &mdb_u)) ) {
    fprintf(stderr, "lmdb: meta write: dbi_open fail: %s\r\n",
                    mdb_strerror(ret_w));
    mdb_txn_abort(txn_u);
    return c3n;
  }

  //  put value by string key
  //
  {
    MDB_val key_u = { .mv_size = strlen(key_c), .mv_data = (void*)key_c };
    MDB_val val_u = { .mv_size = val_i,         .mv_data = val_p };

    if ( (ret_w = mdb_put(txn_u, mdb_u, &key_u, &val_u, 0)) ) {
      fprintf(stderr, "lmdb: write failed: %s\r\n", mdb_strerror(ret_w));
      mdb_txn_abort(txn_u);
      return c3n;
    }
  }

  //  commit txn
  //
  if ( (ret_w = mdb_txn_commit(txn_u)) ) {
    fprintf(stderr, "lmdb: meta write: commit failed: %s\r\n",
                    mdb_strerror(ret_w));
    return c3n;
  }

  return c3y;
}
