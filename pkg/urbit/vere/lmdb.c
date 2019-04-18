/* vere/lmdb.c
*/

#include "all.h"

#include <uv.h>
#include <lmdb.h>

// Event log persistence for Urbit
//
// Persistence works by having an lmdb environment opened on the main
// thread. This environment is used to create read-only transactions
// synchronously when needed.
//
// But most of the lmdb operates asynchronously in the uv worker pool. Since
// individual transactions are bound to threads, we perform all blocking
// writing on worker threads. We do this so we can perform event batching on
// the main thread instead of blocking it; the main thread is still a libuv
// loop.
//
// There are several metadata writes which we can perform from whatever thread
// because we they're inherently stop-the-world blocking. Thankfully, these
// should be "cheap" and "rare".

// Opens up a log environment. This can eventually be made 
//
// Precondition: log_path points to an already created directory
//
MDB_env* u3m_lmdb_init(const char* log_path)
{
  MDB_env* env = 0;
  c3_w ret_w = mdb_env_create(&env);
  if (ret_w != 0) {
    u3l_log("lmdb: init fail: %s\n", mdb_strerror(ret_w));
    return 0;
  }

  // Our databases have up to three tables: META, EVENTS, and GRAINS.
  ret_w = mdb_env_set_maxdbs(env, 3);
  if (ret_w != 0) {
    u3l_log("lmdb: failed to set number of databases: %s\n", mdb_strerror(ret_w));
    return 0;
  }

  ret_w = mdb_env_open(env, log_path, 0, 0664);
  if (ret_w != 0) {
    u3l_log("lmdb: failed to open event log: %s\n", mdb_strerror(ret_w));
    return 0;
  }

  return env;
}

void u3m_lmdb_shutdown(MDB_env* env)
{
  mdb_env_close(env);
}

// Writes a key/value pair to a specific database as part of a transaction.
//
// The raw version doesn't take ownership of either key/value and performs no
// nock calculations, so it is safe to call from any thread.
//
static
void _perform_put_on_databse_raw(MDB_txn* transaction_u,
                                 MDB_dbi database_u,
                                 void* key,
                                 size_t key_len,
                                 void* value,
                                 size_t value_len) {
  MDB_val key_val, value_val;

  key_val.mv_size = key_len;
  key_val.mv_data = key;

  value_val.mv_size = value_len;
  value_val.mv_data = value;

  c3_w ret_w = mdb_put(transaction_u, database_u, &key_val, &value_val, 0);
  if (ret_w != 0) {
    u3l_log("lmdb: write failed: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail);
  }
}

static
void _perform_get_on_databse_raw(MDB_txn* transaction_u,
                                 MDB_dbi database_u,
                                 void* key,
                                 size_t key_len,
                                 MDB_val* value) {
  MDB_val key_val;
  key_val.mv_size = key_len;
  key_val.mv_data = key;

  c3_w ret_w = mdb_get(transaction_u, database_u, &key_val, value);
  if (ret_w != 0) {
    u3l_log("lmdb: read failed: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail);
  }
}

static
void _perform_put_on_databse_noun(MDB_txn* transaction_u,
                                  MDB_dbi database_u,
                                  c3_c* key,
                                  u3_noun noun) {
  // jam noun into an atom representation
  u3_atom mat = u3ke_jam(noun);

  // copy the jammed noun into a byte buffer we can hand to lmdb
  c3_w  len_w   = u3r_met(3, mat);
  c3_y* bytes_y = (c3_y*) malloc(len_w);
  u3r_bytes(0, len_w, bytes_y, mat);

  _perform_put_on_databse_raw(transaction_u,
                              database_u,
                              key, strlen(key),
                              bytes_y, len_w);

  free(bytes_y);
  u3z(mat);
}

static
void _perform_get_on_databse_noun(MDB_txn* transaction_u,
                                  MDB_dbi database_u,
                                  c3_c* key,
                                  u3_noun* noun) {
  MDB_val value_val;
  _perform_get_on_databse_raw(transaction_u,
                              database_u,
                              key, strlen(key),
                              &value_val);

  // Take the bytes and cue them.
  u3_atom raw_atom = u3i_bytes(value_val.mv_size, value_val.mv_data);
  *noun = u3qe_cue(raw_atom);
}

// Implementation of u3m_lmdb_write_events() called on a worker thread.
//
static void u3m_lmdb_write_events_cb(uv_work_t* req) {

}

// Implementation of u3m_lmdb_write_events() called on the main loop thread
// after the worker thread event completes.
//
static void u3m_lmdb_write_events_after_cb(uv_work_t* req, int status) {
  // Calls the user provided cb


  // Cleans up req->data.

}


//  u3m_lmdb_write_events(): Asynchronously writes events to the database.
//
//  This writes all the passed in events along with log metadata updates to the
//  database as a single transaction on a worker thread. Once the transaction
//  is completed, it calls the passed in callback on the main loop thread.
//
void u3m_lmdb_write_events(MDB_env* environment,
                           int first_event_id,
                           u3_noun* events,
                           int event_count
                           /*,  TODO: ADD CALLBACK */
                           )
{
  // Packs up all the events for transport.
  //

  uv_work_t req;
  req.data = /* malloc() */ 0;

  uv_queue_work(uv_default_loop(),
                &req,
                u3m_lmdb_write_events_cb,
                u3m_lmdb_write_events_after_cb);
}


// Writes the event log identity information.
//
// We have a secondary database (table) in this environment named META where we
// read/write identity information from/to.
//
void u3m_lmdb_write_identity(MDB_env* environment,
                             u3_noun who,
                             u3_noun is_fake,
                             u3_noun life)
{
  // Creates the write transaction.
  MDB_txn* transaction_u;
  c3_w ret_w = mdb_txn_begin(environment,
                             (MDB_txn *) NULL,
                             0, /* flags */
                             &transaction_u);
  if (0 != ret_w) {
    u3l_log("lmdb: txn_begin fail: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail);
  }

  // Opens the database as part of the transaction.
  c3_w flags_w = MDB_CREATE;
  MDB_dbi database_u;
  ret_w = mdb_dbi_open(transaction_u,
                       "META",
                       flags_w,
                       &database_u);
  if (0 != ret_w) {
    u3l_log("lmdb: dbi_open fail: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail);
  }

  _perform_put_on_databse_noun(transaction_u, database_u, "who", who);
  _perform_put_on_databse_noun(transaction_u, database_u, "is-fake", is_fake);
  _perform_put_on_databse_noun(transaction_u, database_u, "life", life);

  ret_w = mdb_txn_commit(transaction_u);
  if (0 != ret_w) {
    u3l_log("lmdb: failed to commit transaction: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail);
  }
}

// Reads the event log identity information.
//
void u3m_lmdb_read_identity(MDB_env* environment,
                            u3_noun* who,
                            u3_noun* is_fake,
                            u3_noun* life) {
  // Creates the write transaction.
  MDB_txn* transaction_u;
  c3_w ret_w = mdb_txn_begin(environment,
                             (MDB_txn *) NULL,
                             0, /* flags */
                             &transaction_u);
  if (0 != ret_w) {
    u3l_log("lmdb: txn_begin fail: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail);
  }

  // Opens the database as part of the transaction.
  MDB_dbi database_u;
  ret_w =  mdb_dbi_open(transaction_u,
                        "META",
                        0,
                        &database_u);
  if (0 != ret_w) {
    u3l_log("lmdb: dbi_open fail: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail);
  }

  _perform_get_on_databse_noun(transaction_u, database_u, "who", who);
  _perform_get_on_databse_noun(transaction_u, database_u, "is-fake", is_fake);
  _perform_get_on_databse_noun(transaction_u, database_u, "life", life);

  // Read-only transactions are aborted since we don't need to record the fact
  // that we performed a read.
  mdb_txn_abort(transaction_u);
}
