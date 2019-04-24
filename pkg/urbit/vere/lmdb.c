/* vere/lmdb.c
*/

#include "all.h"

#include <uv.h>
#include <lmdb.h>

#include "vere/vere.h"

// Event log persistence for Urbit
//
// Persistence works by having an lmdb environment opened on the main
// thread. This environment is used to create read-only transactions
// synchronously when needed.
//
// But the majority of lmdb writes operate asynchronously in the uv worker
// pool. Since individual transactions are bound to threads, we perform all
// blocking writing on worker threads.
//
// We perform the very first metadata writes on the main thread because we
// can't do anything until they persist.

/* u3_lmdb_init(): Opens up a log environment
**
** Precondition: log_path points to an already created directory
*/
MDB_env* u3_lmdb_init(const char* log_path)
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

  // TODO: Start with a gigabyte for the event log.
  //
  ret_w = mdb_env_set_mapsize(env, 1024 * 1024 * 1024);
  if (ret_w != 0) {
    u3l_log("lmdb: failed to set database size: %s\n", mdb_strerror(ret_w));
    return 0;
  }

  ret_w = mdb_env_open(env, log_path, 0, 0664);
  if (ret_w != 0) {
    u3l_log("lmdb: failed to open event log: %s\n", mdb_strerror(ret_w));
    return 0;
  }

  return env;
}

/* u3_lmdb_shutdown(): Shuts down lmdb
*/
void u3_lmdb_shutdown(MDB_env* env)
{
  mdb_env_close(env);
}

/* _perform_put_on_databse_raw(): Writes a key/value pair to a specific
** database as part of a transaction.
**
** The raw version doesn't take ownership of either key/value and performs no
** nock calculations, so it is safe to call from any thread.
*/
static
void _perform_put_on_databse_raw(MDB_txn* transaction_u,
                                 MDB_dbi database_u,
                                 c3_w flags,
                                 void* key,
                                 size_t key_len,
                                 void* value,
                                 size_t value_len) {
  MDB_val key_val, value_val;

  key_val.mv_size = key_len;
  key_val.mv_data = key;

  value_val.mv_size = value_len;
  value_val.mv_data = value;

  c3_w ret_w = mdb_put(transaction_u, database_u, &key_val, &value_val, flags);
  if (ret_w != 0) {
    u3l_log("lmdb: write failed: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail);
  }
}

/* _perform_get_on_databse_raw(): Reads a key/value pair to a specific
** database as part of a transaction.
*/
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

/* _perform_put_on_databse_noun(): Writes a noun to the database.
**
** This requires access to the loom so it must only be run from the libuv
** thread.
*/
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
                              0,
                              key, strlen(key),
                              bytes_y, len_w);

  free(bytes_y);
  u3z(mat);
}

/* _perform_get_on_databse_noun(): Reads a noun from the database.
**
** This requires access to the loom so it must only be run from the libuv
** thread.
*/
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


/* _write_request_data: callback struct for u3_lmdb_write_event()
*/
struct _write_request_data {
  // The database environment to write to. This object is thread-safe, though
  // the transactions and handles opened from it are explicitly not.
  MDB_env* environment;

  // The original event. Not to be accessed from the worker thread; only used
  // in the callback executed on the main loop thread.
  u3_writ* event;

  // The event number from event separated out so we can access it on the other
  // thread.
  c3_d event_number;

  // The event serialized out of the loom into a malloced structure accessible
  // from the worker thread.
  void* malloced_event_data;

  // The size of the malloced_event_data. We keep track of this for the
  // database write.
  size_t malloced_event_data_size;

  // Called on main loop thread on completion.
  void (*on_complete)(u3_writ*);
};

/* _u3_lmdb_write_event_cb(): Implementation of u3_lmdb_write_event()
**
** This is always run on a libuv background worker thread; actual nouns cannot
** be touched here.
*/
static void _u3_lmdb_write_event_cb(uv_work_t* req) {
  struct _write_request_data* data = req->data;

  // Creates the write transaction.
  MDB_txn* transaction_u;
  c3_w ret_w = mdb_txn_begin(data->environment,
                             (MDB_txn *) NULL,
                             0, /* flags */
                             &transaction_u);
  if (0 != ret_w) {
    u3l_log("lmdb: txn_begin fail: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail);
  }

  // Opens the database as part of the transaction.
  c3_w flags_w = MDB_CREATE | MDB_INTEGERKEY;
  MDB_dbi database_u;
  ret_w = mdb_dbi_open(transaction_u,
                       "EVENTS",
                       flags_w,
                       &database_u);
  if (0 != ret_w) {
    u3l_log("lmdb: dbi_open fail: %s\n", mdb_strerror(ret_w));
    u3m_bail(c3__fail);
  }

  // TODO: We need to detect the database being full, making the database
  // maxsize larger, and then retrying this transaction.
  //
  _perform_put_on_databse_raw(transaction_u,
                              database_u,
                              MDB_NOOVERWRITE,
                              &(data->event_number),
                              sizeof(c3_d),
                              data->malloced_event_data,
                              data->malloced_event_data_size);

  ret_w = mdb_txn_commit(transaction_u);
  if (0 != ret_w) {
    u3l_log("lmdb: failed to commit event %" PRIu64  ": %s\n",
            data->event_number,
            mdb_strerror(ret_w));
    u3m_bail(c3__fail);
  }
}

/* _u3_lmdb_write_event_after_cb(): Implementation of u3_lmdb_write_event()
**
** This is always run on the main loop thread after the worker thread event
** completes.
*/
static void _u3_lmdb_write_event_after_cb(uv_work_t* req, int status) {
  struct _write_request_data* data = req->data;

  data->on_complete(data->event);

  free(data->malloced_event_data);
  free(data);
  free(req);
}

/* u3_lmdb_write_event(): Asynchronously writes events to the database.
**
** This writes all the passed in events along with log metadata updates to the
** database as a single transaction on a worker thread. Once the transaction
** is completed, it calls the passed in callback on the main loop thread.
**
** TODO: Make this take multiple events in one commit once we have this
** working one at a time.
*/
void u3_lmdb_write_event(MDB_env* environment,
                         u3_writ* event_u,
                         void (*on_complete)(u3_writ*))
{
  // Serialize the jammed $work into a malloced buffer we can send to the other
  // thread.
  c3_w  siz_w  = u3r_met(3, event_u->mat);
  c3_y* data_u = c3_calloc(siz_w);
  u3r_bytes(0, siz_w, data_u, event_u->mat);

  // Structure to pass to the worker thread.
  struct _write_request_data* data = c3_malloc(sizeof(struct _write_request_data));
  data->environment = environment;
  data->event = event_u;
  data->event_number = event_u->evt_d;
  data->malloced_event_data = data_u;
  data->malloced_event_data_size = siz_w;
  data->on_complete = on_complete;

  // Queue asynchronous work to happen on the other thread.
  uv_work_t* req = c3_malloc(sizeof(uv_work_t));
  req->data = data;

  uv_queue_work(uv_default_loop(),
                req,
                _u3_lmdb_write_event_cb,
                _u3_lmdb_write_event_after_cb);
}

/* u3_lmdb_read_events(): Synchronously reads events from the database.
**
** Reads back up to |len_d| events starting with |first_event_d|. For
** each event, the event will be passed to |on_event_read| and further
** reading will be aborted if the callback returns c3n.
**
** Returns c3y on complete success; c3n on any error.
*/
c3_o u3_lmdb_read_events(u3_pier* pir_u,
                         c3_d first_event_d,
                         c3_d len_d,
                         c3_o(*on_event_read)(u3_pier* pir_u, c3_d id,
                                              u3_noun mat, u3_noun ovo))
{
  // Creates the read transaction.
  MDB_txn* transaction_u;
  c3_w ret_w = mdb_txn_begin(pir_u->log_u->db_u,
                             //environment,
                             (MDB_txn *) NULL,
                             MDB_RDONLY, /* flags */
                             &transaction_u);
  if (0 != ret_w) {
    u3l_log("lmdb: txn_begin fail: %s\n", mdb_strerror(ret_w));
    return c3n;
  }

  // Opens the database as part of the transaction.
  c3_w flags_w = MDB_CREATE | MDB_INTEGERKEY;
  MDB_dbi database_u;
  ret_w = mdb_dbi_open(transaction_u,
                       "EVENTS",
                       flags_w,
                       &database_u);
  if (0 != ret_w) {
    u3l_log("lmdb: dbi_open fail: %s\n", mdb_strerror(ret_w));
    return c3n;
  }

  // Creates a cursor to iterate over keys starting at first_event_d.
  MDB_cursor* cursor_u;
  ret_w = mdb_cursor_open(transaction_u, database_u, &cursor_u);
  if (0 != ret_w) {
    u3l_log("lmdb: cursor_open fail: %s\n", mdb_strerror(ret_w));
    return c3n;
  }

  // Sets the cursor to the position of first_event_d.
  MDB_val key;
  MDB_val val;
  key.mv_size = sizeof(c3_d);
  key.mv_data = &first_event_d;

  ret_w = mdb_cursor_get(cursor_u, &key, &val, MDB_SET_KEY);
  if (0 != ret_w) {
    u3l_log("lmdb: could not find initial event %" PRIu64 ": %s\r\n",
            first_event_d, mdb_strerror(ret_w));
    mdb_cursor_close(cursor_u);
    return c3n;
  }

  // Load up to len_d events, iterating forward across the cursor.
  for (c3_d loaded = 0; (ret_w != MDB_NOTFOUND) && (loaded < len_d); ++loaded) {
    // As a sanity check, we make sure that there aren't any discontinuities in
    // the sequence of loaded events.
    c3_d current_id = first_event_d + loaded;
    if (key.mv_size != sizeof(c3_d) ||
        *(c3_d*)key.mv_data != current_id) {
      u3l_log("lmdb: invalid cursor key\r\n");
      return c3n;
    }

    // Now build the atom version and then the cued version from the raw data
    u3_noun mat = u3i_bytes(val.mv_size, val.mv_data);
    u3_noun ovo = u3ke_cue(u3k(mat));

    if (on_event_read(pir_u, current_id, mat, ovo) == c3n) {
      u3z(ovo);
      u3z(mat);
      u3l_log("lmdb: aborting replay due to error.\r\n");
      return c3n;
    }

    u3z(ovo);
    u3z(mat);

    ret_w = mdb_cursor_get(cursor_u, &key, &val, MDB_NEXT);
    if (ret_w != 0 && ret_w != MDB_NOTFOUND) {
      u3l_log("lmdb: error while loading events: %s\r\n",
              mdb_strerror(ret_w));
      return c3n;
    }
  }

  mdb_cursor_close(cursor_u);

  // Read-only transactions are aborted since we don't need to record the fact
  // that we performed a read.
  mdb_txn_abort(transaction_u);

  return c3y;
}

/* u3_lmdb_get_latest_event_number(): Gets last event id persisted
**
** Reads the last key in order from the EVENTS table as the latest event
** number. On table empty, returns c3y but doesn't modify event_number.
*/
c3_o u3_lmdb_get_latest_event_number(MDB_env* environment, c3_d* event_number)
{
  // Creates the read transaction.
  MDB_txn* transaction_u;
  c3_w ret_w = mdb_txn_begin(environment,
                             (MDB_txn *) NULL,
                             0, /* flags */
                             &transaction_u);
  if (0 != ret_w) {
    u3l_log("lmdb: txn_begin fail: %s\n", mdb_strerror(ret_w));
    return c3n;
  }

  // Opens the database as part of the transaction.
  c3_w flags_w = MDB_CREATE | MDB_INTEGERKEY;
  MDB_dbi database_u;
  ret_w = mdb_dbi_open(transaction_u,
                       "EVENTS",
                       flags_w,
                       &database_u);
  if (0 != ret_w) {
    u3l_log("lmdb: dbi_open fail: %s\n", mdb_strerror(ret_w));
    return c3n;
  }

  // Creates a cursor to point to the last event
  MDB_cursor* cursor_u;
  ret_w = mdb_cursor_open(transaction_u, database_u, &cursor_u);
  if (0 != ret_w) {
    u3l_log("lmdb: cursor_open fail: %s\n", mdb_strerror(ret_w));
    return c3n;
  }

  // Set the cursor at the end of the line.
  MDB_val key;
  MDB_val val;
  ret_w = mdb_cursor_get(cursor_u, &key, &val, MDB_LAST);
  if (MDB_NOTFOUND == ret_w) {
    // Clean up, but don't error out.
    mdb_cursor_close(cursor_u);
    mdb_txn_abort(transaction_u);
    return c3y;
  }

  if (0 != ret_w) {
    u3l_log("lmdb: could not find last event: %s\r\n", mdb_strerror(ret_w));
    mdb_cursor_close(cursor_u);
    mdb_txn_abort(transaction_u);
    return c3n;
  }

  *event_number = *(c3_d*)key.mv_data;

  mdb_cursor_close(cursor_u);

  // Read-only transactions are aborted since we don't need to record the fact
  // that we performed a read.
  mdb_txn_abort(transaction_u);

  return c3y;
}

/* u3_lmdb_write_identity(): Writes the event log identity information
**
** We have a secondary database (table) in this environment named META where we
** read/write identity information from/to.
*/
void u3_lmdb_write_identity(MDB_env* environment,
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


/* u3_lmdb_read_identity(): Reads the event log identity information.
*/
void u3_lmdb_read_identity(MDB_env* environment,
                           u3_noun* who,
                           u3_noun* is_fake,
                           u3_noun* life) {
  // Creates the write transaction.
  MDB_txn* transaction_u;
  c3_w ret_w = mdb_txn_begin(environment,
                             (MDB_txn *) NULL,
                             MDB_RDONLY, /* flags */
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
