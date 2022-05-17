//! @file epoc.c
//!
//! Event log epoch containing a snapshot and an LMDB instance. The snapshot
//! represents the state before the application of the first event committed to
//! the LMDB instance (which is also the event number that shows up in the epoch
//! directory name).
//!
//! As an example, the directory layout for epoch `0i101`, which contains a
//! snapshot representing the state after event 100 and whose first committed
//! event in the LMDB instance is 101, is:
//! ```console
//! 0i101/
//!   data.mdb
//!   lock.mdb
//!   north.bin
//!   south.bin
//! ```

#include "vere/epoc.h"

#include <sys/stat.h>

#include "all.h"
#include "c/bile.h"
#include "vere/vere.h"

//==============================================================================
// Types
//==============================================================================

//! Iterator over an epoch's events.
struct _u3_epoc_iter {
  c3_t        ope_t; //!< true if iterator is open
  MDB_txn*    txn_u; //!< LMDB read-only transaction
  MDB_dbi     dbi_u; //!< LMDB database handle
  MDB_cursor* cur_u; //!< LMDB cursor
  c3_d        fir_d; //!< starting event ID of iterator
  c3_d        cur_d; //!< current event ID of iterator
};

//! Epoch.
struct _u3_epoc {
  c3_path*     pax_u; //!< path to epoch directory
  MDB_env*     env_u; //!< LMDB environment
  c3_d         fir_d; //!< ID of first committed event
  c3_d         las_d; //!< ID of last committed event
  u3_epoc_iter itr_u; //!< iterator
};

//==============================================================================
// Constants
//==============================================================================

const c3_w epo_ver_w = 1;

const size_t epo_siz_i = sizeof(u3_epoc);

const c3_c epo_pre_c[] = "0i";

//! Zero is not a valid value of `epo_min_d` because the `las_d` field of the
//! `u3_epoc` handle is set to `epo_min_d - 1` for the first epoch, which would
//! underflow.
const c3_d epo_min_d = 1ULL;

//! Name of directory housing first epoch. Should be `<epo_pre_c><epo_min_d>`.
static const c3_c fir_nam_c[] = "0i1";

//! Name of binary file containing the epoch version number.
static const c3_c ver_nam_c[] = "version.bin";

//! Name of binary file containing the lifecycle length.
static const c3_c lif_nam_c[] = "lifecycle.bin";

//==============================================================================
// Macros
//==============================================================================

//! Error-handling wrapper for LMDB API calls that return 0 on success and
//! non-zero on failure.
//!
//! @param[in] lmdb_call       LMDB API call.
//! @param[in] failure_action  Statement to execute after logging failure.
#define try_lmdb(lmdb_call, failure_action, ...)                               \
  do {                                                                         \
    c3_i ret_i = lmdb_call;                                                    \
    if ( 0 != ret_i ) {                                                        \
      fprintf(stderr, "lmdb: " __VA_ARGS__);                                   \
      mdb_logerror(stderr, ret_i, "");                                         \
      failure_action;                                                          \
    }                                                                          \
  } while ( 0 )

//==============================================================================
// Patch
//==============================================================================
// MinGW requires a patch to LMDB that implements a custom logging function
// mdb_logerror(), so we need to declare mdb_logerror() here and provide an
// implementation of it for non-MinGW systems.

//! Write an error message and LMDB error code to a file stream.
//!
//! @param[in] f    File stream.
//! @param[in] err  LMDB error code.
//! @param[in] fmt  Error message format string.
void
mdb_logerror(FILE* f, int err, const char* fmt, ...);

//! Get the size of an LMDB database file on disk.
//!
//! @param[in] handle  LMDB file handle.
intmax_t
mdb_get_filesize(mdb_filehandle_t handle);

#if !defined(U3_OS_mingw)
void
mdb_logerror(FILE* f, int err, const char* fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vfprintf(f, fmt, ap);
  va_end(ap);
  fprintf(f, ": %s\r\n", mdb_strerror(err));
}

intmax_t
mdb_get_filesize(mdb_filehandle_t han_u)
{
  struct stat sat_u;
  fstat(han_u, &sat_u);
  return (intmax_t)sat_u.st_size;
}
#endif /* if !defined(U3_OS_mingw) */

//==============================================================================
// Static functions
//==============================================================================

//! Determine if an epoch is the first epoch.
//!
//! @param[in] pax_u  Path housing epoch.
//!
//! @return 1  Epoch is the first epoch.
//! @return 0  Otherwise.
static inline c3_t
_epoc_is_first(const c3_path* const pax_u);

//! Build an epoch path of the form `<par_c>/0i<fir_d>`.
//!
//! @param[in] par_u  Path to parent directory.
//! @param[in] fir_d
//!
//! @return  Epoch path.
static c3_path*
_epoc_path(const c3_path* const par_u, const c3_d fir_d);

//! Get the first and last event numbers from an epoch's LMDB instance.
//!
//! @param[in]  env_u  Epoch LMDB instance.
//! @param[out] fir_d  Will be filled with first event number.
//! @param[out] las_d  Will be filled with last event number.
//!
//! @return 1  `fir_d` and `las_d` were populated with the first and last event
//!            numbers.
//! @return 0  Otherwise.
static c3_t
_lmdb_gulf(MDB_env* env_u, c3_d* const fir_d, c3_d* const las_d);

//! Open an epoch's LMDB environment.
//!
//! An epoch's LMDB environment consists of only a single database which is used
//! to store the epoch's events unless it's the first epoch that was created as
//! a result of u3_epoc_migrate(), in which case the LMDB environment will
//! contain a database for the epoch's events and another for its metadata
//! (which is unused but retained for archival reasons).
//!
//! @param[in] pax_u  Path to directory containing LMDB environment.
//!
//! @return  NULL         LMDB environment could not be created.
//! @return  LMDB handle  Otherwise.
static MDB_env*
_lmdb_init(const c3_path* const pax_u);

//! Move a file to a new directory.
//!
//! @param[in] src_u  Source directory containing the file.
//! @param[in] dst_u  Destination directory to contain the file.
//! @param[in] nam_c  Name of the file.
//!
//! @return 1  File was successfully moved.
//! @return 0  Otherwise.
static c3_t
_move_file(c3_path* const src_u, c3_path* const dst_u, const c3_c* const nam_c);

static inline c3_t
_epoc_is_first(const c3_path* const pax_u)
{
  c3_c* sar_c = strstr(c3_path_str(pax_u), fir_nam_c);
  return sar_c && 0 == strcmp(sar_c, fir_nam_c);
}

//! @n (1) This is the largest possible unsigned 64-bit number.
static c3_path*
_epoc_path(const c3_path* const par_u, const c3_d fir_d)
{
  static const c3_c lar_c[] = "18446744073709551615"; // (1)

  c3_c dir_c[sizeof(epo_pre_c) + sizeof(lar_c)];
  snprintf(dir_c, sizeof(dir_c), "%s%" PRIu64, epo_pre_c, fir_d);

  return c3_path_fv(2, c3_path_str(par_u), dir_c);
}

static c3_t
_lmdb_gulf(MDB_env* env_u, c3_d* const fir_d, c3_d* const las_d)
{
  c3_t suc_t = 0;

  if ( !env_u || !fir_d || !las_d ) {
    goto end;
  }

  MDB_txn* txn_u;
  try_lmdb(mdb_txn_begin(env_u, NULL, MDB_RDONLY, &txn_u),
           goto end,
           "failed to create read transaction");

  MDB_dbi dbi_u;
  try_lmdb(mdb_dbi_open(txn_u, "EVENTS", MDB_CREATE | MDB_INTEGERKEY, &dbi_u),
           goto abort_txn,
           "failed to open EVENTS database");

  MDB_cursor* cur_u;
  try_lmdb(mdb_cursor_open(txn_u, dbi_u, &cur_u),
           goto abort_txn,
           "failed to open cursor");

  // TODO(peter): handle empty epoch

  MDB_val key_u, val_u;
  try_lmdb(mdb_cursor_get(cur_u, &key_u, &val_u, MDB_FIRST),
           goto close_cursor,
           "failed to read first event number");
  *fir_d = *(c3_d*)key_u.mv_data;

  try_lmdb(mdb_cursor_get(cur_u, &key_u, &val_u, MDB_LAST),
           goto close_cursor,
           "failed to read last event number");
  *las_d = *(c3_d*)key_u.mv_data;

  suc_t = 1;

close_cursor:
  mdb_cursor_close(cur_u);
abort_txn:
  mdb_txn_abort(txn_u);
end:
  return suc_t;
}

//! @n (1) From the LMDB docs: "The [map size] value should be chosen as large
//!        as possible, to accommodate future growth of the database."
static MDB_env*
_lmdb_init(const c3_path* const pax_u)
{
  c3_i     ret_i;
  MDB_env* env_u;
  try_lmdb(mdb_env_create(&env_u), goto fail, "failed to create environment");

  // (1)
  static size_t siz_i =
#if ( defined(U3_CPU_aarch64) && defined(U3_OS_linux) ) || defined(U3_OS_mingw)
    0xf00000000;
#else
    0x10000000000;
#endif
  try_lmdb(mdb_env_set_mapsize(env_u, siz_i),
           goto close_env,
           "failed to set map size");

  try_lmdb(mdb_env_set_maxdbs(env_u, 2),
           goto close_env,
           "failed to set max number of databases");

  const c3_c* pax_c = c3_path_str(pax_u);
  try_lmdb(mdb_env_open(env_u, pax_c, 0, 0664),
           goto close_env,
           "failed to open environment at %s",
           pax_c);

  goto succeed;

close_env:
  mdb_env_close(env_u);
fail:
  return NULL;

succeed:
  return env_u;
}

static c3_t
_move_file(c3_path* const src_u, c3_path* const dst_u, const c3_c* const nam_c)
{
  c3_t suc_t = 0;

  c3_path_push(src_u, nam_c);
  if ( 0 != access(c3_path_str(src_u), R_OK | W_OK) ) {
    goto pop_src_path;
  }

  c3_path_push(dst_u, nam_c);
  if ( 0 != rename(c3_path_str(src_u), c3_path_str(dst_u)) ) {
    goto pop_dst_path;
  }

  suc_t = 1;

pop_dst_path:
  c3_path_pop(dst_u);
pop_src_path:
  c3_path_pop(src_u);
end:
  return suc_t;
}

//==============================================================================
// Functions
//==============================================================================

//! @n (1) Protect against (unlikely) potential underflow.
//! @n (2) Write the epoch version number to a file.
//! @n (3) Convert to network byte order to ensure portability across platforms
//!        of varying endianness.
//! @n (4) Take snapshot to save the state before the first event in the
//!        epoch is applied unless this is the very first epoch.
//! @n (5) Write the lifecycle length to a file if this is the very first epoch.
//! @n (6) See (3).
u3_epoc*
u3_epoc_new(const c3_path* const par_u, const c3_d fir_d, c3_w lif_w)
{
  if ( !par_u || 0 == fir_d ) {
    goto fail;
  }

  u3_epoc* poc_u = c3_calloc(sizeof(*poc_u));

  poc_u->fir_d = fir_d;
  poc_u->las_d = fir_d - 1;
  c3_assert(poc_u->las_d < poc_u->fir_d); // (1)
  poc_u->pax_u = _epoc_path(par_u, poc_u->fir_d);
  mkdir(c3_path_str(poc_u->pax_u), 0700);

  if ( !(poc_u->env_u = _lmdb_init(poc_u->pax_u)) ) {
    goto free_epoc;
  }

  { // (2)
    c3_path_push(poc_u->pax_u, ver_nam_c);
    c3_w ver_w = htonl(epo_ver_w); // (3)
    if ( !c3_bile_write_new(poc_u->pax_u, &ver_w, sizeof(ver_w)) ) {
      goto free_epoc;
    }
    c3_path_pop(poc_u->pax_u);
  }

  if ( epo_min_d != fir_d ) { // (4)
#ifndef U3_EPOC_TEST
    u3e_save();
    c3_assert(c3y == u3e_copy(c3_path_str(poc_u->pax_u)));
#endif
  }
  else { // (5)
    c3_path_push(poc_u->pax_u, lif_nam_c);
    lif_w = htonl(lif_w); // (6)
    if ( !c3_bile_write_new(poc_u->pax_u, &lif_w, sizeof(lif_w)) ) {
      goto free_epoc;
    }
    c3_path_pop(poc_u->pax_u);
  }

  goto succeed;

free_epoc:
  u3_epoc_close(poc_u);
  c3_free(poc_u);
fail:
  return NULL;

succeed:
  return poc_u;
}

//! @n (1) Protect against unlikely case where `epo_min_d` is erroneously
//!        changed to 0.
//! @n (2) Relocate LDMB instance to the newly created epoch.
//! @n (3) The incremental snapshot must be up-to-date to successfully migrate.
//! @n (4) Read metadata out of LMDB instance.
//! @n (5) Write metadata to binary files.
//! @n (6) Convert to network byte order to ensure portability across platforms
//!        of varying endianness.
//! @n (7) See (6).
u3_epoc*
u3_epoc_migrate(const c3_path* const par_u,
                c3_path* const       src_u,
                u3_meta* const       met_u)
{
  if ( !par_u || !src_u ) {
    goto fail;
  }

  u3_epoc* poc_u = c3_calloc(sizeof(*poc_u));
  poc_u->fir_d   = epo_min_d;
  poc_u->las_d   = epo_min_d - 1;
  c3_assert(poc_u->las_d < poc_u->fir_d); // (1)
  poc_u->pax_u   = _epoc_path(par_u, epo_min_d);
  mkdir(c3_path_str(poc_u->pax_u), 0700);

  { // (2)
    if ( !_move_file(src_u, poc_u->pax_u, "data.mdb") ) {
      goto free_epoc;
    }
    if ( !_move_file(src_u, poc_u->pax_u, "lock.mdb") ) {
      goto rename_data_mdb;
    }
  }

  if ( !(poc_u->env_u = _lmdb_init(poc_u->pax_u)) ) {
    goto rename_lock_mdb;
  }

  if ( !_lmdb_gulf(poc_u->env_u, &poc_u->fir_d, &poc_u->las_d) ) {
    goto rename_lock_mdb;
  }

  if ( u3A->eve_d != poc_u->las_d ) { // (3)
    fprintf(stderr,
            "IMPORTANT: cannot migrate the existing event log format to the\r\n"
            "           new epoch-based event log format because the\r\n"
            "           incremental snapshot is not up-to-date. To resolve\r\n"
            "           this, run your ship using version 1.8 of the urbit\r\n"
            "           binary and exit *gracefully* using Ctrl-D. Then,\r\n"
            "           try again using the latest version of the urbit\r\n"
            "           binary. If you have questions or concerns, please\r\n"
            "           file an issue against\r\n"
            "           https://github.com/urbit/urbit and assign it to\r\n"
            "           mcevoypeter.\r\n");
    goto rename_lock_mdb;
  }

  MDB_txn* txn_u;
  { // (4)
    try_lmdb(mdb_txn_begin(poc_u->env_u, NULL, MDB_RDONLY, &txn_u),
             goto rename_lock_mdb,
             "failed to create read-only transaction");

    MDB_dbi dbi_u;
    try_lmdb(mdb_dbi_open(txn_u, "META", 0, &dbi_u),
             goto abort_txn,
             "failed to open META database");

    MDB_val key_u, val_u;

    key_u.mv_data = (void*)"who";
    key_u.mv_size = strlen("who");
    try_lmdb(mdb_get(txn_u, dbi_u, &key_u, &val_u),
             goto abort_txn,
             "failed to lookup metadata value for key 'who'");
    u3_atom who = u3i_bytes(val_u.mv_size, val_u.mv_data);
    u3r_chubs(0, 2, met_u->who_d, who);
    u3z(who);

    key_u.mv_data = (void*)"fake";
    key_u.mv_size = strlen("fake");
    try_lmdb(mdb_get(txn_u, dbi_u, &key_u, &val_u),
             goto abort_txn,
             "failed to lookup metadata value for key 'fake'");
    met_u->fak_o = u3i_bytes(val_u.mv_size, val_u.mv_data);

    key_u.mv_data = (void*)"life";
    key_u.mv_size = strlen("life");
    try_lmdb(mdb_get(txn_u, dbi_u, &key_u, &val_u),
             goto abort_txn,
             "failed to lookup metadata value for key 'life'");
    met_u->lif_w = u3i_bytes(val_u.mv_size, val_u.mv_data);

    key_u.mv_data = (void*)"version";
    key_u.mv_size = strlen("version");
    try_lmdb(mdb_get(txn_u, dbi_u, &key_u, &val_u),
             goto abort_txn,
             "failed to lookup metadata value for key 'version'");
    met_u->ver_w = u3i_bytes(val_u.mv_size, val_u.mv_data);

    mdb_txn_abort(txn_u);
  }

  { // (5)
    c3_path_push(poc_u->pax_u, ver_nam_c);
    c3_w ver_w = htonl(met_u->ver_w); // (6)
    if ( !c3_bile_write_new(poc_u->pax_u, &ver_w, sizeof(ver_w)) ) {
      goto rename_lock_mdb;
    }
    c3_path_pop(poc_u->pax_u);

    c3_path_push(poc_u->pax_u, lif_nam_c);
    c3_w lif_w = htonl(met_u->lif_w); // (7)
    if ( !c3_bile_write_new(poc_u->pax_u, &lif_w, sizeof(lif_w)) ) {
      goto rename_lock_mdb;
    }
    c3_path_pop(poc_u->pax_u);
  }

  goto succeed;

abort_txn:
  mdb_txn_abort(txn_u);
rename_lock_mdb:
  c3_assert(_move_file(poc_u->pax_u, src_u, "lock.mdb"));
rename_data_mdb:
  c3_assert(_move_file(poc_u->pax_u, src_u, "data.mdb"));
free_epoc:
  u3_epoc_close(poc_u);
  c3_free(poc_u);
fail:
  return NULL;

succeed:
  return poc_u;
}

//! @n (1) Read contents of version file.
//! @n (2) We'll need to do something more sophisticated than a simple assertion
//!        when we bump the epoch version number for the first time, but this is
//!        fine for now.
//! @n (3) Read contents of lifecycle file.
u3_epoc*
u3_epoc_open(const c3_path* const pax_u, c3_w* const lif_w)
{
  if ( !pax_u ) {
    goto fail;
  }

  u3_epoc* poc_u = c3_calloc(sizeof(*poc_u));

  poc_u->pax_u = c3_path_fp(pax_u);

  if ( !(poc_u->env_u = _lmdb_init(poc_u->pax_u)) ) {
    goto free_epoc;
  }

  { // (1)
    c3_path_push(poc_u->pax_u, ver_nam_c);
    c3_w ver_w;
    if ( !c3_bile_read_existing(poc_u->pax_u, &ver_w, sizeof(ver_w)) ) {
      goto free_epoc;
    }
    ver_w = ntohl(ver_w);
    c3_path_pop(poc_u->pax_u);
    c3_assert(epo_ver_w == ver_w); // (2)
  }

  if ( _epoc_is_first(pax_u) ) { // (3)
    c3_path_push(poc_u->pax_u, lif_nam_c);
    if ( !c3_bile_read_existing(poc_u->pax_u, lif_w, sizeof(*lif_w)) ) {
      goto free_epoc;
    }
    c3_path_pop(poc_u->pax_u);
    if ( !lif_w ) {
      goto free_epoc;
    }
    *lif_w = ntohl(*lif_w);
  } else if ( lif_w ) {
    *lif_w = 0;
  }

  if ( !_lmdb_gulf(poc_u->env_u, &poc_u->fir_d, &poc_u->las_d) ) {
    goto free_epoc;
  }
  goto succeed;

free_epoc:
  u3_epoc_close(poc_u);
  c3_free(poc_u);
fail:
  return NULL;

succeed:
  return poc_u;
}

const c3_path*
u3_epoc_path(const u3_epoc* const poc_u)
{
  return poc_u->pax_u;
}

const c3_c*
u3_epoc_path_str(const u3_epoc* const poc_u)
{
  return c3_path_str(poc_u->pax_u);
}

c3_d
u3_epoc_first_commit(const u3_epoc* const poc_u)
{
  return poc_u->fir_d;
}

c3_d
u3_epoc_last_commit(const u3_epoc* const poc_u)
{
  return poc_u->las_d;
}

c3_d
u3_epoc_first_evt(const u3_epoc* const poc_u)
{
  return u3_epoc_first_commit(poc_u) - 1;
}

c3_t
u3_epoc_is_empty(const u3_epoc* const poc_u)
{
  return poc_u->las_d < poc_u->fir_d;
}

size_t
u3_epoc_len(const u3_epoc* const poc_u)
{
  return u3_epoc_is_empty(poc_u) ? 0 : poc_u->las_d + 1 - poc_u->fir_d;
}

c3_t
u3_epoc_has(const u3_epoc* const poc_u, const c3_d ide_d)
{
  return u3_epoc_first_commit(poc_u) <= ide_d
         && ide_d <= u3_epoc_last_commit(poc_u);
}

c3_t
u3_epoc_is_first(const u3_epoc* const poc_u)
{
  return u3_epoc_has(poc_u, epo_min_d);
}

c3_t
u3_epoc_commit(u3_epoc* const poc_u, const c3_lode* nod_u, const size_t len_i)
{
  MDB_txn* txn_u;
  try_lmdb(mdb_txn_begin(poc_u->env_u, NULL, 0, &txn_u),
           goto fail,
           "failed to create write transaction");

  MDB_dbi dbi_u;
  try_lmdb(mdb_dbi_open(txn_u, "EVENTS", MDB_CREATE | MDB_INTEGERKEY, &dbi_u),
           goto abort_txn,
           "failed to open EVENTS database");

  c3_d ide_d = poc_u->las_d;
  for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
    ide_d++;
    if ( !nod_u ) {
      goto abort_txn;
    }
    MDB_val key_u = {.mv_data = &ide_d, .mv_size = sizeof(ide_d)};
    MDB_val val_u = {
      .mv_data = c3_lode_data(nod_u),
      .mv_size = c3_lode_len(nod_u),
    };

    try_lmdb(mdb_put(txn_u, dbi_u, &key_u, &val_u, MDB_NOOVERWRITE),
             goto abort_txn,
             "failed to write event %" PRIu64,
             ide_d);

    nod_u = c3_lode_next(nod_u);
  }

  try_lmdb(mdb_txn_commit(txn_u),
           goto abort_txn,
           "failed to commit transaction");

  poc_u->las_d = ide_d;

  goto succeed;

abort_txn:
  mdb_txn_abort(txn_u);
fail:
  return 0;

succeed:
  return 1;
}

//! @n (1) Open read-only transaction.
//! @n (2) Open database containing the events.
//! @n (3) Create cursor in events database.
//! @n (4) Position cursor on event ID `ide_d`.
c3_t
u3_epoc_iter_open(u3_epoc* const poc_u, c3_d ide_d)
{
  if ( poc_u->itr_u.ope_t ) {
    fprintf(stderr, "epoc: iterator already open\r\n");
    goto fail;
  }

  // (1)
  MDB_txn* txn_u;
  try_lmdb(mdb_txn_begin(poc_u->env_u, NULL, MDB_RDONLY, &txn_u),
           goto fail,
           "failed to open read transaction");

  // (2)
  MDB_dbi dbi_u;
  try_lmdb(mdb_dbi_open(txn_u, "EVENTS", MDB_INTEGERKEY, &dbi_u),
           goto abort_txn,
           "failed to open EVENTS database");

  // (3)
  MDB_cursor* cur_u;
  try_lmdb(mdb_cursor_open(txn_u, dbi_u, &cur_u),
           goto abort_txn,
           "failed to open cursors");

  // (4)
  MDB_val key_u = {.mv_data = &ide_d, .mv_size = sizeof(ide_d)}, val_u;
  try_lmdb(mdb_cursor_get(cur_u, &key_u, &val_u, MDB_SET_KEY),
           goto close_cursor,
           "failed to position cursor on event ID %" PRIu64,
           ide_d);

  poc_u->itr_u = (u3_epoc_iter){
    .ope_t = 1,
    .txn_u = txn_u,
    .dbi_u = dbi_u,
    .cur_u = cur_u,
    .fir_d = ide_d,
    .cur_d = ide_d,
  };

  goto succeed;

close_cursor:
  mdb_cursor_close(cur_u);
abort_txn:
  mdb_txn_abort(txn_u);
fail:
  return 0;

succeed:
  return 1;
}

//! @n (1) We already consumed the last event in the epoch.
//! @n (2) No need to advance to the next key if this is the first call to
//!        u3_epoc_iter_step().
//! @n (3) The key we fetched from database didn't match the key we expected.
c3_t
u3_epoc_iter_step(u3_epoc* const poc_u, c3_y** const byt_y, size_t* const byt_i)

{
  if ( !poc_u->itr_u.ope_t ) {
    fprintf(stderr, "epoc: no open iterator\r\n");
    goto fail;
  }

  if ( poc_u->las_d < poc_u->itr_u.cur_d ) { // (1)
    goto fail;
  }

  MDB_val       key_u, val_u;
  c3_t          fir_t = (poc_u->itr_u.fir_d == poc_u->itr_u.cur_d);
  MDB_cursor_op ops_u = (fir_t ? MDB_GET_CURRENT : MDB_NEXT); // (2)
  try_lmdb(mdb_cursor_get(poc_u->itr_u.cur_u, &key_u, &val_u, ops_u),
           goto fail,
           "failed to get event ID %" PRIu64,
           poc_u->itr_u.cur_d);

  c3_d key_d = *(c3_d*)key_u.mv_data;
  if ( key_d != poc_u->itr_u.cur_d ) { // (3)
    fprintf(stderr,
            "epoc: event ID mismatch: expected %" PRIu64 ", received %" PRIu64
            "\r\n",
            poc_u->itr_u.cur_d,
            key_d);
    goto fail;
  }

  *byt_y = val_u.mv_data;
  *byt_i = val_u.mv_size;
  poc_u->itr_u.cur_d++;
  goto succeed;

fail:
  return 0;

succeed:
  return 1;
}

void
u3_epoc_iter_close(u3_epoc* const poc_u)
{
  mdb_cursor_close(poc_u->itr_u.cur_u);
  mdb_txn_abort(poc_u->itr_u.txn_u);
  poc_u->itr_u.ope_t = 0;
}

void
u3_epoc_info(const u3_epoc* const poc_u)
{
  fprintf(stderr, "  epoc: %s\r\n", u3_epoc_path_str(poc_u));
  fprintf(stderr,
          "    first commit: %" PRIu64 "\r\n",
          u3_epoc_first_commit(poc_u));
  fprintf(stderr,
          "    last commit: %" PRIu64 "\r\n",
          u3_epoc_last_commit(poc_u));

  MDB_stat    mst_u;
  MDB_envinfo mei_u;

  (void)mdb_env_stat(poc_u->env_u, &mst_u);
  (void)mdb_env_info(poc_u->env_u, &mei_u);

  fprintf(stderr, "    map size: %zu\r\n", mei_u.me_mapsize);
  fprintf(stderr, "    page size: %u\r\n", mst_u.ms_psize);
  fprintf(stderr, "    max pages: %zu\r\n", mei_u.me_mapsize / mst_u.ms_psize);
  fprintf(stderr, "    number of pages used: %zu\r\n", mei_u.me_last_pgno + 1);
  fprintf(stderr, "    last transaction ID: %zu\r\n", mei_u.me_last_txnid);
  fprintf(stderr, "    max readers: %u\r\n", mei_u.me_maxreaders);
  fprintf(stderr, "    number of readers used: %u\r\n", mei_u.me_numreaders);
}

void
u3_epoc_close(u3_epoc* const poc_u)
{
  if ( !poc_u ) {
    return;
  }

  if ( poc_u->env_u ) {
    mdb_env_close(poc_u->env_u);
  }
  if ( poc_u->pax_u ) {
    c3_path_free(poc_u->pax_u);
  }
}

#undef try_lmdb
