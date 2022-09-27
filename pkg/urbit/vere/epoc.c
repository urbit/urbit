/// @file epoc.c
///
/// Event log epoch containing a snapshot and an LMDB instance. The snapshot
/// represents the state before the application of the first event committed to
/// the LMDB instance (which is also the event number that shows up in the epoch
/// directory name).
///
/// As an example, the directory layout for epoch `0i100`, which contains a
/// snapshot representing the state after event 100 and whose first committed
/// event in the LMDB instance is 101, is:
/// ```console
/// 0i100/
///   data.mdb
///   epoc_version.txt
///   lock.mdb
///   north.bin
///   south.bin
///   urbit_version.txt
/// ```

#include "vere/epoc.h"

#include <sys/stat.h>

#include "all.h"
#include "c/prim.h"
#include "vere/vere.h"

//==============================================================================
// Types
//==============================================================================

/// An iterator over an epoch's events.
struct _u3_epoc_iter {
  c3_t        ope_t; ///< true if iterator is open
  MDB_txn*    txn_u; ///< LMDB read-only transaction
  MDB_dbi     dbi_u; ///< LMDB database handle
  MDB_cursor* cur_u; ///< LMDB cursor
  c3_d        fir_d; ///< starting event ID of iterator
  c3_d        cur_d; ///< current event ID of iterator
};

/// An epoch.
struct _u3_epoc {
  c3_path*     pax_u; ///< path to epoch directory
  MDB_env*     env_u; ///< LMDB environment
  c3_d         fir_d; ///< ID of first committed event
  c3_d         las_d; ///< ID of last committed event
  u3_epoc_iter itr_u; ///< iterator
};

//==============================================================================
// Constants
//==============================================================================

const c3_w epo_ver_w = 1;

const size_t epo_siz_i = sizeof(u3_epoc);

const c3_c epo_pre_c[] = "0i";

/// Zero is not a valid value of `epo_min_d` because the `las_d` field of the
/// `u3_epoc` handle is set to `epo_min_d - 1` for the first epoch, which would
/// underflow.
const c3_d epo_min_d = 1ULL;

/// Name of directory housing first epoch. Should be `<epo_pre_c><epo_min_d - 1>`.
static const c3_c fir_nam_c[] = "0i0";

/// Name of text file containing the epoch version number.
static const c3_c epv_nam_c[] = "epoc_version.txt";

/// Name of text file containing the urbit binary version number.
static const c3_c urv_nam_c[] = "urbit_version.txt";

/// Name of LMDB database holding the events.
static const c3_c dab_nam_c[] = "EVENTS";

//==============================================================================
// Macros
//==============================================================================

/// Error-handling wrapper for LMDB API calls that return 0 on success and
/// non-zero on failure.
///
/// @param[in] lmdb_call       LMDB API call.
/// @param[in] failure_action  Statement to execute after logging failure.
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

/// Write an error message and LMDB error code to a file stream.
///
/// @param[in] f    File stream.
/// @param[in] err  LMDB error code.
/// @param[in] fmt  Error message format string.
void
mdb_logerror(FILE* f, int err, const char* fmt, ...);

/// Get the size of an LMDB database file on disk.
///
/// @param[in] handle  LMDB file handle.
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

/// Parse the first event number of an epoch out of its path.
///
/// @param[in] pax_u  Path housing epoch.
///
/// @return 0  Parsing failed.
/// @return    First event number of epoch.
static c3_d
_epoc_first_evt_from_path(const c3_path* const pax_u);

/// Determine if an epoch is the first epoch.
///
/// @param[in] pax_u  Path housing epoch.
///
/// @return 1  Epoch is the first epoch.
/// @return 0  Otherwise.
static inline c3_t
_epoc_is_first(const c3_path* const pax_u);

/// Build an epoch path of the form `<par_c>/0i<fir_d - 1>`.
///
/// @param[in] par_u  Path to parent directory.
/// @param[in] fir_d  ID of first event committed to the epoch. Must NOT
///                   be 0.
///
/// @return  Epoch path.
static c3_path*
_epoc_path(const c3_path* const par_u, const c3_d fir_d);

/// Determine the length of the boot sequence.
///
/// The boot sequence is the sequence of events needed to boot a new ship. The
/// length of the initial subset of the boot sequence required to minimally
/// bootstrap an Arvo kernel is referred to as the life cycle length because
/// this subset must be computed using the life cycle function.
///
/// The notion of a boot sequence only makes sense in the context of the first
/// epoch, and passing a non-first epoch to this function will simply succeed
/// and populate `len_w` with 0.
///
/// For the first epoch, the boot sequence length is determined by attempting to
/// read the value of the "life" key of the META database. Unfortunately, this
/// isn't actually the boot sequence length but instead the life cycle length,
/// but we intentionally overlook this discrepancy because it's unlikely to
/// cause issues in practice most first epochs with a META database will
/// presumably be truncated sooner rather than later.
///
/// A first epoch will only have a META database if it was migrated from the
/// event log format that predated the epoch-based event log. If there is no
/// META database, then the epoch was not migrated and the length of the entire
/// epoch is understood to be the boot sequence length.
///
/// @param[in]  poc_u  Epoch handle.
/// @param[out] len_w  Pointer to length of boot sequence. Only relevant for
///                    first epoch. Can be NULL if not first epoch. If not NULL
///                    and not first epoch, will be set to 0.
///
/// @return 1  `poc_u` is not the first epoch and `len_w` was NULL.
/// @return 1  `len_w` was not NULL and successfully populated.
/// @return 0  Otherwise.
static c3_t
_get_boot_seq_len(const u3_epoc* const poc_u, c3_w* const len_w)
{
  c3_t suc_t = 0;

  if ( !poc_u ) {
    goto end;
  }

  // Non-first epochs don't have boot sequence lengths, so return successfully.
  if ( !_epoc_is_first(poc_u->pax_u) ) {
    if ( len_w ) {
      *len_w = 0;
    }
    suc_t = 1;
    goto end;
  }

  if ( !len_w ) {
    goto end;
  }

  MDB_txn* txn_u;
  try_lmdb(mdb_txn_begin(poc_u->env_u, NULL, MDB_RDONLY, &txn_u),
           goto end,
           "failed to create read-only transaction");

  MDB_dbi dbi_u;
  switch ( mdb_dbi_open(txn_u, "META", 0, &dbi_u) ) {
    // Read the life cycle length out of the epoch's META database.
    case 0: {
      static const c3_c key_c[] = "life";
      MDB_val           key_u   = {
                    .mv_data = (void*)key_c,
                    // Keys in the META database omit the NULL terminator.
                    .mv_size = sizeof(key_c) - 1,
      };
      MDB_val val_u;
      try_lmdb(mdb_get(txn_u, dbi_u, &key_u, &val_u),
               break,
               "failed to get value for key '%s' in META database",
               key_c);
      if ( val_u.mv_size != sizeof(*len_w) ) {
        fprintf(stderr,
                "epoc: size of life cycle length unexpected:"
                " expected %lu, got %lu\r\n",
                sizeof(*len_w),
                val_u.mv_size);
      }
      else {
        memcpy(len_w, val_u.mv_data, sizeof(*len_w));
        suc_t = 1;
      }
      break;
    }

    // The epoch doesn't have a META database, which means that its entire
    // length is the life cycle length.
    case MDB_NOTFOUND:
      *len_w = u3_epoc_len(poc_u);
      suc_t  = 1;
      break;

    default:
      fprintf(stderr,
              "epoc: unexpected error occurred while determining"
              " the life cycle length from %s\r\n",
              u3_epoc_path_str(poc_u));
      break;
  }

  mdb_txn_abort(txn_u);

end:
  return suc_t;
}

/// Get the first and last event numbers from an epoch's LMDB instance.
///
/// @param[in]  env_u  Epoch LMDB instance.
/// @param[out] fir_d  Will be filled with first event number.
/// @param[out] las_d  Will be filled with last event number.
///
/// @return 1  `fir_d` and `las_d` were populated with the first and last event
///            numbers or `epo_min_d` and `0`, respectively, if the LMDB
///            instance has no events committed yet.
/// @return 0  Otherwise.
static c3_t
_lmdb_gulf(MDB_env* env_u, c3_d* const fir_d, c3_d* const las_d);

/// Open an epoch's LMDB environment.
///
/// An epoch's LMDB environment consists of only a single database which is used
/// to store the epoch's events unless it's the first epoch that was created as
/// a result of u3_epoc_migrate(), in which case the LMDB environment will
/// contain a database for the epoch's events and another for its metadata
/// (which is unused but retained for archival reasons).
///
/// @param[in] pax_u  Path to directory containing LMDB environment.
///
/// @return  NULL         LMDB environment could not be created.
/// @return  LMDB handle  Otherwise.
static MDB_env*
_lmdb_init(const c3_path* const pax_u);

/// Move a file to a new directory.
///
/// @param[in] src_u  Source directory containing the file.
/// @param[in] dst_u  Destination directory to contain the file.
/// @param[in] nam_c  Name of the file.
///
/// @return 1  File was successfully moved.
/// @return 0  Otherwise.
static c3_t
_move_file(c3_path* const src_u, c3_path* const dst_u, const c3_c* const nam_c);

/// Persist the Urbit binary version number to `urv_nam_c`.
///
/// @param[in] pax_u  Path to house `urv_nam_c`.
/// @param[in] ver_c  Binary version number.
///
/// @return 1  Success.
/// @return 0  Otherwise.
static c3_t
_persist_binary_version(c3_path* const pax_u, const c3_c* const ver_c);

/// Persist the epoch version number to `epv_nam_c`.
///
/// @param[in] pax_u  Path to house `epv_nam_c`.
/// @param[in] ver_w  Epoch version number.
///
/// @return 1  Success.
/// @return 0  Otherwise.
static c3_t
_persist_epoc_version(c3_path* const pax_u, const c3_w ver_w);

static c3_d
_epoc_first_evt_from_path(const c3_path* const pax_u)
{
  const c3_c* pax_c = c3_path_str(pax_u);
  const c3_c* cur_c = pax_c + strlen(pax_c);
  while ( 0 != strncmp(cur_c, epo_pre_c, sizeof(epo_pre_c) - 1) ) {
    if ( cur_c == pax_c ) {
      goto fail;
    }
    cur_c--;
  }

  c3_c* end_c;
  c3_d  fir_d = strtoull(cur_c + sizeof(epo_pre_c) - 1, &end_c, 10);
  if ( '\0' == *end_c && 0 != fir_d ) {
    return fir_d + 1;
  }

fail:
  return 0;
}

static inline c3_t
_epoc_is_first(const c3_path* const pax_u)
{
  c3_c* sar_c = strstr(c3_path_str(pax_u), fir_nam_c);
  return sar_c && 0 == strcmp(sar_c, fir_nam_c);
}

static c3_path*
_epoc_path(const c3_path* const par_u, const c3_d fir_d)
{
  // This is the largest possible unsigned 64-bit number.
  static const c3_c lar_c[] = "18446744073709551615";

  c3_c dir_c[sizeof(epo_pre_c) + sizeof(lar_c)];
  snprintf(dir_c, sizeof(dir_c), "%s%" PRIu64, epo_pre_c, fir_d - 1);

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

  // If the events database can't be opened, then it must not exist, which means
  // that the epoch to which this LMDB environment belongs is empty.
  MDB_dbi dbi_u;
  if ( 0 != mdb_dbi_open(txn_u, dab_nam_c, MDB_INTEGERKEY, &dbi_u) ) { // (1)
    *fir_d = epo_min_d;
    *las_d = epo_min_d - 1;
    suc_t  = 1;
    goto abort_txn;
  }

  MDB_cursor* cur_u;
  try_lmdb(mdb_cursor_open(txn_u, dbi_u, &cur_u),
           goto abort_txn,
           "failed to open cursor");

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

static MDB_env*
_lmdb_init(const c3_path* const pax_u)
{
  c3_i     ret_i;
  MDB_env* env_u;
  try_lmdb(mdb_env_create(&env_u), goto fail, "failed to create environment");

  // From the LMDB docs: "The [map size] value should be chosen as large as
  // possible, to accommodate future growth of the database."
  static size_t siz_i =
#if ( defined(U3_CPU_aarch64) && defined(U3_OS_linux) ) || defined(U3_OS_mingw)
    0xf00000000;
#else
    0x10000000000;
#endif
  try_lmdb(mdb_env_set_mapsize(env_u, siz_i),
           goto close_env,
           "failed to set map size");

  // The greatest number of databases an epoch can have is 2: META and EVENTS.
  // The only epoch that will have a META database is the first epoch of a ship
  // that predates the epoch-based event log. All other epochs will only have an
  // EVENTS database.
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

static c3_t
_persist_binary_version(c3_path* const pax_u, const c3_c* const ver_c)
{
  if ( !pax_u || !ver_c ) {
    return 0;
  }

  c3_path_push(pax_u, urv_nam_c);
  c3_t suc_t = c3_prim_put(pax_u, c3_prim_str, &ver_c);
  c3_path_pop(pax_u);

  if ( !suc_t ) {
    fprintf(stderr,
            "epoc: failed to write Urbit binary version number to %s\r\n",
            c3_path_str(pax_u));
  }

  return suc_t;
}

static c3_t
_persist_epoc_version(c3_path* const pax_u, const c3_w ver_w)
{
  if ( !pax_u ) {
    return 0;
  }

  c3_path_push(pax_u, epv_nam_c);
  c3_t suc_t = c3_prim_put(pax_u, c3_prim_uint32, &ver_w);
  c3_path_pop(pax_u);

  if ( !suc_t ) {
    fprintf(stderr,
            "epoc: failed to write epoch version number to %s\r\n",
            c3_path_str(pax_u));
  }

  return suc_t;
}

//==============================================================================
// Functions
//==============================================================================

u3_epoc*
u3_epoc_new(const c3_path* const par_u, const c3_d fir_d)
{
  if ( !par_u || 0 == fir_d ) {
    goto fail;
  }

  u3_epoc* poc_u = c3_calloc(sizeof(*poc_u));

  poc_u->fir_d = fir_d;
  poc_u->las_d = fir_d - 1;
  // Protect against (unlikely) potential underflow.
  c3_assert(poc_u->las_d < poc_u->fir_d); // (1)
  poc_u->pax_u = _epoc_path(par_u, poc_u->fir_d);

  c3_assert(0 != access(c3_path_str(poc_u->pax_u), F_OK));
  mkdir(c3_path_str(poc_u->pax_u), 0700);

  // Take snapshot to save the state before the first event in the epoch is
  // applied unless this is the very first epoch.
  if ( epo_min_d != fir_d ) {
#ifndef U3_EPOC_TEST
    u3e_save();
    c3_assert(c3y == u3e_copy(c3_path_str(poc_u->pax_u)));
#endif
  }

  if ( !_persist_epoc_version(poc_u->pax_u, epo_ver_w) ) {
    goto free_epoc;
  }

  if ( !_persist_binary_version(poc_u->pax_u, URBIT_VERSION) ) {
    goto free_epoc;
  }

  if ( !(poc_u->env_u = _lmdb_init(poc_u->pax_u)) ) {
    goto free_epoc;
  }

  return poc_u;

free_epoc:
  u3_epoc_close(poc_u);
  c3_free(poc_u);
fail:
  return NULL;
}

u3_epoc*
u3_epoc_migrate(c3_path* const       src_u,
                const c3_path* const par_u,
                const c3_d           cur_d)
{
  if ( !src_u || !par_u ) {
    goto fail;
  }

  MDB_env* env_u = _lmdb_init(src_u);
  if ( !env_u ) {
    fprintf(stderr,
            "failed to initialize environment at %s\r\n",
            c3_path_str(src_u));
    goto fail;
  }

  c3_d fir_d, las_d;
  if ( !_lmdb_gulf(env_u, &fir_d, &las_d) ) {
    fprintf(stderr,
            "failed to read first and last event "
            "numbers from environment at %s\r\n",
            c3_path_str(src_u));
    goto close_env;
  }

  // The incremental snapshot must be up-to-date to successfully update.
  if ( cur_d != las_d ) {
    // TODO(peter): update version number.
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
    goto close_env;
  }

  u3_epoc* const poc_u = c3_calloc(sizeof(*poc_u));
  poc_u->env_u         = env_u;
  poc_u->fir_d         = fir_d;
  poc_u->las_d         = las_d;
  poc_u->pax_u         = _epoc_path(par_u, poc_u->fir_d);
  mkdir(c3_path_str(poc_u->pax_u), 0700);

  // TODO: are there any repercussions to not reading the event log version out
  // of the META database here?
  if ( !_persist_epoc_version(poc_u->pax_u, epo_ver_w) ) {
    goto free_epoc;
  }

  if ( !_persist_binary_version(poc_u->pax_u, URBIT_VERSION) ) {
    goto free_epoc;
  }

  { // Relocate LMDB instance to the newly created epoch.
    if ( !_move_file(src_u, poc_u->pax_u, "lock.mdb") ) {
      goto free_epoc;
    }
    if ( !_move_file(src_u, poc_u->pax_u, "data.mdb") ) {
      goto rename_lock_mdb;
    }
  }

  return poc_u;

rename_lock_mdb:
  c3_assert(_move_file(poc_u->pax_u, src_u, "lock.mdb"));
  // TODO: remove binary files on failure.
free_epoc:
  u3_epoc_close(poc_u);
  c3_free(poc_u);
close_env:
  mdb_env_close(env_u);
fail:
  return NULL;
}

u3_epoc*
u3_epoc_open(const c3_path* const pax_u, const c3_t rdo_t, c3_w* const len_w)
{
  if ( !pax_u ) {
    goto fail;
  }

  u3_epoc* poc_u = c3_calloc(sizeof(*poc_u));

  poc_u->pax_u = c3_path_fp(pax_u);

  if ( !(poc_u->env_u = _lmdb_init(poc_u->pax_u)) ) {
    goto free_epoc;
  }

  { // Read contents of version file.
    c3_path_push(poc_u->pax_u, epv_nam_c);
    c3_w ver_w;
    if ( !c3_prim_get(poc_u->pax_u, c3_prim_uint32, &ver_w) ) {
      goto free_epoc;
    }
    c3_path_pop(poc_u->pax_u);
    // We'll need to do something more sophisticated than a simple assertion
    // when we bump the epoch version number for the first time, but this is
    // fine for now.
    c3_assert(epo_ver_w == ver_w);
  }

  if ( !_lmdb_gulf(poc_u->env_u, &poc_u->fir_d, &poc_u->las_d) ) {
    goto free_epoc;
  }

  if ( u3_epoc_is_empty(poc_u) ) {
    if ( 0 == (poc_u->fir_d = _epoc_first_evt_from_path(poc_u->pax_u)) ) {
      fprintf(stderr,
              "epoc: failed to parse first event number from empty"
              " epoch %s\r\n",
              c3_path_str(poc_u->pax_u));
      goto free_epoc;
    }
    poc_u->las_d = poc_u->fir_d - 1;
  }

  if ( !_get_boot_seq_len(poc_u, len_w) ) {
    fprintf(stderr,
            "epoc: failed to determine life cycle length from epoch %s\r\n",
            c3_path_str(poc_u->pax_u));
    goto free_epoc;
  }

  if ( rdo_t ) {
    mdb_env_close(poc_u->env_u);
    poc_u->env_u = NULL;
  }

  return poc_u;

free_epoc:
  u3_epoc_close(poc_u);
  c3_free(poc_u);
fail:
  return NULL;
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
u3_epoc_num(const u3_epoc* const poc_u)
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
  return epo_min_d == u3_epoc_first_commit(poc_u);
}

c3_t
u3_epoc_commit(u3_epoc* const poc_u, const c3_lode* nod_u, const size_t len_i)
{
  MDB_txn* txn_u;
  try_lmdb(mdb_txn_begin(poc_u->env_u, NULL, 0, &txn_u),
           goto fail,
           "failed to create write transaction");

  MDB_dbi dbi_u;
  try_lmdb(mdb_dbi_open(txn_u, dab_nam_c, MDB_CREATE | MDB_INTEGERKEY, &dbi_u),
           goto abort_txn,
           "failed to open %s database",
           dab_nam_c);

  c3_d ide_d = poc_u->las_d;
  for ( size_t idx_i = 0; idx_i < len_i; idx_i++ ) {
    ide_d++;
    if ( !nod_u ) {
      goto abort_txn;
    }
    MDB_val key_u = {.mv_data = &ide_d, .mv_size = sizeof(ide_d)};
    MDB_val val_u = {
      .mv_data = *(c3_y**)c3_lode_data(nod_u),
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

c3_t
u3_epoc_iter_open(u3_epoc* const poc_u, c3_d ide_d)
{
  if ( poc_u->itr_u.ope_t ) {
    fprintf(stderr, "epoc: iterator already open\r\n");
    goto fail;
  }

  // Open read-only transaction.
  MDB_txn* txn_u;
  try_lmdb(mdb_txn_begin(poc_u->env_u, NULL, MDB_RDONLY, &txn_u),
           goto fail,
           "failed to open read transaction");

  // Open database containing the events.
  MDB_dbi dbi_u;
  try_lmdb(mdb_dbi_open(txn_u, dab_nam_c, MDB_INTEGERKEY, &dbi_u),
           goto abort_txn,
           "failed to open %s database",
           dab_nam_c);

  // Create cursor in events database.
  MDB_cursor* cur_u;
  try_lmdb(mdb_cursor_open(txn_u, dbi_u, &cur_u),
           goto abort_txn,
           "failed to open cursors");

  // Position cursor on event ID `ide_d`.
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

c3_t
u3_epoc_iter_step(u3_epoc* const poc_u, c3_y** const byt_y, size_t* const byt_i)

{
  if ( !poc_u->itr_u.ope_t ) {
    fprintf(stderr, "epoc: no open iterator\r\n");
    goto fail;
  }

  // We already consumed the last event in the epoch.
  if ( poc_u->las_d < poc_u->itr_u.cur_d ) {
    goto fail;
  }

  MDB_val key_u, val_u;
  c3_t    fir_t = (poc_u->itr_u.fir_d == poc_u->itr_u.cur_d);
  // No need to advance to the next key if this is the first call to
  // u3_epoc_iter_step().
  MDB_cursor_op ops_u = (fir_t ? MDB_GET_CURRENT : MDB_NEXT);
  try_lmdb(mdb_cursor_get(poc_u->itr_u.cur_u, &key_u, &val_u, ops_u),
           goto fail,
           "failed to get event ID %" PRIu64,
           poc_u->itr_u.cur_d);

  c3_d key_d = *(c3_d*)key_u.mv_data;
  // The key we fetched from the database didn't match the key we expected.
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

c3_t
u3_epoc_delete(u3_epoc* const poc_u)
{
  if ( !poc_u ) {
    return 0;
  }

  DIR* dir_u = opendir(u3_epoc_path_str(poc_u));
  if ( !dir_u ) {
    fprintf(stderr,
            "epoc: failed to open %s: %s\r\n",
            u3_epoc_path_str(poc_u),
            strerror(errno));
    return 0;
  }

  c3_path*       pax_u = c3_path_fp(u3_epoc_path(poc_u));
  struct dirent* ent_u;
  errno = 0;
  while ( (ent_u = readdir(dir_u)) ) {
    if ( 0 == strcmp(ent_u->d_name, ".") || 0 == strcmp(ent_u->d_name, "..") ) {
      continue;
    }

    c3_path_push(pax_u, ent_u->d_name);
    if ( 0 != unlink(c3_path_str(pax_u)) ) {
      fprintf(stderr,
              "epoc: failed to delete %s: %s\r\n",
              c3_path_str(pax_u),
              strerror(errno));
      c3_path_free(pax_u);
      return 0;
    }
    c3_path_pop(pax_u);
  }
  c3_path_free(pax_u);

  c3_t suc_t = (0 == errno);
  closedir(dir_u);
  if ( !suc_t ) {
    fprintf(stderr,
            "epoc: failed to delete all files in %s: %s\r\n",
            u3_epoc_path_str(poc_u),
            strerror(errno));
    return 0;
  }

  if ( 0 != rmdir(u3_epoc_path_str(poc_u)) ) {
    fprintf(stderr,
            "epoc: failed to delete %s: %s\r\n",
            u3_epoc_path_str(poc_u),
            strerror(errno));
    return 0;
  }

  return 1;
}

#undef try_lmdb
