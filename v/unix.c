/* v/unix.c
**
**  This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <dirent.h>
#include <stdint.h>
#include <uv.h>
#include <termios.h>
#include <term.h>
#include <errno.h>

#include "all.h"
#include "v/vere.h"

/* _unix_down(): descend path.
*/
static c3_c*
_unix_down(c3_c* pax_c, c3_c* sub_c)
{
  c3_w pax_w = strlen(pax_c);
  c3_w sub_w = strlen(sub_c);
  c3_c* don_c = c3_malloc(pax_w + strlen(sub_c) + 2);

  strncpy(don_c, pax_c, pax_w + 1);
  don_c[pax_w] = '/';
  strncpy(don_c + pax_w + 1, sub_c, sub_w + 1);
  don_c[pax_w + sub_w + 1] = '\0';

  return don_c;
}

/* _unix_mkdir(): mkdir, asserting.
*/
static void
_unix_mkdir(c3_c* pax_c)
{
  if ( 0 != mkdir(pax_c, 0755) && EEXIST != errno) {
    uL(fprintf(uH, "%s: %s\n", pax_c, strerror(errno)));
    c3_assert(0);
  }
}

/* _unix_opendir(): opendir, recreating if nonexistent.
*/
static DIR*
_unix_opendir(c3_c* pax_c)
{
  DIR* rid_u = opendir(pax_c);

  if ( !rid_u ) {
    // uL(fprintf(uH, "%s: %s\n", pax_c, strerror(errno)));
    _unix_mkdir(pax_c);
    rid_u = opendir(pax_c);
    if ( !rid_u ) {
      uL(fprintf(uH, "%s: %s\n", pax_c, strerror(errno)));
      c3_assert(0);
    }
  }
  return rid_u;
}

/* _unix_unlink(): unlink, asserting.
*/
static void
_unix_unlink(c3_c* pax_c)
{
  if ( 0 != unlink(pax_c) && ENOENT != errno) {
    uL(fprintf(uH, "%s: %s\n", pax_c, strerror(errno)));
    c3_assert(0);
  }
}

/* u3_unix_acquire(): acquire a lockfile, killing anything that holds it.
*/
void
u3_unix_acquire(c3_c* pax_c)
{
  c3_c* paf_c = _unix_down(pax_c, ".vere.lock");
  c3_w pid_w;
  FILE* loq_u;

  if ( NULL != (loq_u = fopen(paf_c, "r")) ) {
    if ( 1 != fscanf(loq_u, "%u", &pid_w) ) {
      uL(fprintf(uH, "lockfile %s is corrupt!\n", paf_c));
      kill(getpid(), SIGTERM);
      sleep(1); c3_assert(0);
    }
    else {
      c3_w i_w;

      if ( -1 != kill(pid_w, SIGTERM) ) {
        uL(fprintf(uH, "unix: stopping process %d, live in %s...\n",
                        pid_w, pax_c));

        for ( i_w = 0; i_w < 16; i_w++ ) {
          sleep(1);
          if ( -1 == kill(pid_w, SIGTERM) ) {
            break;
          }
        }
        if ( 16 == i_w ) {
          for ( i_w = 0; i_w < 16; i_w++ ) {
            if ( -1 == kill(pid_w, SIGKILL) ) {
              break;
            }
            sleep(1);
          }
        }
        if ( 16 == i_w ) {
          uL(fprintf(uH, "process %d seems unkillable!\n", pid_w));
          c3_assert(0);
        }
        uL(fprintf(uH, "unix: stopped old process %u\n", pid_w));
      }
    }
    fclose(loq_u);
    unlink(paf_c);
  }

  loq_u = fopen(paf_c, "w");
  fprintf(loq_u, "%u\n", getpid());

  {
    c3_i fid_i = fileno(loq_u);
#if defined(U3_OS_linux)
    fdatasync(fid_i);
#elif defined(U3_OS_osx)
    fcntl(fid_i, F_FULLFSYNC);
#elif defined(U3_OS_bsd)
    fsync(fid_i);
#else
#   error "port: datasync"
#endif
  }
  fclose(loq_u);
  free(paf_c);
}

/* u3_unix_release(): release a lockfile.
*/
void
u3_unix_release(c3_c* pax_c)
{
  c3_c* paf_c = _unix_down(pax_c, ".vere.lock");

  unlink(paf_c);
  free(paf_c);
}

/* _unix_dir_dry(): recursively dry a directory.
*/
static void
_unix_dir_dry(u3_udir* dir_u)
{
  u3_udir* dis_u;
  u3_ufil* fil_u;

  dir_u->dry = u3_yes;
  for ( dis_u = dir_u->dis_u; dis_u; dis_u = dis_u->nex_u ) {
    _unix_dir_dry(dis_u);
  }
  for ( fil_u = dir_u->fil_u; fil_u; fil_u = fil_u->nex_u ) {
    fil_u->dry = u3_yes;
  }
}

/* _unix_fs_event_cb(): filesystem event callback.
*/
static void
_unix_fs_event_cb(uv_fs_event_t* was_u,
                  const c3_c*    pax_c,
                  c3_i           evt_i,
                  c3_i           sas_i)
{
  
  // note that we're doing something tricky and weird here.
  //
  // * libuv passes around a pointer to a uv_fs_event_t
  // * we define a struct that STARTS with a uv_fs_event_t and then has
  //     more fields after it
  // * this is what we pass into libuv up top
  // * this is what we get out of libuv down below
  // * thus a cast is cool
  u3_unod* nod_u = (u3_unod*) was_u;

#ifdef SYNCLOG

  c3_w slot = u3_Host.unx_u.lot_w++ % 1024;
  free(u3_Host.unx_u.sylo[slot].pax_c);
  u3_Host.unx_u.sylo[slot].pax_c = 0;
  u3_Host.unx_u.sylo[slot].unx   = u3_yes;
  u3_Host.unx_u.sylo[slot].wer_m = c3_s4('u','v','s','y');
  u3_Host.unx_u.sylo[slot].wot_m = 0;
  u3_Host.unx_u.sylo[slot].pax_c = strdup(nod_u->pax_c);
#endif

  {
    while ( nod_u ) {
      nod_u->dry = u3_no;
      nod_u = (u3_unod*) nod_u->par_u;
    }
  }
}

/* _unix_file_watch(): create file tracker (from filesystem)
*/
static void
_unix_file_watch(u3_ufil* fil_u,
                 u3_udir* dir_u,
                 c3_c*    pax_c,
                 mpz_t    mod_mp)
{
  // (1) build data structure
  //
  fil_u->non = u3_no;
  fil_u->dry = u3_no;
  fil_u->pax_c = pax_c;
  {
    c3_c* dot_c = strrchr(pax_c, '.');
    c3_c* fas_c = strrchr(pax_c, '/');

    fil_u->dot_c = dot_c ? (fas_c ? ((dot_c > fas_c) ? dot_c : 0)
                            : dot_c)
      : 0;
  }
  fil_u->par_u = dir_u;
  mpz_init_set(fil_u->mod_mp, mod_mp);
  fil_u->nex_u = 0;

  c3_assert(!fil_u->dot_c || (fil_u->dot_c > fil_u->pax_c));


  // (2) stuff data structure into libuv
  //
  c3_w ret_w = uv_fs_event_init(u3L,          // loop
                                &fil_u->was_u // uv_fs_event_t 
                                );
  if (0 != ret_w){
    uL(fprintf(uH, "event init: %s\n", strerror(ret_w)));
    c3_assert(0);
  }

  //
  ret_w = uv_fs_event_start(&fil_u->was_u,     // uv_fs_event_t
                            _unix_fs_event_cb, // callback
                            pax_c,             // dir as strings
                            0);                // flags
  if (0 != ret_w){
    uL(fprintf(uH, "event start: %s\n", strerror(ret_w)));
    c3_assert(0);
  }

}

/* _unix_file_form(): form a filename path downward.
*/
static c3_c*
_unix_file_form(u3_udir* dir_u,
                u3_noun  pre,
                u3_bean  ket,
                u3_noun  ext)
{
  c3_c* pre_c = u3_cr_string(pre);
  c3_c* ext_c = u3_cr_string(ext);
  c3_w  pax_w = strlen(dir_u->pax_c);
  c3_w  pre_w = strlen(pre_c);
  c3_w  ext_w = strlen(ext_c);
  c3_w  ket_w = (u3_yes == ket) ? 1 : 0;
  c3_c* pax_c = c3_malloc(pax_w + 1 + pre_w + 1 + ket_w + ext_w + 1);

  strncpy(pax_c, dir_u->pax_c, pax_w);
  pax_c[pax_w] = '/';
  strncpy(pax_c + pax_w + 1, pre_c, pre_w);
  pax_c[pax_w + 1 + pre_w] = '.';
  if ( u3_yes == ket ) {
    pax_c[pax_w + 1 + pre_w + 1] = '^';
  }
  strncpy(pax_c + pax_w + 1 + pre_w + 1 + ket_w, ext_c, ext_w);
  pax_c[pax_w + 1 + pre_w + 1 + ket_w + ext_w] = '\0';

  free(pre_c); free(ext_c);
  u3z(pre); u3z(ext);

  return pax_c;
}

/* _unix_dir_watch(): instantiate directory tracker.
*/
static void
_unix_dir_watch(u3_udir* dir_u, u3_udir* par_u, c3_c* pax_c)
{
  // (1) build data structure
  //
  dir_u->yes = u3_yes;
  dir_u->dry = u3_no;
  dir_u->pax_c = pax_c;
  dir_u->par_u = par_u;
  dir_u->dis_u = 0;
  dir_u->fil_u = 0;
  dir_u->nex_u = 0;


  // (2) stuff data structure into libuv
  //
  c3_w ret_w = uv_fs_event_init(u3L, &dir_u->was_u );
  if (0 != ret_w){
    uL(fprintf(uH, "event init: %s\n", uv_strerror(ret_w)));
    c3_assert(0);
  }

  // note that we're doing something tricky here; see comment in _unix_fs_event_cb
  //
  ret_w = uv_fs_event_start(&dir_u->was_u,
                           _unix_fs_event_cb,
                           pax_c,
                           0);
  if (0 != ret_w){
    uL(fprintf(uH, "event start: %s\n", uv_strerror(ret_w)));
    c3_assert(0);
  }

}

/* _unix_dir_forge: instantiate directory tracker (and make directory).
*/
static void
_unix_dir_forge(u3_udir* dir_u, u3_udir* par_u, u3_noun tet)
{
  c3_c* tet_c = u3_cr_string(tet);
  c3_w  pax_w = strlen(par_u->pax_c);
  c3_w  tet_w = strlen(tet_c);
  c3_c* pax_c = c3_malloc(pax_w + 1 + tet_w + 1);

  strncpy(pax_c, par_u->pax_c, pax_w + 1);
  pax_c[pax_w] = '/';
  strncpy(pax_c + pax_w + 1, tet_c, tet_w + 1);
  pax_c[pax_w + tet_w + 1] = '\0';

  free(tet_c);
  u3z(tet);

  _unix_mkdir(pax_c);
  _unix_dir_watch(dir_u, par_u, pax_c);
}

/* _unix_file_done(): finish freeing file.
*/
static void
_unix_file_done(uv_handle_t* was_u)
{
  u3_ufil* fil_u = (void*) was_u;

  // uL(fprintf(uH, "file: dun: %s\n", fil_u->pax_c));
  free(fil_u->pax_c);
  mpz_clear(fil_u->mod_mp);
  free(fil_u);
}

/* _unix_file_free(): free (within) file tracker.
*/
static void
_unix_file_free(u3_ufil* fil_u)
{
  // uL(fprintf(uH, "file: free: %s\n", fil_u->pax_c));
  uv_close((uv_handle_t*)&fil_u->was_u, _unix_file_done);
}

#if 0
/* _unix_file_sane(): sanity check file.
*/
static void
_unix_file_sane(u3_ufil* fil_u)
{
}

/* _unix_dir_sane(): sanity check directory.
*/
static void
_unix_dir_sane(u3_udir* dir_u)
{
  u3_udir* dis_u;
  u3_ufil* fil_u;

  for ( dis_u = dir_u->dis_u; dis_u; dis_u = dis_u->nex_u ) {
    _unix_dir_sane(dis_u);
  }
  for ( fil_u = dir_u->fil_u; fil_u; fil_u = fil_u->nex_u ) {
    _unix_file_sane(fil_u);
  }
}
#endif

/* _unix_dir_free(): free (within) directory tracker.
*/
static void _unix_dir_free(u3_udir* dir_u);
static void
_unix_dir_done(uv_handle_t* was_u)
{
  u3_udir* dir_u = (void*) was_u;

  while ( dir_u->dis_u ) {
    u3_udir* nex_u = dir_u->dis_u->nex_u;

    _unix_dir_free(dir_u->dis_u);
    dir_u->dis_u = nex_u;
  }
  while ( dir_u->fil_u ) {
    u3_ufil* nex_u = dir_u->fil_u->nex_u;

    _unix_file_free(dir_u->fil_u);
    dir_u->fil_u = nex_u;
  }

  free(dir_u->pax_c);
  free(dir_u);
}
static void
_unix_dir_free(u3_udir* dir_u)
{
  uv_close((uv_handle_t*)&dir_u->was_u, _unix_dir_done);
}

#if 0
/* _unix_file_update(): update file, true if plausibly changed.
*/
static u3_bean
_unix_file_update(u3_ufil* fil_u, mpz_t mod_mp)
{
  if ( 0 == mpz_cmp(mod_mp, fil_u->mod_mp) ) {
    return u3_no;
  } else {
    mpz_clear(fil_u->mod_mp);
    mpz_init_set(fil_u->mod_mp, mod_mp);

    return u3_yes;
  }
}
#endif

/* _unix_dir_update(): update directory.
*/
static void
_unix_dir_update(u3_udir* dir_u, DIR* rid_u)
{
  if ( u3_yes == dir_u->dry ) {
    return;
  }
  else {
    //  Update all wet subdirectories.
    //
    u3_udir** dis_u;
    u3_ufil** fil_u;

    for ( dis_u = &(dir_u->dis_u); *dis_u; ) {
      if ( u3_yes == (*dis_u)->dry ) {
        dis_u = &(*dis_u)->nex_u;
      }
      else {
        DIR* red_u = opendir((*dis_u)->pax_c);

        if ( 0 == red_u ) {
          u3_udir* ded_u = *dis_u;
          u3_udir* nex_u = ded_u->nex_u;

          // uL(fprintf(uH, "removed directory %s\n", ded_u->pax_c));
          _unix_dir_free(ded_u);

          *dis_u = nex_u;
        }
        else {
          _unix_dir_update(*dis_u, red_u);

          closedir(red_u);
          dis_u = &(*dis_u)->nex_u;
        }
      }
    }

    //  Check all wet files to see if they need deleting.
    //
    for ( fil_u = &(dir_u->fil_u); *fil_u; ) {
      if ( u3_yes == (*fil_u)->dry ) {
        fil_u = &(*fil_u)->nex_u;
      }
      else {
        struct stat buf_u;

        if ( -1 == stat((*fil_u)->pax_c, &buf_u) ||
             !(S_IFREG & buf_u.st_mode) )
        {
          u3_ufil* ded_u = *fil_u;
          u3_ufil* nex_u = ded_u->nex_u;

          // uL(fprintf(uH, "removed file %s\n", ded_u->pax_c));
          _unix_file_free(ded_u);
          *fil_u = nex_u;
        }
        else {
          fil_u = &(*fil_u)->nex_u;
        }
      }
    }

    //  Scan for new files/directories.  XX - this is O(n^2) brute
    //  force, and could be done by smarter event processing.
    //
    while ( 1 ) {
      struct dirent  ent_u;
      struct dirent* out_u;

      if ( readdir_r(rid_u, &ent_u, &out_u) != 0 ) {
        // uL(fprintf(uH, "%s: %s\n", dir_u->pax_c, strerror(errno)));
        c3_assert(0);
      }
      else if ( !out_u ) {
        break;
      }
      else if ( ('.' == out_u->d_name[0]) ) {    //  XX screws up some paths
        continue;
      }
      else {
        c3_c* pax_c = _unix_down(dir_u->pax_c, out_u->d_name);
        struct stat buf_u;

        // uL(fprintf(uH, "  in %s\n", pax_c));
        if ( 0 != stat(pax_c, &buf_u) ) {
          free(pax_c);
          continue;
        }
        else {
          if ( !S_ISDIR(buf_u.st_mode) ) {
            mpz_t    mod_mp;
            u3_ufil* fil_u;

            if ( ( NULL == strrchr(out_u->d_name, '.')) ||
                 ( '~' == out_u->d_name[strlen(out_u->d_name) - 1] )
               ) {
              continue;
            }

            {
              u3_noun mod = c3_stat_mtime(&buf_u);

              u3_cr_mp(mod_mp, mod);
              u3z(mod);
            }
            for ( fil_u = dir_u->fil_u; fil_u; fil_u = fil_u->nex_u ) {
              if ( !strcmp(pax_c, fil_u->pax_c) ) {
                break;
              }
            }
            if ( !fil_u ) {
              fil_u = c3_malloc(sizeof(u3_ufil));

              // uL(fprintf(uH, "found file %s\n", pax_c));
              _unix_file_watch(fil_u, dir_u, pax_c, mod_mp);

              fil_u->nex_u = dir_u->fil_u;
              dir_u->fil_u = fil_u;
            }
            mpz_clear(mod_mp);
          }
          else {
            u3_udir* dis_u;

            for ( dis_u = dir_u->dis_u; dis_u; dis_u = dis_u->nex_u ) {
              if ( !strcmp(pax_c, dis_u->pax_c) ) {
                break;
              }
            }
            if ( !dis_u ) {
              DIR* red_u = _unix_opendir(pax_c);
              dis_u = c3_malloc(sizeof(u3_udir));

              // uL(fprintf(uH, "found directory %s\n", pax_c));
              _unix_dir_watch(dis_u, dir_u, pax_c);
              _unix_dir_update(dis_u, red_u);

              dis_u->nex_u = dir_u->dis_u;
              dir_u->dis_u = dis_u;

              closedir(red_u);
            } else {
              free(pax_c);
            }
          }
        }
      }
    }
  }
}

/* unix_load(): load a file.
*/
static u3_noun
_unix_load(c3_c* pax_c)
{
  struct stat buf_u;
  c3_i        fid_i = open(pax_c, O_RDONLY, 0644);
  c3_w        fln_w, red_w;
  c3_y*       pad_y;

  if ( (fid_i < 0) || (fstat(fid_i, &buf_u) < 0) ) {
    //  ignore if the file disappeared between getting the sync event and now
    if ( ENOENT != errno ) {
      uL(fprintf(uH, "%s: %s\n", pax_c, strerror(errno)));
    }
    return 0;
  }
  fln_w = buf_u.st_size;
  pad_y = c3_malloc(buf_u.st_size);

  red_w = read(fid_i, pad_y, fln_w);
  close(fid_i);

  if ( fln_w != red_w ) {
    free(pad_y);
    c3_assert(0);
    return 0;
  }
  else {
    u3_noun pad = u3_ci_bytes(fln_w, (c3_y *)pad_y);
    free(pad_y);

    return pad;
  }
}

/* unix_save(): save a file.
*/
static void
_unix_save(c3_c* pax_c, u3_atom oat)
{
  c3_i  fid_i = open(pax_c, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  c3_w  fln_w, rit_w;
  c3_y* oat_y;

  if ( fid_i < 0 ) {
    uL(fprintf(uH, "%s: %s\n", pax_c, strerror(errno)));
    u3_cm_bail(c3__fail);
  }

  fln_w = u3_cr_met(3, oat);
  oat_y = c3_malloc(fln_w);
  u3_cr_bytes(0, fln_w, oat_y, oat);
  u3z(oat);

  rit_w = write(fid_i, oat_y, fln_w);
  if ( rit_w != fln_w ) {
    uL(fprintf(uH, "%s: %s\n", pax_c, strerror(errno)));
    c3_assert(0);
  }

  close(fid_i);
  free(oat_y);
}

/* _unix_file_load(): load a file by watcher.
*/
static u3_noun
_unix_file_load(u3_ufil* fil_u)
{
  u3_noun raw = _unix_load(fil_u->pax_c);

  if ( (0 == raw) || ('^' != fil_u->dot_c[1]) ) {
    return raw;
  }
  else return u3_cke_cue(raw);
}


/* _unix_dir_name(): directory name.
*/
static u3_noun
_unix_dir_name(u3_udir* dir_u)
{
  c3_w pel_w = strlen(dir_u->par_u->pax_c);
  c3_c* pax_c = dir_u->pax_c + pel_w + 1;
  c3_c* fas_c = strchr(pax_c, '/');

  return fas_c ? u3_ci_bytes((fas_c - pax_c), (c3_y*) pax_c)
               : u3_ci_string(pax_c);
}

/* _unix_file_tame(): file name/extension for toplevel.
*/
static u3_noun
_unix_file_tame(u3_ufil* fil_u)
{
  c3_c* fas_c = strrchr(fil_u->pax_c, '/');
  c3_c* pax_c = fil_u->pax_c + (fas_c-fil_u->pax_c) + 1;

  if ( !fil_u->dot_c ) {
    return u3_none;
  }
  else {
    c3_c* ext_c = fil_u->dot_c + 1;
    c3_w  nam_w = fil_u->dot_c - pax_c;

    return u3nc(u3_ci_bytes(nam_w, (c3_y*)pax_c),
                u3_ci_string(ext_c));
  }
}

/* _unix_file_name(): file name/extension.
*/
static u3_noun
_unix_file_name(u3_ufil* fil_u)
{
  c3_w pel_w = strlen(fil_u->par_u->pax_c);
  c3_c* pax_c = fil_u->pax_c + pel_w + 1;

  if ( !fil_u->dot_c ) {
    return u3_ci_string(pax_c);
  }
  else {
    c3_c* ext_c = fil_u->dot_c + 1;

    ext_c = (*ext_c == '^') ? (ext_c + 1) : ext_c;
    return u3nc(u3_ci_bytes((fil_u->dot_c - pax_c), (c3_y*)pax_c),
                u3_ci_string(ext_c));
  }
}

/* _unix_dir_ankh_file(): process a file for ankh.
*/
static u3_noun
_unix_dir_ankh_file(u3_noun pam, u3_noun wib, u3_noun baw, u3_noun woz)
{
  u3_weak ole;
  if ( u3_no == u3du(wib) ) {
    ole = u3_ckdb_get(u3k(pam), u3k(wib));

    if ( u3_none == ole ) {
      ole = u3_do("cosh", u3nt(0, woz, u3_nul));
    } else {
      u3_noun elo;

      elo = u3_do("cosh", u3nt(0, woz, u3k(u3t(u3t(ole)))));
      u3z(ole);

      ole = elo;
    }
    pam = u3_ckdb_put(pam, wib, ole);
  }
  else {
    u3_noun fid = u3h(wib);
    u3_noun har = u3t(wib);

    ole = u3_ckdb_get(u3k(pam), u3k(fid));

    if ( u3_none == ole ) {
      ole = u3nt
        (0, u3_nul, u3_ckdb_put(u3_nul,
                                  u3k(har),
                                  u3_do("cosh", u3nt(0, woz, u3_nul))));
      ole = u3_do("cosh", ole);
    }
    else {
      u3_noun roo = u3t(u3t(ole));
      u3_weak tup = u3_ckdb_get(u3k(roo), u3k(har));
      u3_noun oor, elo;

      if ( u3_none == tup ) {
        tup = u3_do("cosh", u3nt(0, woz, u3_nul));
      } else {
        u3_noun upt;

        upt = u3_do("cosh", u3nt(0, woz, u3k(u3t(u3t(tup)))));
        u3z(tup);

        tup = upt;
      }
      oor = u3_ckdb_put(u3k(roo), u3k(har), tup);
      elo = u3_do("cosh", u3nt(0, u3k(u3h(u3t(ole))), oor));

      u3z(ole); ole = elo;
    }
    pam = u3_ckdb_put(pam, u3k(fid), ole);
    u3z(wib);
  }
  return pam;
}

/* _unix_dir_ankh(): resolve directory to new style ankh.
*/
static u3_noun
_unix_dir_ankh(u3_udir* dir_u)
{
  u3_udir* dis_u;
  u3_ufil* fil_u;
  u3_noun pam = u3_nul;

  for ( dis_u = dir_u->dis_u; dis_u; dis_u = dis_u->nex_u ) {
    u3_noun pre = _unix_dir_name(dis_u);
    u3_noun ank = _unix_dir_ankh(dis_u);

    // uL(fprintf(uH, "dir %s\n", u3_cr_string(pre)));
    if ( 0 != u3h(ank) ) {
      pam = u3_ckdb_put(pam, pre, ank);
    }
  }

  if ( !dir_u->par_u->par_u ) {                        //  toplevel
    for ( fil_u = dir_u->par_u->fil_u; fil_u; fil_u = fil_u->nex_u ) {
      //  uL(fprintf(uH, "top %s\n", fil_u->pax_c));
      u3_noun wib = _unix_file_tame(fil_u);
      if ( u3_none == wib ) continue;
      u3_noun dur = _unix_dir_name(dir_u);
      if ( u3_no == u3_cr_sing(dur, u3h(wib)) ) {         //  wrong desk
        u3z(wib); u3z(dur);
        continue;
      }
      u3_noun baw = _unix_file_load(fil_u);
      u3_noun woz = u3nt(u3_nul, u3_do("sham", u3k(baw)), baw);
      u3z(dur);
      pam = _unix_dir_ankh_file(pam, u3k(u3t(wib)), baw, woz);
      u3z(wib);
    }
  }

  for ( fil_u = dir_u->fil_u; fil_u; fil_u = fil_u->nex_u ) {
    u3_noun wib = _unix_file_name(fil_u);
    u3_noun baw = _unix_file_load(fil_u);
    u3_noun woz = u3nt(u3_nul, u3_do("sham", u3k(baw)), baw);
    pam = _unix_dir_ankh_file(pam, wib, baw, woz);
  }
  return u3_do("cosh", u3nt(0, u3_nul, pam));
}

/* _find_mug(): find a noun with a given mug.  retain. DELETEME
*/
#if 0
static u3_weak
_find_mug(u3_noun som, c3_l mug_l)
{
  if ( mug_l == u3_cr_mug(som) ) {
    return som;
  } 
  else {
    u3_weak pro;

    if ( u3_so(u3du(som)) ) {
      if ( (pro = _find_mug(u3h(som), mug_l)) != u3_none ) {
        return pro;
      }
      else return _find_mug(u3t(som), mug_l);
    }
    else return u3_none;
  }
}
#endif

/* _unix_desk_peek(): peek for ankh.
*/
static u3_noun
_unix_desk_peek(u3_noun hox,
                u3_noun syd,
                u3_noun lok)
{
  u3_noun arg;
  u3_noun cay;

  arg = u3nc(c3_s2('c','z'), u3nq(hox, syd, lok, u3_nul));
  cay = u3_cv_peek(arg);

  if ( u3_nul == cay ) {
    return u3nt(0, u3_nul, u3_nul);
  } 
  else {
    u3_noun ank = u3k(u3t(cay));

    u3z(cay); return ank;
  }
}

#if 0 
/* _unix_ankh_sing_map(): compare ankh maps for u3_ankh_sing().
*/
static u3_bean _unix_ankh_sing_in(u3_noun, u3_noun);

static u3_bean
_unix_ankh_sing_map(u3_noun mun, u3_noun mur)           //  retain
{
  u3_noun n_mun, l_mun, r_mun;
  u3_noun n_mur, l_mur, r_mur;

  if ( (u3_nul == mun) && (u3_nul == mur) ) { return u3_yes; }
  if ( (u3_nul == mun) || (u3_nul == mur) ) { return u3_no; }

  u3_cx_trel(mun, &n_mun, &l_mun, &r_mun);
  u3_cx_trel(mur, &n_mur, &l_mur, &r_mur);

  if ( (u3_no == (u3_cr_sing(u3h(n_mun), u3h(n_mur)))) ||
       (u3_no == _unix_ankh_sing_in(u3t(n_mun), u3t(n_mur))) ||
       (u3_no == _unix_ankh_sing_map(l_mun, l_mur)) ||
       (u3_no == _unix_ankh_sing_map(r_mun, r_mur)) ) 
  {
    return u3_no;
  } else return u3_yes;
}

/* _unix_node_sing(): test node equality.
*/
static u3_bean
_unix_node_sing(u3_noun xud, u3_noun bud)
{
  if ( (u3_nul == xud) && (u3_nul == bud) ) { return u3_yes; }
  if ( (u3_nul == xud) || (u3_nul == bud) ) { return u3_no; }

  return u3_cr_sing(u3t(u3t(xud)), u3t(u3t(bud)));
}

/* _unix_ankh_sing_in(): stupid ankh test which ignores broken hash.
*/
static u3_bean
_unix_ankh_sing_in(u3_noun xun, u3_noun bur)               //  retain
{
  u3_noun p_xun, q_xun, r_xun;
  u3_noun p_bur, q_bur, r_bur;

  u3_cx_trel(xun, &p_xun, &q_xun, &r_xun);
  u3_cx_trel(bur, &p_bur, &q_bur, &r_bur);

  if ( u3_no == _unix_node_sing(q_xun, q_bur) ) {
    return u3_no;
  }
  return _unix_ankh_sing_map(r_xun, r_bur);
}

/* _unix_ankh_sing(): full ankh compare.
*/
static u3_bean
_unix_ankh_sing(u3_noun xun, u3_noun bur)                 //  retain
{
  if ( u3_yes == u3_cr_sing(xun, bur) ) {
    return u3_yes;
  } else {
    if ( u3_no == _unix_ankh_sing_in(xun, bur) ) {
      // fprintf(stderr, "uas: no, no (%x, %x)\r\n", u3_mug(xun), u3_mug(bur));
      return u3_no;
    }
    else {
      // fprintf(stderr, "uas: no, yes\r\n");
      return u3_yes;
    }
  }
}
#endif

/* _unix_desk_sync_into(): sync external changes to desk.
*/
static void
_unix_desk_sync_into(u3_noun  who,
                     u3_noun  hox,
                     u3_noun  syd,
                     u3_udir* dir_u)
{
  u3_noun xun, bur, doz, fav, pax;

  xun = _unix_dir_ankh(dir_u);
  bur = _unix_desk_peek(hox, u3k(syd), u3k(u3A->wen));

  if ( (u3_no == u3_cr_sing(u3h(xun), u3h(bur))))
  {
    doz = u3_dc("cost", xun, bur);

    pax = u3nq(u3_blip, c3__sync, u3k(u3A->sen), u3_nul);
    fav = u3nq(c3__into, who, syd, u3nc(u3_yes, doz));

    u3_cv_plan(pax, fav);
  }
  else {
    u3z(who); u3z(syd); u3z(xun); u3z(bur);
  }
}

/* _unix_ship_update(): update top level ship.
*/
static void
_unix_ship_update(u3_uhot* hot_u)
{
  u3_udir* dir_u = &(hot_u->dir_u);

  if ( u3_no == dir_u->dry ) {
    DIR*     rid_u = _unix_opendir(dir_u->pax_c);
    u3_udir* dis_u;
    u3_noun  who, hox;

    _unix_dir_update(dir_u, rid_u);

    {
      mpz_t who_mp;

      mpz_init_set(who_mp, hot_u->who_mp);
      who = u3_ci_mp(who_mp);
      hox = u3_dc("scot", 'p', u3k(who));
    }

    for ( dis_u = dir_u->dis_u; dis_u; dis_u = dis_u->nex_u ) {
      u3_noun syd = _unix_dir_name(dis_u);

      // uL(fprintf(uH, "sync %s %s\n", u3_cr_string(hox), u3_cr_string(syd)));
      _unix_desk_sync_into(u3k(who), u3k(hox), syd, dis_u);
    }
    u3z(hox);
    u3z(who);

    closedir(rid_u);
    _unix_dir_dry(dir_u);
  }
}

/* _unix_hot_gain(): gain ship.
*/
static void
_unix_hot_gain(u3_noun who, u3_bean mek)
{
  u3_noun hox = u3_dc("scot", 'p', u3k(who));
  c3_c*   hox_c = u3_cr_string(hox);
  c3_c*   pax_c = _unix_down(u3_Host.cpu_c, hox_c + 1);
  DIR*    rid_u = opendir(pax_c);

  if ( !rid_u ) {
    if ( u3_yes == mek ) {
      _unix_mkdir(pax_c);
    } else {
      u3z(who);
      u3z(hox);
      return;
    }
  } else closedir(rid_u);

  // uL(fprintf(uH, "GAIN %s\n", pax_c));
  free(hox_c);
  u3z(hox);
  u3_unix_acquire(pax_c);

  {
    u3_uhot* hot_u = c3_malloc(sizeof(u3_uhot));

    _unix_dir_watch(&hot_u->dir_u, 0, pax_c);

    u3_cr_mp(hot_u->who_mp, who);
    u3z(who);

    hot_u->nex_u = u3_Host.unx_u.hot_u;
    u3_Host.unx_u.hot_u = hot_u;
  }
}

/* _unix_hot_lose(): release within a host directory.
*/
static void
_unix_hot_lose(u3_uhot* hot_u)
{
  // uL(fprintf(uH, "lose: %s\n", hot_u->dir_u.pax_c));
  _unix_dir_free(&(hot_u->dir_u));
}

/* _unix_pdir(): find directory reference from text.
*/
static u3_udir**
_unix_pdir(u3_udir* par_u, u3_noun tet)
{
  c3_c*     tet_c = u3_cr_string(tet);
  c3_w      pax_w = strlen(par_u->pax_c);
  u3_udir** dir_u;

  dir_u = &(par_u->dis_u);
  while ( 1 ) {
    if ( !*dir_u || !strcmp(((*dir_u)->pax_c + pax_w + 1), tet_c) ) {
      free(tet_c); return dir_u;
    }
    else dir_u = &((*dir_u)->nex_u);
  }
}

/* _unix_home(): find home directory from identity.
*/
static u3_uhot*
_unix_home(u3_noun who)
{
  u3_unix* unx_u = &u3_Host.unx_u;
  u3_uhot* hot_u;
  mpz_t    who_mp;

  u3_cr_mp(who_mp, who);
  for ( hot_u = unx_u->hot_u;
        hot_u && (0 != mpz_cmp(who_mp, hot_u->who_mp));
        hot_u = hot_u->nex_u )
  {
    // uL(fprintf(uH, "uh: %p, %s\n", hot_u, hot_u->dir_u.pax_c));
  }
  mpz_clear(who_mp);
  return hot_u;
}

/* _unix_desk_sync_udon(): apply udon to existing value.
*/
static u3_noun
_unix_desk_sync_udon(u3_noun don, u3_noun old)
{
  return u3_dc("lump", don, old);
}

/* _unix_desk_sync_tofu(): sync out file install.
*/
static void
_unix_desk_sync_tofu(u3_udir* dir_u,
                     u3_noun  pre,
                     u3_noun  ext,
                     u3_noun  mis)
{
  c3_c*     pox_c = _unix_file_form(dir_u, u3k(pre), u3_no, u3k(ext));
  c3_c*     pux_c = _unix_file_form(dir_u, u3k(pre), u3_yes, u3k(ext));
  u3_ufil** fil_u;

  // uL(fprintf(uH, "tofu pox_c %s op %s\n", pox_c, u3_cr_string(u3h(mis))));

#ifdef SYNCLOG
  c3_w slot = u3_Host.unx_u.lot_w++ % 1024;
  free(u3_Host.unx_u.sylo[slot].pax_c);
  u3_Host.unx_u.sylo[slot].pax_c = 0;
#endif

  fil_u = &(dir_u->fil_u);
  while ( 1 ) {                               //  XX crude!
    if ( !*fil_u ||
         !strcmp((*fil_u)->pax_c, pox_c) ||
         !strcmp((*fil_u)->pax_c, pux_c) )
    {
      break;
    }
    else fil_u = &((*fil_u)->nex_u);
  }

  if ( *fil_u && (c3__del == u3h(mis)) ) {
    u3_ufil* ded_u = *fil_u;

#ifdef SYNCLOG
    u3_Host.unx_u.sylo[slot].unx   = u3_no;
    u3_Host.unx_u.sylo[slot].wer_m = c3_s4('t','o','f','u');
    u3_Host.unx_u.sylo[slot].wot_m = c3__del;
    u3_Host.unx_u.sylo[slot].pax_c = strdup(ded_u->pax_c);
#endif

    *fil_u = ded_u->nex_u;
    _unix_unlink(ded_u->pax_c);
    _unix_file_free(ded_u);

    free(pox_c);
    free(pux_c);
  }
  else {
    u3_noun god, oat;
    c3_c*   pax_c;

    if ( *fil_u ) {
      u3_noun old = _unix_file_load(*fil_u);
      c3_assert(c3__mut == u3h(mis));

      god = _unix_desk_sync_udon(u3k(u3t(mis)), old);
      _unix_unlink((*fil_u)->pax_c);
      free((*fil_u)->pax_c);
    }
    else {
      c3_assert(c3__ins == u3h(mis));
      god = u3k(u3t(mis));
    }

    if ( u3_yes == u3du(god) ) {
      oat = u3_cke_jam(god);
      pax_c = pux_c; free(pox_c);
    } else {
      oat = god;
      pax_c = pox_c; free(pux_c);
    }

#ifdef SYNCLOG
    u3_Host.unx_u.sylo[slot].unx   = u3_no;
    u3_Host.unx_u.sylo[slot].wer_m = c3_s4('t','o','f','u');
    u3_Host.unx_u.sylo[slot].wot_m = u3h(mis);
    u3_Host.unx_u.sylo[slot].pax_c = strdup(pax_c);
#endif

    _unix_save(pax_c, oat);

    if ( *fil_u ) {
      (*fil_u)->dot_c = (pax_c + ((*fil_u)->dot_c - (*fil_u)->pax_c));
      (*fil_u)->pax_c = pax_c;

      mpz_clear((*fil_u)->mod_mp);
      u3_cr_mp((*fil_u)->mod_mp, u3A->now);
    }
    else {
      mpz_t mod_mp;

      u3_cr_mp(mod_mp, u3A->now);
      *fil_u = c3_malloc(sizeof(u3_ufil));

      _unix_file_watch(*fil_u, dir_u, pax_c, mod_mp);
      mpz_clear(mod_mp);
    }
  }
  u3z(pre); u3z(ext); u3z(mis);
}

/* _unix_desk_sync_tako(): sync out change.
*/
static void
_unix_desk_sync_tako(u3_udir* dir_u, u3_noun pax, u3_noun mis)
{
  if ( (u3_no == u3du(pax)) ) {
    c3_assert(!"tack");
  }
  else if ( u3_no == u3du(u3t(pax)) ) {                //  at toplevel
    u3_noun i_pax = u3h(pax);
    u3_noun t_pax = u3t(pax);
    c3_c* par_u = strrchr(dir_u->pax_c, '/') + 1;
    u3_noun pem = u3_ci_string(par_u);
    c3_assert( u3_nul == t_pax );                      //  XX ugly, wrong

    _unix_desk_sync_tofu(dir_u->par_u, pem, u3k(i_pax), mis);
  }
  else {
    u3_noun i_pax = u3h(pax);
    u3_noun t_pax = u3t(pax);
    u3_noun it_pax = u3h(t_pax);
    u3_noun tt_pax = u3t(t_pax);

    if ( u3_nul == tt_pax ) {
      _unix_desk_sync_tofu(dir_u, u3k(i_pax), u3k(it_pax), mis);
    }
    else {
      u3_udir** dis_u = _unix_pdir(dir_u, u3k(i_pax));

      if ( !*dis_u ) {
        *dis_u = c3_malloc(sizeof(u3_udir));

        _unix_dir_forge(*dis_u, dir_u, u3k(i_pax));
      }
      _unix_desk_sync_tako(*dis_u, u3k(t_pax), mis);
    }
  }
  u3z(pax);
}

/* _unix_desk_sync_soba(): sync computed changes.
*/
static void
_unix_desk_sync_soba(u3_udir* dir_u, u3_noun doz)
{
  u3_noun zod = u3t(doz);

  while ( u3_nul != zod ) {
    _unix_desk_sync_tako(dir_u, u3k(u3h(u3h(zod))), u3k(u3t(u3h(zod))));
    zod = u3t(zod);
  }
  u3z(doz);
}

/* _unix_desk_sync_ergo(): sync desk changes to unix.
*/
static void
_unix_desk_sync_ergo(u3_noun  hox,
                     u3_noun  syd,
                     u3_noun  lok,
                     u3_uhot* hot_u)
{
  u3_udir** dir_u = _unix_pdir(&(hot_u->dir_u), syd);
  u3_noun   xun;

#if 0
  uL(fprintf(uH, "ergo %s %s %s\n", u3_cr_string(hox),
                                    u3_cr_string(syd),
                                    u3_cr_string(lok)));
#endif

  if ( !*dir_u ) {
    *dir_u = c3_malloc(sizeof(u3_udir));

    xun = u3nt(0, u3_nul, u3_nul);
    _unix_dir_forge(*dir_u, &(hot_u->dir_u), u3k(syd));
  } else {
    xun = _unix_dir_ankh(*dir_u);
  }

  {
    u3_noun bur = _unix_desk_peek(hox, syd, lok);

    if ( u3_no == u3_cr_sing(xun, bur) ) {
      u3_noun doz = u3_dc("cost", bur, xun);

      _unix_desk_sync_soba(*dir_u, doz);
    }
    else {
      u3z(xun); u3z(bur);
    }
  }
}

/* u3_unix_ef_init(): update filesystem for new acquisition.
*/
void
u3_unix_ef_init(u3_noun who)
{
  _unix_hot_gain(u3k(who), u3_yes);

  u3_cv_plan(u3nq(u3_blip, c3__sync, u3k(u3A->sen), u3_nul),
             u3nq(c3__into, who,
                            u3_blip,
                            u3nt(u3_yes, u3nc(0, 0), u3_nul)));
}

/* u3_unix_ef_ergo(): update filesystem, outbound.
*/
void
u3_unix_ef_ergo(u3_noun who,
                u3_noun syd,
                u3_noun rel)
{
  u3_noun  hox = u3_dc("scot", 'p', u3k(who));
  u3_noun  lok = u3_dc("scot", c3__ud, rel);
  u3_uhot* hot_u;

  hot_u = _unix_home(who);

  if ( 0 != hot_u ) {
    _unix_desk_sync_ergo(hox, syd, lok, hot_u);
  }
}

/* u3_unix_ef_look(): update the root.
*/
void
u3_unix_ef_look(void)
{
  u3_unix* unx_u = &u3_Host.unx_u;
  u3_noun  won;
  u3_uhot* hot_u;

  if ( u3_nul != u3A->roe ) {
    //  We can't generate a working %into event here because there
    //  are other events, possibly containing %into, that are queued;
    //  they will change the state of %clay and cause a patch that
    //  doesn't work.
    //
    return;
  }

  //  find owners without directories
  {
    for ( won = u3A->own; u3_nul != won; won = u3t(won) ) {
      u3_noun who = u3h(won);
      mpz_t who_mp;

      u3_cr_mp(who_mp, who);
      for ( hot_u = unx_u->hot_u;
            hot_u && (0 != mpz_cmp(who_mp, hot_u->who_mp));
            hot_u = hot_u->nex_u );

      mpz_clear(who_mp);
      if ( 0 == hot_u ) {
        _unix_hot_gain(u3k(who), u3_no);
      }
    }
  }

  //  find directories without owners
  {
    u3_uhot** het_u = &(unx_u->hot_u);

    while ( 0 != (hot_u=*het_u) ) {
      for ( won = u3A->own; u3_nul != won; won = u3t(won) ) {
        u3_noun who = u3h(won);
        mpz_t   who_mp;
        c3_w    cmp_w;

        u3_cr_mp(who_mp, who);
        cmp_w = mpz_cmp(who_mp, hot_u->who_mp);
        mpz_clear(who_mp);
        if ( 0 == cmp_w ) {
          break;
        }
      }

      if ( u3_nul == won ) {
        *het_u = hot_u->nex_u;

        // uL(fprintf(uH, "sync: lose %s\n", hot_u->dir_u.pax_c));
        _unix_hot_lose(hot_u);

        free(hot_u);
        continue;
      }
      else {
        het_u = &(hot_u->nex_u);
      }
    }
  }

  //  update all ships
  {
    u3_uhot* hot_u;

    for ( hot_u = unx_u->hot_u; hot_u; hot_u = hot_u->nex_u ) {
      _unix_ship_update(hot_u);
    }
  }
}

/* _unix_ef_sync(): check for files to sync.
 */
static void
_unix_ef_sync(uv_check_t* han_u)
{
  u3_lo_open();
  u3_lo_shut(u3_yes);
}

/* _unix_sign_cb: signal callback.
*/
static void
_unix_sign_cb(uv_signal_t* sil_u, c3_i num_i)
{
  u3_lo_open();
  {
    switch ( num_i ) {
      default: fprintf(stderr, "\r\nmysterious signal %d\r\n", num_i); break;

      case SIGTERM:
        fprintf(stderr, "\r\ncaught signal %d\r\n", num_i);
        u3_Host.liv = u3_no;
        break;
      case SIGINT: 
        fprintf(stderr, "\r\ninterrupt\r\n");
        u3_term_ef_ctlc(); 
        break;
      case SIGWINCH: u3_term_ef_winc(); break;
      // case SIGCHLD: u3_save_ef_chld(); break;
    }
  }
  u3_lo_shut(u3_yes);
}

/* u3_unix_ef_hold()
*/
void
u3_unix_ef_hold(void)
{
  u3_unix* unx_u = &u3_Host.unx_u;
  u3_usig* sig_u;

  for ( sig_u = unx_u->sig_u; sig_u; sig_u = sig_u->nex_u ) {
    uv_signal_stop(&sig_u->sil_u);
  }
}

/* u3_unix_ef_move()
*/
void
u3_unix_ef_move(void)
{
  u3_unix* unx_u = &u3_Host.unx_u;
  u3_usig* sig_u;

  for ( sig_u = unx_u->sig_u; sig_u; sig_u = sig_u->nex_u ) {
    uv_signal_start(&sig_u->sil_u, _unix_sign_cb, sig_u->num_i);
  }
}

/* u3_unix_io_init(): initialize unix sync.
*/
void
u3_unix_io_init(void)
{
  u3_unix* unx_u = &u3_Host.unx_u;

  {
    u3_usig* sig_u;

    sig_u = c3_malloc(sizeof(u3_usig));
    uv_signal_init(u3L, &sig_u->sil_u);

    sig_u->num_i = SIGTERM;
    sig_u->nex_u = unx_u->sig_u;
    unx_u->sig_u = sig_u;
  }
  {
    u3_usig* sig_u;

    sig_u = c3_malloc(sizeof(u3_usig));
    uv_signal_init(u3L, &sig_u->sil_u);

    sig_u->num_i = SIGINT;
    sig_u->nex_u = unx_u->sig_u;
    unx_u->sig_u = sig_u;
  }
  {
    u3_usig* sig_u;

    sig_u = c3_malloc(sizeof(u3_usig));
    uv_signal_init(u3L, &sig_u->sil_u);

    sig_u->num_i = SIGWINCH;
    sig_u->nex_u = unx_u->sig_u;
    unx_u->sig_u = sig_u;
  }
#if SYNCLOG
  unx_u->lot_w = 0;
  memset(unx_u->sylo, 0, sizeof(unx_u->sylo));
#endif
  uv_check_init(u3_Host.lup_u, &u3_Host.unx_u.syn_u);
}

/* u3_unix_io_talk(): start listening for fs events.
*/
void
u3_unix_io_talk()
{
  u3_unix_acquire(u3_Host.cpu_c);
  u3_unix_ef_move();
  uv_check_start(&u3_Host.unx_u.syn_u, _unix_ef_sync);
}

/* u3_unix_io_exit(): terminate unix I/O.
*/
void
u3_unix_io_exit(void)
{
  uv_check_stop(&u3_Host.unx_u.syn_u);
  u3_unix_release(u3_Host.cpu_c);

  {
    u3_uhot* hot_u;

    for ( hot_u = u3_Host.unx_u.hot_u; hot_u; hot_u = hot_u->nex_u ) {
      u3_unix_release(hot_u->dir_u.pax_c);
    }
  }
#ifdef SYNCLOG
  for (int i = 0; i<1024; i++) {
    free(u3_Host.unx_u.sylo[i].pax_c);
  }
#endif
}

/* u3_unix_io_poll(): update unix IO state.
*/
void
u3_unix_io_poll(void)
{
}
