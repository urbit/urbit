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

/* _unix_ship_in(): c3_w[4] to ship.
*/
static u3_noun 
_unix_ship_in(c3_w* who_w)
{
  return u3i_words(4, who_w);
}

/* _unix_ship_out(): ship to c3_w[4].  RETAIN.
*/
static void
_unix_ship_out(u3_noun own, c3_w* who_w)
{
  u3r_words(0, 4, who_w, own);
  {
    u3_noun inn = _unix_ship_in(who_w);

    c3_assert(_(u3r_sing(inn, own)));
    u3z(inn);
  }
}

/* _unix_ship_sing(): yes iff two ships are identical.
*/
static c3_o
_unix_ship_sing(c3_w* one_w, c3_w* two_w)
{
  return __((one_w[0] == two_w[0]) &&
            (one_w[1] == two_w[1]) &&
            (one_w[2] == two_w[2]) &&
            (one_w[3] == two_w[3]));
}

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
  if ( 0 != unlink(pax_c) && ENOENT != errno ) {
    uL(fprintf(uH, "error unlinking %s: %s\n", pax_c, strerror(errno)));
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
    if ( 1 != fscanf(loq_u, "%" SCNu32, &pid_w) ) {
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

  dir_u->dry = c3y;
  for ( dis_u = dir_u->dis_u; dis_u; dis_u = dis_u->nex_u ) {
    _unix_dir_dry(dis_u);
  }
  for ( fil_u = dir_u->fil_u; fil_u; fil_u = fil_u->nex_u ) {
    fil_u->dry = c3y;
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
  u3_Host.unx_u.sylo[slot].unx   = c3y;
  u3_Host.unx_u.sylo[slot].wer_m = c3_s4('u','v','s','y');
  u3_Host.unx_u.sylo[slot].wot_m = 0;
  u3_Host.unx_u.sylo[slot].pax_c = strdup(nod_u->pax_c);
#endif

  {
    while ( nod_u ) {
      nod_u->dry = c3n;
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
                 c3_c*    pot_c,
                 c3_w*    mod_w)
{
  // (1) build data structure
  //
  fil_u->non = c3n;
  fil_u->dry = c3n;
  fil_u->pax_c = pax_c;
  fil_u->pot_c = pot_c;
  {
    c3_c* dot_c = strrchr(pax_c, '.');
    c3_c* fas_c = strrchr(pax_c, '/');

    fil_u->dot_c = dot_c ? (fas_c ? ((dot_c > fas_c) ? dot_c : 0)
                            : dot_c)
      : 0;
  }
  fil_u->par_u = dir_u;
  fil_u->mod_w[0] = mod_w[0]; fil_u->mod_w[1] = mod_w[1];
  fil_u->mod_w[2] = mod_w[2]; fil_u->mod_w[3] = mod_w[3];
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
_unix_file_form(c3_c* pax_c,
                u3_noun  pre,
                u3_noun  ket,
                u3_noun  ext)
{
  c3_c* pre_c = u3r_string(pre);
  c3_c* ext_c = u3r_string(ext);
  c3_w  pax_w = strlen(pax_c);
  c3_w  pre_w = strlen(pre_c);
  c3_w  ext_w = strlen(ext_c);
  c3_w  ket_w = (c3y == ket) ? 1 : 0;
  c3_c* pox_c = c3_malloc(pax_w + 1 + pre_w + 1 + ket_w + ext_w + 1);

  strncpy(pox_c, pax_c, pax_w);
  pox_c[pax_w] = '/';
  strncpy(pox_c + pax_w + 1, pre_c, pre_w);
  pox_c[pax_w + 1 + pre_w] = '.';
  if ( c3y == ket ) {
    pox_c[pax_w + 1 + pre_w + 1] = '^';
  }
  strncpy(pox_c + pax_w + 1 + pre_w + 1 + ket_w, ext_c, ext_w);
  pox_c[pax_w + 1 + pre_w + 1 + ket_w + ext_w] = '\0';

  free(pre_c); free(ext_c);
  u3z(pre); u3z(ext);

  return pox_c;
}

/* _unix_dir_watch(): instantiate directory tracker.
*/
static void
_unix_dir_watch(u3_udir* dir_u, u3_udir* par_u, c3_c* pax_c, c3_c* pot_c)
{
  // (1) build data structure
  //
  dir_u->yes = c3y;
  dir_u->dry = c3n;
  dir_u->pax_c = pax_c;
  dir_u->pot_c = pot_c;
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
  c3_c* tet_c = u3r_string(tet);
  c3_w  pax_w = strlen(par_u->pax_c);
  c3_w  tet_w = strlen(tet_c);
  c3_c* pax_c = c3_malloc(pax_w + 1 + tet_w + 1);
  c3_c* pot_c = c3_malloc(pax_w + 1 + 1 + tet_w + 1);

  strncpy(pax_c, par_u->pax_c, pax_w + 1);
  pax_c[pax_w] = '/';
  strncpy(pax_c + pax_w + 1, tet_c, tet_w + 1);
  pax_c[pax_w + tet_w + 1] = '\0';

  strncpy(pot_c, par_u->pot_c, pax_w + 1 + 1);
  pot_c[pax_w + 1] = '/';
  strncpy(pot_c + pax_w + 1 + 1, tet_c, tet_w + 1);
  pot_c[pax_w + 1 + tet_w + 1] = '\0';

  free(tet_c);
  u3z(tet);

  _unix_mkdir(pax_c);
  _unix_mkdir(pot_c);
  _unix_dir_watch(dir_u, par_u, pax_c, pot_c);
}

/* _unix_file_done(): finish freeing file.
*/
static void
_unix_file_done(uv_handle_t* was_u)
{
  u3_ufil* fil_u = (void*) was_u;

  // uL(fprintf(uH, "file: dun: %s\n", fil_u->pax_c));
  free(fil_u->pax_c);
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

/* _unix_dir_update(): update directory.
*/
static void
_unix_dir_update(u3_udir* dir_u, DIR* rid_u)
{
  if ( c3y == dir_u->dry ) {
    return;
  }
  else {
    //  Update all wet subdirectories.
    //
    u3_udir** dis_u;
    u3_ufil** fil_u;

    for ( dis_u = &(dir_u->dis_u); *dis_u; ) {
      if ( c3y == (*dis_u)->dry ) {
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
      if ( c3y == (*fil_u)->dry ) {
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
        c3_c* pot_c = _unix_down(dir_u->pot_c, out_u->d_name);
        struct stat buf_u;

        // uL(fprintf(uH, "  in %s\n", pax_c));
        if ( 0 != stat(pax_c, &buf_u) ) {
          free(pax_c);
          free(pot_c);
          continue;
        }
        else {
          if ( !S_ISDIR(buf_u.st_mode) ) {
            c3_w     mod_w[4];
            u3_ufil* fil_u;

            if ( ( NULL == strrchr(out_u->d_name, '.')) ||
                 ( '~' == out_u->d_name[strlen(out_u->d_name) - 1] )
               ) {
              continue;
            }

            {
              u3_noun mod = c3_stat_mtime(&buf_u);
              
              u3r_words(0, 4, mod_w, mod);
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
              _unix_file_watch(fil_u, dir_u, pax_c, pot_c, mod_w);

              fil_u->nex_u = dir_u->fil_u;
              dir_u->fil_u = fil_u;
            }
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
              _unix_dir_watch(dis_u, dir_u, pax_c, pot_c);
              _unix_dir_update(dis_u, red_u);

              dis_u->nex_u = dir_u->dis_u;
              dir_u->dis_u = dis_u;

              closedir(red_u);
            } else {
              free(pax_c);
              free(pot_c);
            }
          }
        }
      }
    }
  }
}

/* unix_load(): load a file as a cage
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
  pad_y = c3_malloc(fln_w);

  red_w = read(fid_i, pad_y, fln_w);
  close(fid_i);

  if ( fln_w != red_w ) {
    free(pad_y);
    c3_assert(0);
    return 0;
  }
  else {
    //  -:!>(*[[@tas @tas ~] @ud @])
    u3_noun typ = u3nt(c3__cell,
                       u3nt(c3__cell,
                            u3nc(c3__atom, c3__tas),
                            u3nt(c3__cell,
                                 u3nc(c3__atom, c3__tas),
                                 u3nt(c3__cube,
                                      u3_nul,
                                      u3nc(c3__atom, 'n')))),
                       u3nt(c3__cell,
                            u3nc(c3__atom, c3__ud),
                            u3nc(c3__atom, u3_nul)));
    u3_noun nam = u3nt(c3__text, u3i_string("plain"), u3_nul);
    u3_noun pad = u3nt(nam, fln_w, u3i_bytes(fln_w, (c3_y *)pad_y));
    u3_noun cay = u3nt(c3__mime, typ, pad);
    free(pad_y);

    return cay;
  }
}

/* unix_save(): save a file.
*/
static void
_unix_save(c3_c* pax_c, u3_atom oat)
{
  c3_i  fid_i = open(pax_c, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  c3_w  fln_w, rit_w, siz_w;
  c3_y* oat_y;

  if ( fid_i < 0 ) {
    uL(fprintf(uH, "error opening %s: %s\n", pax_c, strerror(errno)));
    u3m_bail(c3__fail);
  }

  siz_w = u3h(u3t(oat));
  fln_w = u3r_met(3, u3t(u3t(oat)));
  oat_y = c3_malloc(siz_w);
  memset(oat_y, 0, siz_w);

  u3r_bytes(0, fln_w, oat_y, u3t(u3t(oat)));
  u3z(oat);

  rit_w = write(fid_i, oat_y, siz_w);
  if ( rit_w != siz_w ) {
    uL(fprintf(uH, "error writing %s: %s\n", pax_c, strerror(errno)));
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
  return _unix_load(fil_u->pax_c);

#if 0
  if ( (0 == raw) || ('^' != fil_u->dot_c[1]) ) {
    return raw;
  }

  else return u3ke_cue(raw);
#endif
}


/* _unix_dir_name(): directory name.
*/
static u3_noun
_unix_dir_name(u3_udir* dir_u)
{
  c3_w pel_w = strlen(dir_u->par_u->pax_c);
  c3_c* pax_c = dir_u->pax_c + pel_w + 1;
  c3_c* fas_c = strchr(pax_c, '/');

  return fas_c ? u3i_bytes((fas_c - pax_c), (c3_y*) pax_c)
               : u3i_string(pax_c);
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

    return u3nc(u3i_bytes(nam_w, (c3_y*)pax_c),
                u3i_string(ext_c));
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
    return u3i_string(pax_c);
  }
  else {
    c3_c* ext_c = fil_u->dot_c + 1;

    ext_c = (*ext_c == '^') ? (ext_c + 1) : ext_c;
    return u3nc(u3i_bytes((fil_u->dot_c - pax_c), (c3_y*)pax_c),
                u3i_string(ext_c));
  }
}

/* _unix_dir_khan_file(): process a file for khan.
*/
static u3_noun
_unix_dir_khan_file(u3_noun pam, u3_noun wib, u3_noun baw, u3_noun woz)
{
  u3_weak ole;
  if ( c3n == u3du(wib) ) {
    ole = u3kdb_get(u3k(pam), u3k(wib));

    if ( u3_none == ole ) {
      ole = u3nc(woz, u3_nul);
    } else {
      u3_noun elo;

      elo = u3nc(woz, u3k(u3t(ole)));
      u3z(ole);

      ole = elo;
    }
    pam = u3kdb_put(pam, wib, ole);
  }
  else {
    u3_noun fid = u3h(wib);
    u3_noun har = u3t(wib);

    ole = u3kdb_get(u3k(pam), u3k(fid));

    if ( u3_none == ole ) {
      ole = u3nc(u3_nul,
                 u3kdb_put(u3_nul,
                           u3k(har),
                           u3nc(woz, u3_nul)));
    }
    else {
      u3_noun roo = u3t(ole);
      u3_weak tup = u3kdb_get(u3k(roo), u3k(har));
      u3_noun oor, elo;

      if ( u3_none == tup ) {
        tup = u3nc(woz, u3_nul);
      } else {
        u3_noun upt;

        upt = u3nc(woz, u3k(u3t(tup)));
        u3z(tup);

        tup = upt;
      }
      oor = u3kdb_put(u3k(roo), u3k(har), tup);
      elo = u3nc(u3k(u3h(ole)), oor);

      u3z(ole); ole = elo;
    }
    pam = u3kdb_put(pam, u3k(fid), ole);
    u3z(wib);
  }
  return pam;
}

/* _unix_dir_khan(): resolve directory to khan.
*/
static u3_noun
_unix_dir_khan(u3_udir* dir_u)
{
  u3_udir* dis_u;
  u3_ufil* fil_u;
  u3_noun pam = u3_nul;

  for ( dis_u = dir_u->dis_u; dis_u; dis_u = dis_u->nex_u ) {
    u3_noun pre = _unix_dir_name(dis_u);
    u3_noun kan = _unix_dir_khan(dis_u);

    // uL(fprintf(uH, "dir %s\n", u3r_string(pre)));
    if ( u3_nul != u3h(kan) || u3_nul != u3t(kan) ) {
      pam = u3kdb_put(pam, pre, kan);
    }
    else
    {
      u3z(kan);
    }
  }

  if ( !dir_u->par_u->par_u ) {                        //  toplevel
    for ( fil_u = dir_u->par_u->fil_u; fil_u; fil_u = fil_u->nex_u ) {
      //  uL(fprintf(uH, "top %s\n", fil_u->pax_c));
      u3_noun wib = _unix_file_tame(fil_u);
      if ( u3_none == wib ) continue;
      u3_noun dur = _unix_dir_name(dir_u);
      if ( c3n == u3r_sing(dur, u3h(wib)) ) {         //  wrong desk
        u3z(wib); u3z(dur);
        continue;
      }
      u3_noun baw = _unix_file_load(fil_u);
      u3_noun woz = u3nt(u3_nul, u3do("sham", u3k(u3t(u3t(baw)))), baw);
      u3z(dur);
      pam = _unix_dir_khan_file(pam, u3k(u3t(wib)), baw, woz);
      u3z(wib);
    }
  }

  for ( fil_u = dir_u->fil_u; fil_u; fil_u = fil_u->nex_u ) {
    u3_noun wib = _unix_file_name(fil_u);
    u3_noun baw = _unix_file_load(fil_u);
    if (0 == baw) {
      pam = _unix_dir_khan_file(pam, wib, baw, u3nc(u3_nul, u3_nul));
    } else {
      pam = _unix_dir_khan_file(pam, wib, baw, u3nt(u3_nul, u3_nul, baw));
    }
  }
  return u3nc(u3_nul, pam);
}

/* _find_mug(): find a noun with a given mug.  retain. DELETEME
*/
#if 0
static u3_weak
_find_mug(u3_noun som, c3_l mug_l)
{
  if ( mug_l == u3r_mug(som) ) {
    return som;
  } 
  else {
    u3_weak pro;

    if ( _(u3du(som)) ) {
      if ( (pro = _find_mug(u3h(som), mug_l)) != u3_none ) {
        return pro;
      }
      else return _find_mug(u3t(som), mug_l);
    }
    else return u3_none;
  }
}
#endif

#if 0
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
  cay = u3v_peek(arg);

  if ( u3_nul == cay ) {
    return u3nt(0, u3_nul, u3_nul);
  } 
  else {
    u3_noun ank = u3k(u3t(cay));

    u3z(cay); return ank;
  }
}
#endif

#if 0 
/* _unix_ankh_sing_map(): compare ankh maps for u3_ankh_sing().
*/
static u3_noun _unix_ankh_sing_in(u3_noun, u3_noun);

static u3_noun
_unix_ankh_sing_map(u3_noun mun, u3_noun mur)           //  retain
{
  u3_noun n_mun, l_mun, r_mun;
  u3_noun n_mur, l_mur, r_mur;

  if ( (u3_nul == mun) && (u3_nul == mur) ) { return c3y; }
  if ( (u3_nul == mun) || (u3_nul == mur) ) { return c3n; }

  u3x_trel(mun, &n_mun, &l_mun, &r_mun);
  u3x_trel(mur, &n_mur, &l_mur, &r_mur);

  if ( (c3n == (u3r_sing(u3h(n_mun), u3h(n_mur)))) ||
       (c3n == _unix_ankh_sing_in(u3t(n_mun), u3t(n_mur))) ||
       (c3n == _unix_ankh_sing_map(l_mun, l_mur)) ||
       (c3n == _unix_ankh_sing_map(r_mun, r_mur)) ) 
  {
    return c3n;
  } else return c3y;
}

/* _unix_node_sing(): test node equality.
*/
static u3_noun
_unix_node_sing(u3_noun xud, u3_noun bud)
{
  if ( (u3_nul == xud) && (u3_nul == bud) ) { return c3y; }
  if ( (u3_nul == xud) || (u3_nul == bud) ) { return c3n; }

  return u3r_sing(u3t(u3t(xud)), u3t(u3t(bud)));
}

/* _unix_ankh_sing_in(): stupid ankh test which ignores broken hash.
*/
static u3_noun
_unix_ankh_sing_in(u3_noun xun, u3_noun bur)               //  retain
{
  u3_noun p_xun, q_xun, r_xun;
  u3_noun p_bur, q_bur, r_bur;

  u3x_trel(xun, &p_xun, &q_xun, &r_xun);
  u3x_trel(bur, &p_bur, &q_bur, &r_bur);

  if ( c3n == _unix_node_sing(q_xun, q_bur) ) {
    return c3n;
  }
  return _unix_ankh_sing_map(r_xun, r_bur);
}

/* _unix_ankh_sing(): full ankh compare.
*/
static u3_noun
_unix_ankh_sing(u3_noun xun, u3_noun bur)                 //  retain
{
  if ( c3y == u3r_sing(xun, bur) ) {
    return c3y;
  } else {
    if ( c3n == _unix_ankh_sing_in(xun, bur) ) {
      // fprintf(stderr, "uas: no, no (%x, %x)\r\n", u3_mug(xun), u3_mug(bur));
      return c3n;
    }
    else {
      // fprintf(stderr, "uas: no, yes\r\n");
      return c3y;
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
  u3_noun xun, fav, pax;

  xun = _unix_dir_khan(dir_u);

  pax = u3nq(u3_blip, c3__sync, u3k(u3A->sen), u3_nul);
  fav = u3nq(c3__into, who, syd, xun);

  u3v_plan(pax, fav);
}

/* _unix_ship_update(): update top level ship.
*/
static void
_unix_ship_update(u3_uhot* hot_u)
{
  u3_udir* dir_u = &(hot_u->dir_u);

  if ( c3n == dir_u->dry ) {
    DIR*     rid_u = _unix_opendir(dir_u->pax_c);
    u3_udir* dis_u;
    u3_noun  who = _unix_ship_in(hot_u->who_w);
    u3_noun  hox = u3dc("scot", 'p', u3k(who));

    _unix_dir_update(dir_u, rid_u);
    for ( dis_u = dir_u->dis_u; dis_u; dis_u = dis_u->nex_u ) {
      u3_noun syd = _unix_dir_name(dis_u);

      // uL(fprintf(uH, "sync %s %s\n", u3r_string(hox), u3r_string(syd)));
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
_unix_hot_gain(u3_noun who, u3_noun mek)
{
  u3_noun hox = u3dc("scot", 'p', u3k(who));
  c3_c*   hox_c = u3r_string(hox);
  c3_c*   pax_c = _unix_down(u3_Host.dir_c, hox_c + 1);
  c3_c*   pin_c = _unix_down(pax_c, "in");
  c3_c*   pot_c = _unix_down(pax_c, "out");
  DIR*    rid_u = opendir(pax_c);
  DIR*    rin_u = opendir(pin_c);
  DIR*    rot_u = opendir(pot_c);

  free(pax_c);

  if ( !rid_u ) {
    if ( c3y == mek ) {
      _unix_mkdir(pax_c);
    } else {
      u3z(who);
      u3z(hox);
      return;
    }
  } else closedir(rid_u);
  if ( !rin_u ) {
    if ( c3y == mek ) {
      _unix_mkdir(pin_c);
    } else {
      u3z(who);
      u3z(hox);
      return;
    }
  } else closedir(rin_u);
  if ( !rot_u ) {
    if ( c3y == mek ) {
      _unix_mkdir(pot_c);
    } else {
      u3z(who);
      u3z(hox);
      return;
    }
  } else closedir(rot_u);

  // uL(fprintf(uH, "GAIN %s\n", pax_c));

  free(hox_c);
  u3z(hox);
  u3_unix_acquire(pin_c);

  {
    u3_uhot* hot_u = c3_malloc(sizeof(u3_uhot));

    _unix_dir_watch(&hot_u->dir_u, 0, pin_c, pot_c);

    _unix_ship_out(who, hot_u->who_w);
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
  c3_c*     tet_c = u3r_string(tet);
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
  c3_w     who_w[4];

  _unix_ship_out(who, who_w);
  for ( hot_u = unx_u->hot_u;
        hot_u && !_(_unix_ship_sing(who_w, hot_u->who_w));
        hot_u = hot_u->nex_u )
  {
    // uL(fprintf(uH, "uh: %p, %s\n", hot_u, hot_u->dir_u.pax_c));
  }
  return hot_u;
}

#if 0
/* _unix_desk_sync_udon(): apply udon to existing value.
*/
static u3_noun
_unix_desk_sync_udon(u3_noun don, u3_noun old)
{
  return u3dc("lump", don, old);
}
#endif

/* _unix_desk_sync_tofu(): sync out file install.
*/
static void
_unix_desk_sync_tofu(u3_udir* dir_u,
                     u3_noun  pre,
                     u3_noun  ext,
                     u3_noun  mim)
{
  c3_c*     pox_c = _unix_file_form(dir_u->pax_c, u3k(pre), c3n, u3k(ext));
  c3_c*     pot_c = _unix_file_form(dir_u->pot_c, u3k(pre), c3n, u3k(ext));
  c3_c*     pux_c = _unix_file_form(dir_u->pax_c, u3k(pre), c3y, u3k(ext));
  c3_c*     put_c = _unix_file_form(dir_u->pot_c, u3k(pre), c3y, u3k(ext));
  u3_ufil** fil_u;

  // uL(fprintf(uH, "tofu pox_c %s op %s\n", pox_c, u3r_string(u3h(mis))));

#ifdef SYNCLOG
  c3_w slot = u3_Host.unx_u.lot_w++ % 1024;
  free(u3_Host.unx_u.sylo[slot].pax_c);
  u3_Host.unx_u.sylo[slot].pax_c = 0;
#endif

  fil_u = &(dir_u->fil_u);
  while ( 1 ) {                               //  XX crude!
    if ( !*fil_u ||
         !strcmp((*fil_u)->pax_c, pox_c) /* ||
         !strcmp((*fil_u)->pax_c, pux_c) */ )
    {
      break;
    }
    else fil_u = &((*fil_u)->nex_u);
  }

  if ( *fil_u && u3_nul == mim ) {
    uL(fprintf(uH, "file goning: %s\n", pox_c));
    u3_ufil* ded_u = *fil_u;

#ifdef SYNCLOG
    u3_Host.unx_u.sylo[slot].unx   = c3n;
    u3_Host.unx_u.sylo[slot].wer_m = c3_s4('t','o','f','u');
    u3_Host.unx_u.sylo[slot].wot_m = c3__del;
    u3_Host.unx_u.sylo[slot].pax_c = strdup(ded_u->pax_c);
#endif

    *fil_u = ded_u->nex_u;
    _unix_unlink(ded_u->pax_c);
    _unix_unlink(ded_u->pot_c);
    _unix_file_free(ded_u);
    free(ded_u->pot_c);

    free(pox_c);
    free(pot_c);
    free(pux_c);
    free(put_c);
  }
  else if (u3_nul == mim) {
    uL(fprintf(uH, "file already gone: %s\n", pox_c));

    free(pox_c);
    free(pot_c);
    free(pux_c);
    free(put_c);
  }
  else {
    c3_c*   pax_c;
    c3_c*   pat_c;

    if ( *fil_u ) {
      _unix_unlink((*fil_u)->pax_c);
      free((*fil_u)->pax_c);
      _unix_unlink((*fil_u)->pot_c);
      free((*fil_u)->pot_c);
    }

#if 0
    if ( c3y == u3du(god) ) {
      pax_c = pux_c; free(pox_c);
      pat_c = put_c; free(pot_c);
    } else {
#endif
      pax_c = pox_c; free(pux_c);
      pat_c = pot_c; free(put_c);
#if 0
    }
#endif

#ifdef SYNCLOG
    u3_Host.unx_u.sylo[slot].unx   = c3n;
    u3_Host.unx_u.sylo[slot].wer_m = c3_s4('t','o','f','u');
    u3_Host.unx_u.sylo[slot].wot_m = u3h(mis);
    u3_Host.unx_u.sylo[slot].pax_c = strdup(pax_c);
#endif

    _unix_save(pax_c, u3k(u3t(mim)));
    _unix_save(pat_c, u3t(mim));

    if ( *fil_u ) {
      (*fil_u)->dot_c = (pax_c + ((*fil_u)->dot_c - (*fil_u)->pax_c));
      (*fil_u)->pax_c = pax_c;
      (*fil_u)->pot_c = pat_c;

      u3r_words(0, 4, (*fil_u)->mod_w, u3A->now);
    }
    else {
      c3_w mod_w[4];

      u3r_words(0, 4, mod_w, u3A->now);
      *fil_u = c3_malloc(sizeof(u3_ufil));

      _unix_file_watch(*fil_u, dir_u, pax_c, pat_c, mod_w);
    }
  }

  u3z(pre); u3z(ext); u3z(mim);
}

/* _unix_desk_sync_tako(): sync out change.
*/
static void
_unix_desk_sync_tako(u3_udir* dir_u, u3_noun pax, u3_noun mim)
{
  if ( (c3n == u3du(pax)) ) {
    c3_assert(!"tack");
  }
  else if ( c3n == u3du(u3t(pax)) ) {                //  at toplevel
    u3_noun i_pax = u3h(pax);
    u3_noun t_pax = u3t(pax);
    c3_c* par_u = strrchr(dir_u->pax_c, '/') + 1;
    u3_noun pem = u3i_string(par_u);
    c3_assert( u3_nul == t_pax );                      //  XX ugly, wrong

    _unix_desk_sync_tofu(dir_u->par_u, pem, u3k(i_pax), mim);
  }
  else {
    u3_noun i_pax = u3h(pax);
    u3_noun t_pax = u3t(pax);
    u3_noun it_pax = u3h(t_pax);
    u3_noun tt_pax = u3t(t_pax);

    if ( u3_nul == tt_pax ) {
      _unix_desk_sync_tofu(dir_u, u3k(i_pax), u3k(it_pax), mim);
    }
    else {
      u3_udir** dis_u = _unix_pdir(dir_u, u3k(i_pax));

      if ( !*dis_u ) {
        *dis_u = c3_malloc(sizeof(u3_udir));

        _unix_dir_forge(*dis_u, dir_u, u3k(i_pax));
      }
      _unix_desk_sync_tako(*dis_u, u3k(t_pax), mim);
    }
  }
  u3z(pax);
}

/* _unix_desk_sync_soba(): sync computed changes.
*/
static void
_unix_desk_sync_list(u3_udir* dir_u, u3_noun can)
{
  while ( u3_nul != can ) {
    _unix_desk_sync_tako(dir_u, u3k(u3h(u3h(can))), u3k(u3t(u3h(can))));
    can = u3t(can);
  }
  u3z(can);
}

/* _unix_desk_sync_ergo(): sync desk changes to unix.
*/
static void
_unix_desk_sync_ergo(u3_noun  hox,
                     u3_noun  syd,
                     u3_noun  lok,
                     u3_noun  can,
                     u3_uhot* hot_u)
{
  u3_udir** dir_u = _unix_pdir(&(hot_u->dir_u), syd);

#if 0
  uL(fprintf(uH, "ergo %s %s %s\n", u3r_string(hox),
                                    u3r_string(syd),
                                    u3r_string(lok)));
#endif

  if ( !*dir_u ) {
    *dir_u = c3_malloc(sizeof(u3_udir));

    _unix_dir_forge(*dir_u, &(hot_u->dir_u), u3k(syd));
  }

  _unix_desk_sync_list(*dir_u, can);
}

/* u3_unix_ef_init(): update filesystem for new acquisition.
*/
void
u3_unix_ef_init(u3_noun who)
{
  _unix_hot_gain(u3k(who), c3y);

  u3v_plan(u3nq(u3_blip, c3__sync, u3k(u3A->sen), u3_nul),
             u3nq(c3__into, who,
                            u3_blip,
                            u3nc(u3_nul, u3_nul)));
}

/* u3_unix_ef_ergo(): update filesystem, outbound.
*/
void
u3_unix_ef_ergo(u3_noun who,
                u3_noun syd,
                u3_noun rel,
                u3_noun can)
{
  u3_noun  hox = u3dc("scot", 'p', u3k(who));           // XXX unnecessary?
  u3_noun  lok = u3dc("scot", c3__ud, rel);             // XXX unnecessary?
  u3_uhot* hot_u;

  hot_u = _unix_home(who);

  if ( 0 != hot_u ) {
    _unix_desk_sync_ergo(hox, syd, lok, can, hot_u);
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
      c3_w    who_w[4];

      _unix_ship_out(who, who_w);
      for ( hot_u = unx_u->hot_u;
            hot_u && !_(_unix_ship_sing(who_w, hot_u->who_w));
            hot_u = hot_u->nex_u );

      if ( 0 == hot_u ) {
        _unix_hot_gain(u3k(who), c3n);
      }
    }
  }

  //  find directories without owners
  {
    u3_uhot** het_u = &(unx_u->hot_u);

    while ( 0 != (hot_u=*het_u) ) {
      for ( won = u3A->own; u3_nul != won; won = u3t(won) ) {
        u3_noun who = u3h(won);
        c3_w    who_w[4];

        _unix_ship_out(who, who_w);
        if ( _(_unix_ship_sing(who_w, hot_u->who_w)) ) {
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
  u3_lo_shut(c3y);
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
        u3_Host.liv = c3n;
        break;
      case SIGINT: 
        fprintf(stderr, "\r\ninterrupt\r\n");
        u3_term_ef_ctlc(); 
        break;
      case SIGWINCH: u3_term_ef_winc(); break;
      // case SIGCHLD: u3_save_ef_chld(); break;
    }
  }
  u3_lo_shut(c3y);
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
  u3_unix_acquire(u3_Host.dir_c);
  u3_unix_ef_move();
  uv_check_start(&u3_Host.unx_u.syn_u, _unix_ef_sync);
}

/* u3_unix_io_exit(): terminate unix I/O.
*/
void
u3_unix_io_exit(void)
{
  uv_check_stop(&u3_Host.unx_u.syn_u);
  u3_unix_release(u3_Host.dir_c);

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
