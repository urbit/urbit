// XXX maybe make sure empty directories disappear on update?
// XXX i suspect maybe a problem if there's untrackable files in
//     a directory when we try to delete it?
// XXX maybe it's not a bad idea to have clay handle the placing of the dots?
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
#include <libgen.h>

#include "all.h"
#include "v/vere.h"

/* undef this to turn off syncing out to unix */
#define ERGO_SYNC

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

/* _unix_string_to_path(): convert c string to u3_noun path
 *
 * c string must begin with the pier path plus mountpoint
*/
static u3_noun
_unix_string_to_path_helper(c3_c* pax_c) {

  c3_assert(pax_c[-1] == '/');
  c3_c* end_w = strchr(pax_c, '/');
  if ( !end_w ) {
    end_w = strrchr(pax_c, '.');
    if ( !end_w ) {
      return u3nc(u3i_string(pax_c), u3_nul);
    }
    else {
      return u3nt(u3i_bytes(end_w - pax_c, (c3_y*) pax_c),
                  u3i_string(end_w + 1),
                  u3_nul);
    }
  }
  else {
    return u3nc(u3i_bytes(end_w - pax_c, (c3_y*) pax_c),
                _unix_string_to_path_helper(end_w + 1));
  }
}
static u3_noun
_unix_string_to_path(c3_c* pax_c) {
  pax_c += strlen(u3_Host.dir_c) + 1;
  pax_c = strchr(pax_c,'/');
  if ( !pax_c ) {
    return u3_nul;
  }
  else {
    return _unix_string_to_path_helper(pax_c + 1);
  }
}

/* _unix_mkdir(): mkdir, asserting.
*/
static void
_unix_mkdir(c3_c* pax_c)
{
  if ( 0 != mkdir(pax_c, 0755) && EEXIST != errno) {
    uL(fprintf(uH, "error mkdiring %s: %s\n", pax_c, strerror(errno)));
    c3_assert(0);
  }
}

/* unix_write_file(): write to a file
*/
static void
_unix_write_file(c3_c* pax_c, u3_atom mim)
{
  c3_i  fid_i = open(pax_c, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  c3_w  len_w, rit_w, siz_w;
  c3_y* dat_y;

  if ( fid_i < 0 ) {
    uL(fprintf(uH, "error opening %s for writing: %s\r\n",
               pax_c, strerror(errno)));
  }

  siz_w = u3h(u3t(mim));
  len_w = u3r_met(3, u3t(u3t(mim)));
  dat_y = c3_malloc(siz_w);
  memset(dat_y, 0, siz_w);

  u3r_bytes(0, len_w, dat_y, u3t(u3t(mim)));
  u3z(mim);

  rit_w = write(fid_i, dat_y, siz_w);

  if ( rit_w != siz_w ) {
    uL(fprintf(uH, "error writing %s: %s\r\n",
               pax_c, strerror(errno)));
  }

  close(fid_i);
  free(dat_y);
}

/* _unix_get_mount_point(): retrieve or create mount point
*/
static u3_umon*
_unix_get_mount_point(u3_noun mon)
{
  if ( c3n == u3ud(mon) ) {
    c3_assert(!"mount point must be an atom");
    u3z(mon);
    return NULL;
  }

  c3_c* nam_c = u3r_string(mon);
  u3_umon* mon_u;

  for ( mon_u = u3_Host.unx_u.mon_u;
        mon_u && 0 != strcmp(nam_c, mon_u->nam_c);
        mon_u = mon_u->nex_u )
  {
  }

  if ( !mon_u ) {
    mon_u = malloc(sizeof(u3_umon));
    mon_u->nam_c = nam_c;
    mon_u->dir_u.dir = c3y;
    mon_u->dir_u.dry = c3n;
    mon_u->dir_u.pax_c = _unix_down(u3_Host.dir_c, nam_c);
    mon_u->dir_u.par_u = NULL;
    mon_u->dir_u.nex_u = NULL;
    mon_u->dir_u.kid_u = NULL;
    mon_u->nex_u = u3_Host.unx_u.mon_u;
    u3_Host.unx_u.mon_u = mon_u;
  }

  free(nam_c);
  u3z(mon);

  return mon_u;
}

static void _unix_free_node(u3_unod* nod_u);

/* _unix_free_file(): free file, unlinking it
*/
static void
_unix_free_file(uv_handle_t* was_u)
{
  u3_ufil* fil_u = (void*) was_u;

  if ( 0 != unlink(fil_u->pax_c) && ENOENT != errno ) {
    uL(fprintf(uH, "error unlinking %s: %s\n", fil_u->pax_c, strerror(errno)));
    c3_assert(0);
  }

  free(fil_u->pax_c);
  free(fil_u);
}

/* _unix_free_dir(): free directory, deleting everything within
*/
static void
_unix_free_dir(uv_handle_t* was_u)
{
  u3_udir* dir_u = (void*) was_u;

  u3_unod* nod_u = dir_u->kid_u;
  while ( nod_u ) {
    u3_unod* nex_u = nod_u->nex_u;
    _unix_free_node(nod_u);
    nod_u = nex_u;
  }

  if ( 0 != rmdir(dir_u->pax_c) && ENOENT != errno ) {
    uL(fprintf(uH, "error rmdiring %s: %s\n", dir_u->pax_c, strerror(errno)));
    c3_assert(0);
  }

  free(dir_u->pax_c);
  free(dir_u);
}

/* _unix_free_node(): free node, deleting everything within
 *
 * also deletes from parent list if in it
*/
static void
_unix_free_node(u3_unod* nod_u)
{
  if ( nod_u->par_u ) {
    u3_unod* don_u = nod_u->par_u->kid_u;
  
    if ( !don_u ) {
    }
    else if ( nod_u == don_u ) {
      nod_u->par_u->kid_u = nod_u->par_u->kid_u->nex_u;
    }
    else {
      for ( ; don_u->nex_u && nod_u != don_u->nex_u; don_u = don_u->nex_u ) {
      }
      if ( don_u->nex_u ) {
        don_u->nex_u = don_u->nex_u->nex_u;
      }
    }
  }

  if ( c3y == nod_u->dir ) {
    uv_close((uv_handle_t*)&nod_u->was_u, _unix_free_dir);
  }
  else {
    uv_close((uv_handle_t*)&nod_u->was_u, _unix_free_file);
  }
}

/* _unix_free_mount_point(): free mount point
*/
static void
_unix_free_mount_point(u3_umon* mon_u)
{
  u3_unod* nod_u;
  for ( nod_u = mon_u->dir_u.kid_u; nod_u; ) {
    u3_unod* nex_u = nod_u->nex_u;
    _unix_free_node(nod_u);
    nod_u = nex_u;
  }

  free(mon_u->dir_u.pax_c);
  free(mon_u->nam_c);
  free(mon_u);
}

/* _unix_delete_mount_point(): remove mount point from list and free
*/
static void
_unix_delete_mount_point(u3_noun mon)
{
  if ( c3n == u3ud(mon) ) {
    c3_assert(!"mount point must be an atom");
    u3z(mon);
    return;
  }

  c3_c* nam_c = u3r_string(mon);
  u3_umon* mon_u;
  u3_umon* tem_u;

  mon_u = u3_Host.unx_u.mon_u;
  if ( !mon_u ) {
    uL(fprintf(uH, "mount point already gone: %s\r\n", nam_c));
    goto _delete_mount_point_out;
  }
  if ( 0 == strcmp(nam_c, mon_u->nam_c) ) {
    u3_Host.unx_u.mon_u = mon_u->nex_u;
    _unix_free_mount_point(mon_u);
    goto _delete_mount_point_out;
  }

  for ( ;
        mon_u->nex_u && 0 != strcmp(nam_c, mon_u->nex_u->nam_c);
        mon_u = mon_u->nex_u )
  {
  }

  if ( !mon_u->nex_u ) {
    uL(fprintf(uH, "mount point already gone: %s\r\n", nam_c));
    goto _delete_mount_point_out;
  }

  tem_u = mon_u->nex_u;
  mon_u->nex_u = mon_u->nex_u->nex_u;
  _unix_free_mount_point(tem_u);

_delete_mount_point_out:
  free(nam_c);
  u3z(mon);
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

  while ( nod_u ) {
    nod_u->dry = c3n;
    nod_u = (u3_unod*) nod_u->par_u;
  }
}

/* _unix_watch_file(): initialize file
*/
static void
_unix_watch_file(u3_ufil* fil_u, u3_udir* par_u, c3_c* pax_c)
{
  // initialize fil_u

  fil_u->dir = c3n;
  fil_u->dry = c3n;
  fil_u->pax_c = pax_c;
  fil_u->par_u = par_u;
  fil_u->nex_u = NULL;
  fil_u->mug_w = 0;
  fil_u->gum_w = 0;

  if ( par_u ) {
    fil_u->nex_u = par_u->kid_u;
    par_u->kid_u = (u3_unod*) fil_u;
  }

  // stuff fil_u into libuv
  // note that we're doing something tricky here
  // see comment in _unix_fs_event_cb

  c3_w ret_w = uv_fs_event_init(u3L, &fil_u->was_u);
  if (0 != ret_w){
    uL(fprintf(uH, "event init: %s\n", uv_strerror(ret_w)));
    c3_assert(0);
  }

  ret_w = uv_fs_event_start(&fil_u->was_u, _unix_fs_event_cb, pax_c, 0);
  if ( 0 != ret_w ){
    uL(fprintf(uH, "event start: %s\n", uv_strerror(ret_w)));
    c3_assert(0);
  }
}

/* _unix_watch_dir(): initialize directory
*/
static void
_unix_watch_dir(u3_udir* dir_u, u3_udir* par_u, c3_c* pax_c)
{
  // initialize dir_u

  dir_u->dir = c3y;
  dir_u->dry = c3n;
  dir_u->pax_c = pax_c;
  dir_u->par_u = par_u;
  dir_u->nex_u = NULL;
  dir_u->kid_u = NULL;

  if ( par_u ) {
    dir_u->nex_u = par_u->kid_u;
    par_u->kid_u = (u3_unod*) dir_u;
  }

  // stuff dir_u into libuv
  // note that we're doing something tricky here
  // see comment in _unix_fs_event_cb

  c3_w ret_w = uv_fs_event_init(u3L, &dir_u->was_u);
  if (0 != ret_w){
    uL(fprintf(uH, "event init: %s\n", uv_strerror(ret_w)));
    c3_assert(0);
  }

  ret_w = uv_fs_event_start(&dir_u->was_u, _unix_fs_event_cb, pax_c, 0);
  if (0 != ret_w){
    uL(fprintf(uH, "event start: %s\n", uv_strerror(ret_w)));
    c3_assert(0);
  }
}

/* _unix_create_dir(): create unix directory and watch it
*/
static void
_unix_create_dir(u3_udir* dir_u, u3_udir* par_u, u3_noun nam)
{
  c3_c* nam_c = u3r_string(nam);
  c3_w  nam_w = strlen(nam_c);
  c3_w  pax_w = strlen(par_u->pax_c);
  c3_c* pax_c = c3_malloc(pax_w + 1 + nam_w + 1);

  strncpy(pax_c, par_u->pax_c, pax_w);
  pax_c[pax_w] = '/';
  strncpy(pax_c + pax_w + 1, nam_c, nam_w);
  pax_c[pax_w + nam_w + 1] = '\0';

  free(nam_c);
  u3z(nam);

  _unix_mkdir(pax_c);
  _unix_watch_dir(dir_u, par_u, pax_c);
}

static u3_noun _unix_update_node(u3_unod* nod_u);

/* _unix_update_file(): update file, producing list of changes
 *
 * when scanning through files, if dry, do nothing.  otherwise, mark as
 * dry, then check if file exists.  if not, remove self from node list
 * and add path plus sig to %into event.  otherwise, read the file and
 * get a mug checksum.  if same as mug_w, move on.  otherwise, overwrite
 * mug_w with new mug and add path plus data to %into event.
*/
static u3_noun
_unix_update_file(u3_ufil* fil_u)
{
  c3_assert( c3n == fil_u->dir );

  if ( c3y == fil_u->dry ) {
    return u3_nul;
  }

  fil_u->dry = c3y;

  struct stat buf_u;
  c3_i  fid_i = open(fil_u->pax_c, O_RDONLY, 0644);
  c3_w  len_w, red_w;
  c3_y* dat_y;

  if ( (fid_i < 0) || (fstat(fid_i, &buf_u) < 0) ) {
    if ( ENOENT == errno ) {
      return u3nc(u3nc(_unix_string_to_path(fil_u->pax_c), u3_nul), u3_nul);
    }
    else {
      uL(fprintf(uH, "error opening file %s: %s\r\n",
                 fil_u->pax_c, strerror(errno)));
      return u3_nul;
    }
  }

  len_w = buf_u.st_size;
  dat_y = c3_malloc(len_w);

  red_w = read(fid_i, dat_y, len_w);

  if ( close(fid_i) < 0 ) {
    uL(fprintf(uH, "error closing file %s: %s\r\n",
               fil_u->pax_c, strerror(errno)));
  }

  if ( len_w != red_w ) {
    free(dat_y);
    c3_assert(0);
    return u3_nul;
  }
  else {
    c3_w mug_w = u3r_mug_bytes(dat_y, len_w);
    if ( mug_w == fil_u->mug_w ) {
      fil_u->mug_w = mug_w;

      free(dat_y);
      return u3_nul;
    }
    else {
      fil_u->mug_w = mug_w;

      u3_noun pax = _unix_string_to_path(fil_u->pax_c);
      u3_noun mim = u3nt(c3__text, u3i_string("plain"), u3_nul);
      u3_noun dat = u3nt(mim, len_w, u3i_bytes(len_w, dat_y));

      free(dat_y);
      return u3nc(u3nt(pax, u3_nul, dat), u3_nul);
    }
  }
}

/* _unix_update_dir(): update directory, producing list of changes
*/
static u3_noun
_unix_update_dir(u3_udir* dir_u)
{
  c3_assert( c3y == dir_u->dir );

  if ( c3y == dir_u->dry ) {
    return u3_nul;
  }

  u3_unod* nod_u;

  for ( nod_u = dir_u->kid_u; nod_u->nex_u; ) {
    if ( c3y == nod_u->dry ) {
      nod_u = nod_u->nex_u;
    }
    else {
      if ( c3y == nod_u->dir ) {
        DIR* red_u = opendir(nod_u->pax_c);

        if ( 0 == red_u ) {
          u3_unod* nex_u = nod_u->nex_u;
          _unix_free_node(nod_u);
          nod_u = nex_u;
        }
        else {
          closedir(red_u);
          nod_u = nod_u->nex_u;
        }
      }
      else {
        struct stat buf_u;
        c3_i  fid_i = open(nod_u->pax_c, O_RDONLY, 0644);

        if ( (fid_i < 0) || (fstat(fid_i, &buf_u) < 0) ) {
          if ( ENOENT != errno ) {
            uL(fprintf(uH, "_unix_update_dir: error opening file %s: %s\r\n",
                       nod_u->pax_c, strerror(errno)));
          }

          u3_unod* nex_u = nod_u->nex_u;
          _unix_free_node(nod_u);
          nod_u = nex_u;
        }
        else {
          if ( close(fid_i) < 0 ) {
            uL(fprintf(uH, "_unix_update_dir: error closing file %s: %s\r\n",
                       nod_u->pax_c, strerror(errno)));
          }

          nod_u = nod_u->nex_u;
        }
      }
    }
  }

  // Check for new files

  DIR* rid_u = opendir(dir_u->pax_c);
  if ( !rid_u ) {
    uL(fprintf(uH, "error opening directory %s: %s\r\n",
               dir_u->pax_c, strerror(errno)));
  }

  while ( 1 ) {
    struct dirent  ent_u;
    struct dirent* out_u;
    c3_w err_w;
  
    if ( (err_w = readdir_r(rid_u, &ent_u, &out_u)) != 0 ) {
      uL(fprintf(uH, "error loading directory %s: %s\n",
                 dir_u->pax_c, strerror(err_w)));
      c3_assert(0);
    }
    else if ( !out_u ) {
      break;
    }
    else if ( '.' == out_u->d_name[0] ) {
      continue;
    }
    else {
      c3_c* pax_c = _unix_down(dir_u->pax_c, out_u->d_name);

      struct stat buf_u;

      if ( 0 != stat(pax_c, &buf_u) ) {
        free(pax_c);
        continue;
      }
      else {
        u3_unod* nod_u;
        for ( nod_u = dir_u->kid_u; nod_u; nod_u = nod_u->nex_u ) {
          if ( !strcmp(pax_c, nod_u->pax_c) ) {
            if ( S_ISDIR(buf_u.st_mode) ) {
              c3_assert(nod_u->dir);
            }
            else {
              c3_assert(!nod_u->dir);
            }
            break;
          }
        }

        if ( !nod_u ) {
          if ( !S_ISDIR(buf_u.st_mode) ) {
            if ( !strchr(out_u->d_name,'.')
                 || '~' == out_u->d_name[strlen(out_u->d_name) - 1]
               ) {
              free(pax_c);
              continue;
            }

            u3_ufil* fil_u = c3_malloc(sizeof(u3_ufil));
            _unix_watch_file(fil_u, dir_u, pax_c);
          }
          else {
            u3_udir* dis_u = c3_malloc(sizeof(u3_udir));
            _unix_watch_dir(dis_u, dir_u, pax_c);
            _unix_update_dir(dis_u);
          }
        }
      }
    }
  }

  if ( closedir(rid_u) < 0 ) {
    uL(fprintf(uH, "error closing directory %s: %s\r\n",
               dir_u->pax_c, strerror(errno)));
  }

  // get change list

  u3_noun can = u3_nul;
  for ( nod_u = dir_u->kid_u; nod_u; nod_u = nod_u->nex_u ) {
    can = u3kb_weld(_unix_update_node(nod_u),can);
  }

  return can;
}

/* _unix_update_node(): update node, producing list of changes
*/
static u3_noun
_unix_update_node(u3_unod* nod_u)
{
  if ( c3y == nod_u->dir ) {
    return _unix_update_dir((void*)nod_u);
  }
  else {
    return _unix_update_file((void*)nod_u);
  }
}

/* _unix_update_mount(): update mount point
*/
static void
_unix_update_mount(u3_umon* mon_u)
{
  if ( c3n == mon_u->dir_u.dry ) {
    u3_noun can = _unix_update_dir(&mon_u->dir_u);
    u3v_plan(u3nq(u3_blip, c3__sync, u3k(u3A->sen), u3_nul),
             u3nt(c3__into, u3i_string(mon_u->nam_c), can));
  }
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
    }
  }
  u3_lo_shut(c3y);
}

/* _unix_ef_sync(): check for files to sync.
 */
static void
_unix_ef_sync(uv_check_t* han_u)
{
  u3_lo_open();
  u3_lo_shut(c3y);
}

/* _unix_sync_file(): sync file to unix
*/
static void
_unix_sync_file(u3_udir* par_u, u3_noun nam, u3_noun ext, u3_noun mim)
{
  // form file path

  c3_c* nam_c = u3r_string(nam);
  c3_c* ext_c = u3r_string(ext);
  c3_w  par_w = strlen(par_u->pax_c);
  c3_w  nam_w = strlen(nam_c);
  c3_w  ext_w = strlen(ext_c);
  c3_c* pax_c = c3_malloc(par_w + 1 + nam_w + 1 + ext_w + 1);

  strncpy(pax_c, par_u->pax_c, par_w);
  pax_c[par_w] = '/';
  strncpy(pax_c + par_w + 1, nam_c, nam_w);
  pax_c[par_w + 1 + nam_w] = '.';
  strncpy(pax_c + par_w + 1 + nam_w + 1, ext_c, ext_w);
  pax_c[par_w + 1 + nam_w + 1 + ext_w] = '\0';


  free(nam_c); free(ext_c);
  u3z(nam); u3z(ext);

  // check whether we already know about this file

  u3_unod* nod_u;
  for ( nod_u = par_u->kid_u;
        ( nod_u &&
          ( c3n == nod_u->dir ||
            0 != strcmp(nod_u->pax_c, pax_c) ) );
        nod_u = nod_u->nex_u )
  { }

  // apply change

  if ( u3_nul == mim ) {
    if ( nod_u ) {
      _unix_free_node(nod_u);
    }
  }
  else {
    _unix_write_file(pax_c, u3k(u3t(mim)));

    if ( !nod_u ) {
      u3_ufil* fil_u = c3_malloc(sizeof(u3_ufil));
      _unix_watch_file(fil_u, par_u, pax_c);
    }
  }

  free(pax_c);
  u3z(mim);
}

/* _unix_sync_change(): sync single change to unix
*/
static void
_unix_sync_change(u3_udir* dir_u, u3_noun pax, u3_noun mim)
{
  if ( c3n == u3du(pax) ) {
    uL(fprintf(uH,"can't sync out file as top-level\r\n"));
    u3z(pax); u3z(mim);
    return;
  }
  else if ( c3n == u3du(u3t(pax)) ) {
    u3_noun i_pax = u3h(pax);
    u3_noun t_pax = u3t(pax);
    c3_assert( u3_nul == t_pax );
    c3_c* nam_c = strrchr(dir_u->pax_c, '/') + 1;
    u3_noun nam = u3i_string(nam_c);

    _unix_sync_file(dir_u->par_u, nam, u3k(i_pax), mim);
  }
  else {
    u3_noun i_pax = u3h(pax);
    u3_noun t_pax = u3t(pax);
    u3_noun it_pax = u3h(t_pax);
    u3_noun tt_pax = u3t(t_pax);

    if ( u3_nul == tt_pax ) {
      _unix_sync_file(dir_u, u3k(i_pax), u3k(it_pax), mim);
    }
    else {
      c3_c* nam_c = u3r_string(i_pax);
      c3_w pax_w = strlen(dir_u->pax_c);
      u3_unod* nod_u;

      
      for ( nod_u = dir_u->kid_u;
            ( nod_u &&
              ( c3n == nod_u->dir ||
                0 != strcmp(nod_u->pax_c + pax_w + 1, nam_c) ) );
            nod_u = nod_u->nex_u )
      { }

      if ( nod_u ) {
        _unix_sync_change((u3_udir*) nod_u, u3k(t_pax), mim);
      }
      else {
        u3_udir* dis_u = c3_malloc(sizeof(u3_udir));
        _unix_create_dir(dis_u, dir_u, u3k(i_pax));
      }
    }
  }
  u3z(pax);
}

/* _unix_sync_ergo(): sync list of changes to unix
*/
static void
_unix_sync_ergo(u3_umon* mon_u, u3_noun can)
{
  u3_noun nac = can;

  while ( u3_nul != nac) {
    _unix_sync_change(&mon_u->dir_u, u3k(u3h(u3h(nac))), u3k(u3t(u3h(nac))));
    nac = u3t(nac);
  }

  u3z(can);
}

/* u3_unix_ef_ergo(): update filesystem from urbit
*/
void
u3_unix_ef_ergo(u3_noun mon, u3_noun can)
{
  u3_umon* mon_u = _unix_get_mount_point(mon);

  _unix_sync_ergo(mon_u, can);
}

/* u3_unix_ef_ogre(): delete mount point
*/
void
u3_unix_ef_ogre(u3_noun mon)
{
  _unix_delete_mount_point(mon);
}

/* u3_unix_io_init(): initialize unix sync.
*/
void
u3_unix_io_init(void)
{
  u3_unix* unx_u = &u3_Host.unx_u;

  unx_u->mon_u = NULL;

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

  uv_check_init(u3_Host.lup_u, &u3_Host.unx_u.syn_u);
}

/* u3_unix_acquire(): acquire a lockfile, killing anything that holds it.
*/
static void
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
static void
u3_unix_release(c3_c* pax_c)
{
  c3_c* paf_c = _unix_down(pax_c, ".vere.lock");

  unlink(paf_c);
  free(paf_c);
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

/* u3_unix_ef_look(): update the root.
*/
void
u3_unix_ef_look(void)
{
  u3_umon* mon_u;

  for ( mon_u = u3_Host.unx_u.mon_u; mon_u; mon_u = mon_u->nex_u ) {
    _unix_update_mount(mon_u);
  }
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
}

/* u3_unix_io_poll(): update unix IO state.
*/
void
u3_unix_io_poll(void)
{
}
