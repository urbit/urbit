/* vere/walk.c
**
*/
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <dirent.h>
#include <uv.h>
#include <ncurses/curses.h>
#include <termios.h>
#include <ncurses/term.h>
#include <errno.h>

#include "all.h"
#include "vere/vere.h"

  /*  |%
  **  ++  arch                                        ::  fs node
  **            $%  [& p=@uvI q=*]                    ::  file, hash/data
  **                [| p=(map ,@ta arch)]             ::  directory
  **            ==                                    ::
  **  --
  */

#if 0
static u3_noun
_walk_ok(u3_noun nod)
{
  u3_noun don = u3n_mung(u3k(u2A->toy.arch), u3k(nod));

  if ( c3n == u3_sing(nod, don) ) {
    c3_assert(0);
  }
  u3z(don);
  return nod;
}
#endif

/* u3_walk_safe(): load file or 0.
*/
u3_noun
u3_walk_safe(c3_c* pas_c)
{
  struct stat buf_b;
  c3_i        fid_i = open(pas_c, O_RDONLY, 0644);
  c3_w        fln_w, red_w;
  c3_y*       pad_y;

  if ( (fid_i < 0) || (fstat(fid_i, &buf_b) < 0) ) {
    // u3l_log("%s: %s\n", pas_c, strerror(errno));
    return 0;
  }
  fln_w = buf_b.st_size;
  pad_y = c3_malloc(buf_b.st_size);

  red_w = read(fid_i, pad_y, fln_w);
  close(fid_i);

  if ( fln_w != red_w ) {
    free(pad_y);
    return 0;
  }
  else {
    u3_noun pad = u3i_bytes(fln_w, (c3_y *)pad_y);
    free(pad_y);

    return pad;
  }
}

/* u3_walk_load(): load file or bail.
*/
u3_noun
u3_walk_load(c3_c* pas_c)
{
  struct stat buf_b;
  c3_i        fid_i = open(pas_c, O_RDONLY, 0644);
  c3_w        fln_w, red_w;
  c3_y*       pad_y;

  if ( (fid_i < 0) || (fstat(fid_i, &buf_b) < 0) ) {
    u3l_log("%s: %s\n", pas_c, strerror(errno));
    return u3m_bail(c3__fail);
  }
  fln_w = buf_b.st_size;
  pad_y = c3_malloc(buf_b.st_size);

  red_w = read(fid_i, pad_y, fln_w);
  close(fid_i);

  if ( fln_w != red_w ) {
    free(pad_y);
    return u3m_bail(c3__fail);
  }
  else {
    u3_noun pad = u3i_bytes(fln_w, (c3_y *)pad_y);
    free(pad_y);

    return pad;
  }
}

/* _walk_mkdirp(): recursively make directories in pax at bas_c (RETAIN)
*/
static void
_walk_mkdirp(c3_c* bas_c, u3_noun pax)
{
  c3_c* pax_c;
  c3_y* waq_y;
  c3_w  pax_w, fas_w, len_w;

  if ( u3_nul == pax ) {
    return;
  }

  pax_w = u3r_met(3, u3h(pax));
  fas_w = strlen(bas_c);
  len_w = 1 + fas_w + pax_w;

  pax_c = c3_malloc(1 + len_w);
  strncpy(pax_c, bas_c, len_w);
  pax_c[fas_w] = '/';
  waq_y = (void*)(1 + pax_c + fas_w);
  u3r_bytes(0, pax_w, waq_y, u3h(pax));
  pax_c[len_w] = '\0';

  if ( 0 != mkdir(pax_c, 0755) && EEXIST != errno ) {
    u3l_log("error mkdiring %s: %s\n", pax_c, strerror(errno));
    u3m_bail(c3__fail);
  }

  _walk_mkdirp(pax_c, u3t(pax));
  free(pax_c);
}

/* u3_walk_save(): save file or bail.
*/
void
u3_walk_save(c3_c* pas_c, u3_noun tim, u3_atom pad, c3_c* bas_c, u3_noun pax)
{
  c3_i  fid_i = open(pas_c, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  c3_w  fln_w, rit_w;
  c3_y* pad_y;

  if ( fid_i < 0 ) {
    if ( ENOENT == errno && u3_nul != pax ) {
      _walk_mkdirp(bas_c, pax);
      return u3_walk_save(pas_c, tim, pad, 0, u3_nul);
    }

    u3l_log("%s: %s\n", pas_c, strerror(errno));
    u3m_bail(c3__fail);
  }

  fln_w = u3r_met(3, pad);
  pad_y = c3_malloc(fln_w);
  u3r_bytes(0, fln_w, pad_y, pad);
  u3z(pad);
  u3z(pax);

  rit_w = write(fid_i, pad_y, fln_w);
  close(fid_i);
  free(pad_y);

  if ( rit_w != fln_w ) {
    u3l_log("%s: %s\n", pas_c, strerror(errno));
    u3m_bail(c3__fail);
  }

  if ( 0 != tim ) {
    struct timeval tim_tv[2];

    u3_time_out_tv(&tim_tv[0], u3k(tim));
    u3_time_out_tv(&tim_tv[1], tim);

    utimes(pas_c, tim_tv);
  }
}

/* _walk_in(): inner loop of _walk(), producing map.
*/
static u3_noun
_walk_in(const c3_c* dir_c, c3_w len_w)
{
  DIR*    dir_d = opendir(dir_c);
  u3_noun map = u3_nul;

  if ( !dir_d ) {
    return u3_nul;
  }
  else while ( 1 ) {
    struct dirent  ent_n;
    struct dirent* out_n;

    if ( u3_readdir_r(dir_d, &ent_n, &out_n) != 0 ) {
       u3l_log("%s: %s\n", dir_c, strerror(errno));
      break;
    }
    else if ( !out_n ) {
      break;
    }
    else if ( !strcmp(out_n->d_name, ".") ||
              !strcmp(out_n->d_name, "..") ||
              ('~' == out_n->d_name[0]) ||
              ('.' == out_n->d_name[0]) )     //  XX restricts some spans
    {
      continue;
    }
    else {
      c3_c*  fil_c = out_n->d_name;
      c3_w   lef_w = len_w + 1 + strlen(fil_c);
      c3_c*  pat_c = c3_malloc(lef_w + 1);
      struct stat buf_b;

      strncpy(pat_c, dir_c, lef_w);
      pat_c[len_w] = '/';
      strncpy(pat_c + len_w + 1, fil_c, lef_w);
      pat_c[lef_w] = '\0';

      if ( 0 != stat(pat_c, &buf_b) ) {
        free(pat_c);
      } else {
        u3_noun tim = c3_stat_mtime(&buf_b);

        if ( !S_ISDIR(buf_b.st_mode) ) {
          c3_c* dot_c = strrchr(fil_c, '.');
          c3_c* nam_c = strdup(fil_c);
          c3_c* ext_c = strdup(dot_c + 1);

          nam_c[dot_c - fil_c] = 0;
          {
            u3_noun nam = u3i_string(nam_c);
            u3_noun ext = u3i_string(ext_c);
            u3_noun get = u3kdb_get(u3k(map), u3k(nam));
            u3_noun dat = u3_walk_load(pat_c);
            u3_noun hax;

            if ( !strcmp("noun", ext_c) ) {
              dat = u3ke_cue(dat);
            }
            hax = u3do("sham", u3k(dat));
            if ( u3_none == get ) { get = u3_nul; }

            get = u3kdb_put(get, ext, u3nt(c3y, hax, dat));
            map = u3kdb_put(map, nam, u3nc(c3n, get));
          }
          free(nam_c);
          free(ext_c);
        }
        else {
          u3_noun dir = _walk_in(pat_c, lef_w);

          if ( u3_nul != dir ) {
            map = u3kdb_put
              (map, u3i_string(fil_c), u3nc(c3n, dir));
          }
          else u3z(tim);
        }
        free(pat_c);
      }
    }
  }
  closedir(dir_d);
  return map;
}

/* u3_walk(): traverse `dir_c` to produce an arch, updating `old`.
*/
u3_noun
u3_walk(const c3_c* dir_c, u3_noun old)
{
  //  XX - obviously, cheaper to update old data.
  u3z(old);
  {
    struct stat buf_b;

    if ( 0 != stat(dir_c, &buf_b) ) {
      u3l_log("can't stat %s\n", dir_c);
      // return u3m_bail(c3__fail);
      c3_assert(0);
    }
    else {
      return u3nc(c3n,
                  _walk_in(dir_c, strlen(dir_c)));
    }
  }
}

/* u3_path(): C unix path in computer for file or directory.
*/
c3_c*
u3_path(c3_o fyl, u3_noun pax)
{
  c3_w len_w;
  c3_c *pas_c;

  //  measure
  //
  len_w = strlen(u3_Local);
  {
    u3_noun wiz = pax;

    while ( u3_nul != wiz ) {
      len_w += (1 + u3r_met(3, u3h(wiz)));
      wiz = u3t(wiz);
    }
  }

  //  cut
  //
  pas_c = c3_malloc(len_w + 1);
  strncpy(pas_c, u3_Local, len_w);
  pas_c[len_w] = '\0';
  {
    u3_noun wiz   = pax;
    c3_c*   waq_c = (pas_c + strlen(pas_c));

    while ( u3_nul != wiz ) {
      c3_w tis_w = u3r_met(3, u3h(wiz));

      if ( (c3y == fyl) && (u3_nul == u3t(wiz)) ) {
        *waq_c++ = '.';
      } else *waq_c++ = '/';

      u3r_bytes(0, tis_w, (c3_y*)waq_c, u3h(wiz));
      waq_c += tis_w;

      wiz = u3t(wiz);
    }
    *waq_c = 0;
  }
  u3z(pax);
  return pas_c;
}
