/* v/walk.c
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
#include <ev.h>
#include <curses.h>
#include <termios.h>
#include <term.h>
#include <errno.h>

#include "all.h"
#include "f/coal.h"
#include "v/vere.h"
  
  /*  |%
  **  ++  arch                                        ::  fs node
  **            $%  [& p=@uvI q=*]                    ::  file, hash/data
  **                [| p=(map ,@ta arch)]             ::  directory
  **            ==                                    ::
  **  --
  */

#if 0
static u2_noun
_walk_ok(u2_reck* rec_u, u2_noun nod)
{
  u2_noun don = u2_cn_mung(u2k(rec_u->toy.arch), u2k(nod));

  if ( u2_no == u2_sing(nod, don) ) {
    c3_assert(0);
  }
  u2z(don);
  return nod;
}
#endif

/* u2_walk_safe(): load file or 0.
*/
u2_noun
u2_walk_safe(c3_c* pas_c)
{
  struct stat buf_b;
  c3_i        fid_i = open(pas_c, O_RDONLY, 0644);
  c3_w        fln_w, red_w;
  c3_y*       pad_y;

  if ( (fid_i < 0) || (fstat(fid_i, &buf_b) < 0) ) {
    // uL(fprintf(uH, "%s: %s\n", pas_c, strerror(errno)));
    return 0;
  }
  fln_w = buf_b.st_size;
  pad_y = malloc(buf_b.st_size);

  red_w = read(fid_i, pad_y, fln_w);
  close(fid_i);

  if ( fln_w != red_w ) {
    free(pad_y);
    return 0;
  }
  else {
    u2_noun pad = u2_ci_bytes(fln_w, (c3_y *)pad_y); 
    free(pad_y);

    return pad;
  }
}

/* u2_walk_load(): load file or bail.
*/
u2_noun
u2_walk_load(c3_c* pas_c)
{
  struct stat buf_b;
  c3_i        fid_i = open(pas_c, O_RDONLY, 0644);
  c3_w        fln_w, red_w;
  c3_y*       pad_y;

  if ( (fid_i < 0) || (fstat(fid_i, &buf_b) < 0) ) {
    uL(fprintf(uH, "%s: %s\n", pas_c, strerror(errno)));
    return u2_cm_bail(c3__fail);
  }
  fln_w = buf_b.st_size;
  pad_y = malloc(buf_b.st_size);

  red_w = read(fid_i, pad_y, fln_w);
  close(fid_i);

  if ( fln_w != red_w ) {
    free(pad_y);
    return u2_cm_bail(c3__fail);
  }
  else {
    u2_noun pad = u2_ci_bytes(fln_w, (c3_y *)pad_y); 
    free(pad_y);

    return pad;
  }
}

/* u2_walk_save(): save file or bail.
*/
void
u2_walk_save(c3_c* pas_c, u2_noun tim, u2_atom pad)
{
  c3_i  fid_i = open(pas_c, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  c3_w  fln_w, rit_w;
  c3_y* pad_y;

  if ( fid_i < 0 ) {
    uL(fprintf(uH, "%s: %s\n", pas_c, strerror(errno)));
    u2_cm_bail(c3__fail);
  }

  fln_w = u2_met(3, pad);
  pad_y = malloc(fln_w);
  u2_cr_bytes(0, fln_w, pad_y, pad);
  u2z(pad);

  rit_w = write(fid_i, pad_y, fln_w);
  close(fid_i);
  free(pad_y);

  if ( rit_w != fln_w ) {
    uL(fprintf(uH, "%s: %s\n", pas_c, strerror(errno)));
    u2_cm_bail(c3__fail);
  }

  if ( 0 != tim ) {
    struct timeval tim_tv[2];

    u2_time_out_tv(&tim_tv[0], u2k(tim));
    u2_time_out_tv(&tim_tv[1], tim);
 
    utimes(pas_c, tim_tv);
  }
}

/* _walk_in(): inner loop of _walk(), producing map.
*/
static u2_noun
_walk_in(u2_reck* rec_u, const c3_c* dir_c, c3_w len_w)
{
  DIR*    dir_d = opendir(dir_c);
  u2_noun map = u2_nul;

  if ( !dir_d ) {
    return u2_nul;
  }
  else while ( 1 ) {
    struct dirent  ent_n;
    struct dirent* out_n;

    if ( readdir_r(dir_d, &ent_n, &out_n) != 0 ) {
      uL(fprintf(uH, "%s: %s\n", dir_c, strerror(errno)));
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
      c3_c*  pat_c = malloc(lef_w + 1);
      struct stat buf_b;
  
      strcpy(pat_c, dir_c);
      pat_c[len_w] = '/';
      strcpy(pat_c + len_w + 1, fil_c);

      if ( 0 != stat(pat_c, &buf_b) ) {
        free(pat_c);
      } else {
        u2_noun tim = c3_stat_mtime(&buf_b);

        if ( !S_ISDIR(buf_b.st_mode) ) {
          c3_c* dot_c = strrchr(fil_c, '.');
          c3_c* nam_c = strdup(fil_c);
          c3_c* ext_c = strdup(dot_c + 1);

          nam_c[dot_c - fil_c] = 0;
          {
            u2_noun nam = u2_ci_string(nam_c);
            u2_noun ext = u2_ci_string(ext_c);
            u2_noun get = u2_ckd_by_get(u2k(map), u2k(nam));
            u2_noun dat = u2_walk_load(pat_c);
            u2_noun hax;

            if ( !strcmp("noun", ext_c) ) {
              dat = u2_cke_cue(dat);
            }
            hax = u2_do("sham", u2k(dat));
            if ( u2_none == get ) { get = u2_nul; }
          
            get = u2_ckd_by_put(get, ext, u2nt(u2_yes, hax, dat));
            map = u2_ckd_by_put(map, nam, u2nc(u2_no, get));
          }
          free(nam_c);
          free(ext_c);
        }
        else {
          u2_noun dir = _walk_in(rec_u, pat_c, lef_w);

          if ( u2_nul != dir ) {
            map = u2_ckd_by_put
              (map, u2_ci_string(fil_c), u2nc(u2_no, dir));
          }
          else u2z(tim);
        }
        free(pat_c);
      }
    }
  }
  closedir(dir_d);
  return map;
}

/* u2_walk(): traverse `dir_c` to produce an arch, updating `old`.
*/
u2_noun
u2_walk(u2_reck* rec_u, const c3_c* dir_c, u2_noun old)
{
  //  XX - obviously, cheaper to update old data.
  u2z(old);
  {
    struct stat buf_b;

    if ( 0 != stat(dir_c, &buf_b) ) {
      uL(fprintf(uH, "can't stat %s\n", dir_c));
      // return u2_cm_bail(c3__fail);
      c3_assert(0);
    }
    else {
      return u2nc(u2_no, 
                  _walk_in(rec_u, dir_c, strlen(dir_c)));
    }
  }
}

/* u2_path(): C unix path in computer for file or directory. 
*/
c3_c*
u2_path(u2_bean fyl, u2_noun pax)
{
  c3_w len_w;
  c3_c *pas_c;

  //  measure
  //
  len_w = strlen(u2_Local);
  {
    u2_noun wiz = pax;

    while ( u2_nul != wiz ) {
      len_w += (1 + u2_cr_met(3, u2h(wiz)));
      wiz = u2t(wiz);
    }
  }

  //  cut
  //
  pas_c = malloc(len_w + 1);
  strcpy(pas_c, u2_Local);
  {
    u2_noun wiz   = pax;
    c3_c*   waq_c = (pas_c + strlen(pas_c));

    while ( u2_nul != wiz ) {
      c3_w tis_w = u2_cr_met(3, u2h(wiz));

      if ( (u2_yes == fyl) && (u2_nul == u2t(wiz)) ) {
        *waq_c++ = '.';
      } else *waq_c++ = '/';

      u2_cr_bytes(0, tis_w, (c3_y*)waq_c, u2h(wiz));
      waq_c += tis_w;

      wiz = u2t(wiz);
    }
    *waq_c = 0;
  }
  u2z(pax);
  return pas_c;
}
