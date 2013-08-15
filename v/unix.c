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
#include <ev.h>
#include <curses.h>
#include <termios.h>
#include <term.h>
#include <errno.h>

#include "all.h"
#include "f/coal.h"
#include "v/vere.h"

#define u2R  ((u2_reck *) &u2_Host.arv_u)     //  new school

static void _lo_unix(struct ev_loop *lup_u, struct ev_timer* tim_u, c3_i rev_i)
  { u2_lo_call(u2_Host.arv_u, lup_u, tim_u, c3__unix, rev_i); }

static void _lo_sign(struct ev_loop *lup_u, struct ev_signal* sil_u, c3_i rev_i)
  { u2_lo_call(u2_Host.arv_u, lup_u, sil_u, c3__unix, rev_i); }

static void _lo_stat(struct ev_loop *lup_u, struct ev_stat* sat_u, c3_i rev_i)
  { u2_lo_call(u2_Host.arv_u, lup_u, sat_u, c3__unix, rev_i); }

/* _unix_down(): descend path.
*/
static c3_c*
_unix_down(c3_c* pax_c, c3_c* sub_c)
{
  c3_w pax_w = strlen(pax_c);
  c3_c* don_c = malloc(pax_w + strlen(sub_c) + 2);

  strcpy(don_c, pax_c);
  don_c[pax_w] = '/';
  strcpy(don_c + pax_w + 1, sub_c);

  return don_c;
}

/* _unix_opendir(): opendir, asserting.
*/
static DIR*
_unix_opendir(c3_c* pax_c)
{
  DIR* rid_u = opendir(pax_c);

  if ( !rid_u ) {
    uL(fprintf(uH, "%s: %s\n", pax_c, strerror(errno)));
    c3_assert(0);
  }
  return rid_u;
}

/* _unix_mkdir(): mkdir, asserting.
*/
static void
_unix_mkdir(c3_c* pax_c)
{
  if ( 0 != mkdir(pax_c, 0755) ) {
    uL(fprintf(uH, "%s: %s\n", pax_c, strerror(errno)));
    c3_assert(0);
  }
}

/* _unix_unlink(): unlink, asserting.
*/
static void
_unix_unlink(c3_c* pax_c)
{
  if ( 0 != unlink(pax_c) ) {
    uL(fprintf(uH, "%s: %s\n", pax_c, strerror(errno)));
    c3_assert(0);
  }
}

/* u2_unix_acquire(): acquire a lockfile, killing anything that holds it.
*/
void
u2_unix_acquire(c3_c* pax_c)
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
      }
      uL(fprintf(uH, "unix: stopped old process %u\n", pid_w));
    }
    fclose(loq_u);
    unlink(paf_c);
  }

  loq_u = fopen(paf_c, "w");
  fprintf(loq_u, "%u\n", getpid());

  {
    c3_i fid_i = fileno(loq_u);
#if defined(U2_OS_linux)
    fdatasync(fid_i);
#else
    fcntl(fid_i, F_FULLFSYNC); 
#endif
  }
  fclose(loq_u);
}

/* u2_unix_release(): release a lockfile.
*/
void
u2_unix_release(c3_c* pax_c)
{
  c3_c* paf_c = _unix_down(pax_c, ".vere.lock");

  unlink(paf_c);
  free(paf_c);
}

/* _unix_file_watch(): create file tracker (from filesystem)
*/
static void
_unix_file_watch(u2_reck* rec_u, 
                 u2_ufil* fil_u, 
                 u2_udir* dir_u, 
                 c3_c*    pax_c,
                 mpz_t    mod_mp)
{
  ev_stat_init(&fil_u->was_u, _lo_stat, pax_c, 1.0);

  fil_u->non = u2_no;
  fil_u->dry = u2_no;
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
}

/* _unix_file_form(): form a filename path downward.
*/
static c3_c*
_unix_file_form(u2_reck* rec_u, 
                u2_udir* dir_u, 
                u2_noun  pre,
                u2_bean  ket,
                u2_noun  ext)
{
  c3_c* pre_c = u2_cr_string(pre);
  c3_c* ext_c = u2_cr_string(ext);
  c3_w  pax_w = strlen(dir_u->pax_c);
  c3_w  pre_w = strlen(pre_c);
  c3_w  ext_w = strlen(ext_c);
  c3_w  ket_w = (u2_yes == ket) ? 1 : 0;
  c3_c* pax_c = malloc(pax_w + 1 + pre_w + 1 + ket_w + ext_w + 1);

  strcpy(pax_c, dir_u->pax_c);
  pax_c[pax_w] = '/';
  strcpy(pax_c + pax_w + 1, pre_c);
  pax_c[pax_w + 1 + pre_w] = '.';
  if ( u2_yes == ket ) {
    pax_c[pax_w + 1 + pre_w + 1] = '^';
  }
  strcpy(pax_c + pax_w + 1 + pre_w + 1 + ket_w, ext_c);

  free(pre_c); free(ext_c);
  u2z(pre); u2z(ext);

  return pax_c;
}

/* _unix_dir_watch(): instantiate directory tracker.
*/
static void
_unix_dir_watch(u2_reck* rec_u, u2_udir* dir_u, u2_udir* par_u, c3_c* pax_c)
{
  ev_stat_init(&dir_u->was_u, _lo_stat, pax_c, 1.0);

  dir_u->yes = u2_yes;
  dir_u->dry = u2_no;
  dir_u->pax_c = pax_c;
  dir_u->par_u = par_u;
  dir_u->dis_u = 0;
  dir_u->fil_u = 0;
  dir_u->nex_u = 0;
}

/* _unix_dir_forge: instantiate directory tracker (and make directory).
*/
static void
_unix_dir_forge(u2_reck* rec_u, u2_udir* dir_u, u2_udir* par_u, u2_noun tet)
{
  dir_u->yes = u2_yes;
  dir_u->dry = u2_no;
  { 
    c3_c* tet_c = u2_cr_string(tet);
    c3_w  pax_w = strlen(par_u->pax_c);
    c3_w  tet_w = strlen(tet_c);
    c3_c* pax_c = malloc(pax_w + 1 + tet_w + 1);

    strcpy(pax_c, par_u->pax_c);
    pax_c[pax_w] = '/';
    strcpy(pax_c + pax_w + 1, tet_c);

    free(tet_c);
    u2z(tet);

    ev_stat_init(&dir_u->was_u, _lo_stat, pax_c, 1.0);
    _unix_mkdir(pax_c);
    dir_u->pax_c = pax_c;
  }
  dir_u->par_u = par_u;
  dir_u->dis_u = 0;
  dir_u->fil_u = 0;
  dir_u->nex_u = 0;
}

/* _unix_file_free(): free (within) file tracker.
*/
static void
_unix_file_free(u2_reck* rec_u, u2_ufil* fil_u)
{
  free(fil_u->pax_c);
  mpz_clear(fil_u->mod_mp);
}

#if 0
/* _unix_file_sane(): sanity check file.
*/
static void
_unix_file_sane(u2_ufil* fil_u)
{
}

/* _unix_dir_sane(): sanity check directory.
*/
static void
_unix_dir_sane(u2_udir* dir_u)
{
  u2_udir* dis_u;
  u2_ufil* fil_u;

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
static void
_unix_dir_free(u2_reck* rec_u, u2_udir* dir_u)
{
  free(dir_u->pax_c);

  while ( dir_u->dis_u ) {
    u2_udir* nex_u = dir_u->dis_u->nex_u;

    _unix_dir_free(rec_u, dir_u->dis_u);
    free(dir_u->dis_u);
    dir_u->dis_u = nex_u;
  }
  while ( dir_u->fil_u ) {
    u2_ufil* nex_u = dir_u->fil_u->nex_u;

    _unix_file_free(rec_u, dir_u->fil_u);
    free(dir_u->fil_u);
    dir_u->fil_u = nex_u;
  }
}

/* _unix_file_update(): update file, true if plausibly changed.
*/
static u2_bean
_unix_file_update(u2_reck* rec_u, u2_ufil* fil_u, mpz_t mod_mp)
{
  if ( 0 == mpz_cmp(mod_mp, fil_u->mod_mp) ) {
    return u2_no;
  } else {
    mpz_clear(fil_u->mod_mp);
    mpz_init_set(fil_u->mod_mp, mod_mp);

    return u2_yes;
  }
}

/* _unix_dir_update(): update directory, true if changed.
*/
static u2_bean
_unix_dir_update(u2_reck* rec_u, u2_udir* dir_u, DIR* rid_u)
{
  u2_bean cha = u2_no;

  // uL(fprintf(uH, "dir_update ON %s\n", dir_u->pax_c));
  /* use dry bits as markers
  */
  {
    u2_udir* dis_u;
    u2_ufil* fil_u;

    for ( dis_u = dir_u->dis_u; dis_u; dis_u = dis_u->nex_u ) {
      dis_u->dry = u2_yes;
    }
    for ( fil_u = dir_u->fil_u; fil_u; fil_u = fil_u->nex_u ) {
      fil_u->dry = u2_yes;
    }
  }

  /* iterate through directory, opening and updating
  */
  while ( 1 ) {
    struct dirent  ent_u;
    struct dirent* out_u;

    if ( readdir_r(rid_u, &ent_u, &out_u) != 0 ) {
      uL(fprintf(uH, "%s: %s\n", dir_u->pax_c, strerror(errno)));
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
          u2_ufil* fil_u;

          {
            u2_noun mod = c3_stat_mtime(&buf_u);

            u2_cr_mp(mod_mp, mod);
            u2z(mod);
          }
          for ( fil_u = dir_u->fil_u; fil_u; fil_u = fil_u->nex_u ) {
            if ( !strcmp(pax_c, fil_u->pax_c) ) {
              fil_u->dry = u2_no;
              cha = u2_or(cha, _unix_file_update(rec_u, fil_u, mod_mp));
              break;
            }
          }
          if ( !fil_u ) {
            fil_u = malloc(sizeof(u2_ufil));
            // uL(fprintf(uH, "found file %s\n", pax_c));
            _unix_file_watch(rec_u, fil_u, dir_u, pax_c, mod_mp);

            fil_u->nex_u = dir_u->fil_u;
            dir_u->fil_u = fil_u;
            cha = u2_yes;
          }
          mpz_clear(mod_mp);
        }
        else {
          DIR* red_u = _unix_opendir(pax_c);
          u2_udir* dis_u;

          for ( dis_u = dir_u->dis_u; dis_u; dis_u = dis_u->nex_u ) {
            if ( !strcmp(pax_c, dis_u->pax_c) ) {
              dis_u->dry = u2_no;
              cha = u2_or(cha, _unix_dir_update(rec_u, dis_u, red_u));
              break;
            }
          }
          if ( !dis_u ) {
            dis_u = malloc(sizeof(u2_udir));
            // uL(fprintf(uH, "found directory %s\n", pax_c));
            _unix_dir_watch(rec_u, dis_u, dir_u, pax_c); 
            _unix_dir_update(rec_u, dis_u, red_u);

            dis_u->nex_u = dir_u->dis_u;
            dir_u->dis_u = dis_u;
            cha = u2_yes;
          }
        }
      }
    }
  }

  /* use dry bits as markers
  */
  {
    u2_udir** dis_u;
    u2_ufil** fil_u;

    for ( dis_u = &(dir_u->dis_u); *dis_u; ) {
      if ( u2_no == (*dis_u)->dry ) {
        (*dis_u)->dry = u2_yes;
        dis_u = &(*dis_u)->nex_u;
      }
      else {
        u2_udir* ded_u = *dis_u;
        u2_udir* nex_u = ded_u->nex_u;

        // uL(fprintf(uH, "removed directory %s\n", ded_u->pax_c));
        _unix_dir_free(rec_u, ded_u);
        free(ded_u);

        *dis_u = nex_u;
        cha = u2_yes;
      }
    }

    for ( fil_u = &(dir_u->fil_u); *fil_u; ) {
      if ( u2_no == (*fil_u)->dry ) {
        fil_u = &(*fil_u)->nex_u;
      }
      else {
        u2_ufil* ded_u = *fil_u;
        u2_ufil* nex_u = ded_u->nex_u;

        // uL(fprintf(uH, "removed file %s\n", ded_u->pax_c));
        _unix_file_free(rec_u, ded_u);
        free(ded_u);

        *fil_u = nex_u;
        cha = u2_yes;
      }
    }
  }
  closedir(rid_u);
  // uL(fprintf(uH, "dir_update OFF %s\n", dir_u->pax_c));
  return cha;
}

/* unix_load(): load a file.
*/
static u2_noun
_unix_load(c3_c* pax_c)
{
  struct stat buf_u;
  c3_i        fid_i = open(pax_c, O_RDONLY, 0644);
  c3_w        fln_w, red_w;
  c3_y*       pad_y;

  if ( (fid_i < 0) || (fstat(fid_i, &buf_u) < 0) ) {
    // uL(fprintf(uH, "%s: %s\n", pax_c, strerror(errno)));
    c3_assert(0); 
    return 0;
  }
  fln_w = buf_u.st_size;
  pad_y = malloc(buf_u.st_size);

  red_w = read(fid_i, pad_y, fln_w);
  close(fid_i);

  if ( fln_w != red_w ) {
    free(pad_y);
    c3_assert(0);
    return 0;
  }
  else {
    u2_noun pad = u2_ci_bytes(fln_w, (c3_y *)pad_y); 
    free(pad_y);

    return pad;
  }
}

/* unix_save(): save a file.
*/
static void
_unix_save(c3_c* pax_c, u2_atom oat)
{
  c3_i  fid_i = open(pax_c, O_WRONLY | O_CREAT | O_TRUNC, 0666);
  c3_w  fln_w, rit_w;
  c3_y* oat_y;

  if ( fid_i < 0 ) {
    uL(fprintf(uH, "%s: %s\n", pax_c, strerror(errno)));
    u2_cm_bail(c3__fail);
  }

  fln_w = u2_met(3, oat);
  oat_y = malloc(fln_w);
  u2_cr_bytes(0, fln_w, oat_y, oat);
  u2z(oat);

  rit_w = write(fid_i, oat_y, fln_w);
  close(fid_i);
  free(oat_y);

  if ( rit_w != fln_w ) {
    uL(fprintf(uH, "%s: %s\n", pax_c, strerror(errno)));
    c3_assert(0);
  }
}

/* _unix_file_load(): load a file by watcher.
*/
static u2_noun
_unix_file_load(u2_reck* rec_u, u2_ufil* fil_u)
{
  u2_noun raw = _unix_load(fil_u->pax_c);

  if ( (0 == raw) || ('^' != fil_u->dot_c[1]) ) {
    return raw;
  }
  else return u2_cke_cue(raw);
}


/* _unix_dir_name(): directory name.
*/
static u2_noun
_unix_dir_name(u2_reck* rec_u, u2_udir* dir_u)
{
  c3_w pel_w = strlen(dir_u->par_u->pax_c);
  c3_c* pax_c = dir_u->pax_c + pel_w + 1;
  c3_c* fas_c = strchr(pax_c, '/');

  return fas_c ? u2_ci_bytes((fas_c - pax_c), (c3_y*) pax_c)
               : u2_ci_string(pax_c);
}


/* _unix_file_name(): file name/extension.
*/
static u2_noun 
_unix_file_name(u2_reck* rec_u, u2_ufil* fil_u)
{
  c3_w pel_w = strlen(fil_u->par_u->pax_c);
  c3_c* pax_c = fil_u->pax_c + pel_w + 1;

  if ( !fil_u->dot_c ) {
    return u2_ci_string(pax_c);
  } 
  else {
    c3_c* ext_c = fil_u->dot_c + 1;

    ext_c = (*ext_c == '^') ? (ext_c + 1) : ext_c;
    return u2nc(u2_ci_bytes((fil_u->dot_c - pax_c), (c3_y*)pax_c),
                u2_ci_string(ext_c));
  }
}

/* _unix_dir_arch(): resolve directory to old style arch.
*/
static u2_noun
_unix_dir_arch(u2_reck* rec_u, u2_udir* dir_u)
{
  u2_udir* dis_u;
  u2_ufil* fil_u;
  u2_noun pam = u2_nul;

  for ( dis_u = dir_u->dis_u; dis_u; dis_u = dis_u->nex_u ) {
    u2_noun pre = _unix_dir_name(rec_u, dis_u);
    u2_noun arc = _unix_dir_arch(rec_u, dis_u);

    // uL(fprintf(uH, "dir %s\n", u2_cr_string(pre)));
    pam = u2_ckd_by_put(pam, pre, arc);
  }

  for ( fil_u = dir_u->fil_u; fil_u; fil_u = fil_u->nex_u ) {
    u2_noun wib = _unix_file_name(rec_u, fil_u);
    u2_noun baw = _unix_file_load(rec_u, fil_u);
    u2_noun has = u2_do("sham", u2k(baw));
    u2_noun fil = u2nt(u2_yes, has, baw);

    if ( u2_no == u2du(wib) ) {
      // uL(fprintf(uH, "file %s\n", u2_cr_string(wib)));
      pam = u2_ckd_by_put(pam, wib, fil);
    }
    else {
      u2_weak ole = u2_ckd_by_get(u2k(pam), u2k(u2h(wib)));
      u2_noun arc, rac;

      // uL(fprintf(uH, "file %s.%s\n", u2_cr_string(u2h(wib)),
      //                               u2_cr_string(u2t(wib))));

      if ( u2_none == ole ) {
        arc = u2nc(u2_no, u2_nul);
      } 
      else if ( u2_yes == u2h(ole) ) {
        u2z(ole);
        arc = u2nc(u2_no, u2_nul);
      }
      else arc = ole;

      rac = u2nc(u2_no, u2_ckd_by_put(u2k(u2t(arc)), u2k(u2t(wib)), fil));
      pam = u2_ckd_by_put(pam, u2k(u2h(wib)), rac);

      u2z(wib); u2z(arc);
    }
  }
  return u2nc(u2_no, pam);
}

/* _unix_desk_peek(): peek for arch.
*/
static u2_noun
_unix_desk_peek(u2_reck* rec_u, 
                u2_noun who, 
                u2_noun hox, 
                u2_noun syd, 
                u2_noun lok)
{
  u2_noun cay;

  cay = u2_reck_prick
    (rec_u, who, u2nc(c3_s2('c','z'), u2nq(hox, syd, lok, u2_nul)));

  if ( u2_nul == cay ) {
    return u2nc(u2_no, u2_nul);
  } else {
    u2_noun arc = u2k(u2t(cay));

    u2z(cay); return arc;
  }
}

/* _unix_desk_sync_into(): sync external changes to desk.
*/
static void
_unix_desk_sync_into(u2_reck* rec_u, 
                     u2_noun  who,
                     u2_noun  hox, 
                     u2_noun  syd, 
                     u2_udir* dir_u)
{
  u2_noun xun, bur, doz, fav, pax;

  xun = _unix_dir_arch(rec_u, dir_u);
  bur = _unix_desk_peek(rec_u, u2k(who), hox, syd, u2k(rec_u->wen));

  if ( u2_no == u2_sing(xun, bur) ) {
    doz = u2_dc("cyst", xun, bur);
    pax = u2nq(c3__gold, c3__sync, u2k(rec_u->sen), u2_nul);
    fav = u2nq(c3__into, who, syd, u2nc(u2_yes, doz));

    u2_reck_plan(rec_u, pax, fav);
  } 
  else {
    u2z(xun); u2z(bur);
  }
}

/* _unix_ship_update(): update top level ship.
*/
static void
_unix_ship_update(u2_reck* rec_u, u2_uhot* hot_u)
{
  u2_udir* dir_u = &(hot_u->dir_u);
  DIR*     rid_u = _unix_opendir(dir_u->pax_c);

  if ( u2_yes == _unix_dir_update(rec_u, dir_u, rid_u) ) {
    u2_udir* dis_u;
    u2_noun  who, hox;

    {
      mpz_t who_mp;

      mpz_init_set(who_mp, hot_u->who_mp);
      who = u2_ci_mp(who_mp);
      hox = u2_dc("scot", 'p', u2k(who));
    }

    for ( dis_u = dir_u->dis_u; dis_u; dis_u = dis_u->nex_u ) { 
      u2_noun syd = _unix_dir_name(rec_u, dis_u); 

      // uL(fprintf(uH, "sync %s %s\n", u2_cr_string(hox), u2_cr_string(syd))); 
      _unix_desk_sync_into(rec_u, u2k(who), u2k(hox), syd, dis_u);
    }
    u2z(hox);
    u2z(who);
  }
}
 
/* _unix_hot_gain(): gain ship.
*/
static void
_unix_hot_gain(u2_reck* rec_u, u2_noun who, u2_bean mek)
{
  u2_noun hox = u2_dc("scot", 'p', who);
  c3_c*   hox_c = u2_cr_string(hox);
  c3_c*   pax_c = _unix_down(u2_Host.ops_u.hom_c, hox_c + 1);
  DIR*    rid_u = opendir(pax_c);

  if ( !rid_u ) {
    if ( u2_yes == mek ) {
      _unix_mkdir(pax_c);
    } else return;
  } else closedir(rid_u);

  // uL(fprintf(uH, "GAIN %s\n", pax_c));
  free(hox_c);
  u2z(hox);
  u2_unix_acquire(pax_c);

  {
    u2_uhot* hot_u = malloc(sizeof(u2_uhot));

    _unix_dir_watch(rec_u, &hot_u->dir_u, 0, pax_c);

    u2_cr_mp(hot_u->who_mp, who);
    hot_u->nex_u = u2_Host.unx_u.hot_u;
    u2_Host.unx_u.hot_u = hot_u;
  }
}

/* _unix_hot_lose(): release within a host directory.
*/
static void
_unix_hot_lose(u2_reck* rec_u, u2_uhot* hot_u)
{
  uL(fprintf(uH, "lose: %s\n", hot_u->dir_u.pax_c));
  _unix_dir_free(rec_u, &(hot_u->dir_u));
}

/* _unix_pdir(): find directory reference from text.
*/
static u2_udir**
_unix_pdir(u2_reck* rec_u, u2_udir* par_u, u2_noun tet)
{
  c3_c*     tet_c = u2_cr_string(tet);
  c3_w      pax_w = strlen(par_u->pax_c);
  c3_w      tet_w = strlen(tet_c);
  u2_udir** dir_u;

  dir_u = &(par_u->dis_u); 
  while ( 1 ) { 
    if ( !*dir_u || !strncmp(((*dir_u)->pax_c + pax_w + 1), tet_c, tet_w) ) {
      free(tet_c); return dir_u;
    }
    else dir_u = &((*dir_u)->nex_u);
  }
}

/* _unix_home(): find home directory from identity.
*/
static u2_uhot*
_unix_home(u2_reck* rec_u, u2_noun who)
{
  u2_unix* unx_u = &u2_Host.unx_u;
  u2_uhot* hot_u;
  mpz_t    who_mp;

  u2_cr_mp(who_mp, who);
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
static u2_noun
_unix_desk_sync_udon(u2_reck* rec_u, u2_noun don, u2_noun old)
{
  return u2_dc("lump", don, old);
}

/* _unix_desk_sync_tofu(): sync out file install.
*/
static void
_unix_desk_sync_tofu(u2_reck* rec_u, 
                     u2_udir* dir_u,
                     u2_noun  pre,
                     u2_noun  ext,
                     u2_noun  mis)
{
  c3_c*     pox_c = _unix_file_form(rec_u, dir_u, u2k(pre), u2_no, u2k(ext));
  c3_c*     pux_c = _unix_file_form(rec_u, dir_u, u2k(pre), u2_yes, u2k(ext));
  u2_ufil** fil_u;

  // uL(fprintf(uH, "tofu pox_c %s op %s\n", pox_c, u2_cr_string(u2h(mis))));

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
 
  if ( *fil_u && (c3__del == u2h(mis)) ) {
    u2_ufil* ded_u = *fil_u;

    *fil_u = ded_u->nex_u;
    _unix_unlink(ded_u->pax_c);
    _unix_file_free(rec_u, ded_u);

    free(ded_u);
    free(pox_c);
    free(pux_c);
  }
  else {
    u2_noun god, oat;
    c3_c*   pax_c;

    if ( *fil_u ) {
      u2_noun old = _unix_file_load(rec_u, *fil_u);
      c3_assert(c3__mut == u2h(mis));

      god = _unix_desk_sync_udon(rec_u, u2k(u2t(mis)), old);
      _unix_unlink((*fil_u)->pax_c);
      free((*fil_u)->pax_c);
    } 
    else {
      c3_assert(c3__ins == u2h(mis));
      god = u2k(u2t(mis));
    }

    if ( u2_yes == u2du(god) ) {
      oat = u2_cke_jam(god);
      pax_c = pux_c; free(pox_c);
    } else {
      oat = god;
      pax_c = pox_c; free(pux_c);
    }

    if ( *fil_u ) {
      (*fil_u)->dot_c = (pax_c + ((*fil_u)->dot_c - (*fil_u)->pax_c));
      (*fil_u)->pax_c = pax_c;

      mpz_clear((*fil_u)->mod_mp);
      u2_cr_mp((*fil_u)->mod_mp, rec_u->now);
    } 
    else {
      mpz_t mod_mp;

      u2_cr_mp(mod_mp, rec_u->now);
      *fil_u = malloc(sizeof(u2_ufil));

      _unix_file_watch(rec_u, *fil_u, dir_u, pax_c, mod_mp);
      mpz_clear(mod_mp);
    }

    _unix_save((*fil_u)->pax_c, oat);
  }
  u2z(pre); u2z(ext); u2z(mis);
}

/* _unix_desk_sync_miso(): sync out change.
*/
static void
_unix_desk_sync_miso(u2_reck* rec_u, u2_udir* dir_u, u2_noun pax, u2_noun mis)
{
  if ( (u2_no == u2du(pax)) || u2_no == u2du(u2t(pax)) ) {
    u2_err(u2_Wire, "pax", pax);

    u2z(pax); u2z(mis);
  }
  else {
    u2_noun i_pax = u2h(pax); 
    u2_noun t_pax = u2t(pax);
    u2_noun it_pax = u2h(t_pax);
    u2_noun tt_pax = u2t(t_pax);

    if ( u2_nul == tt_pax ) {
      _unix_desk_sync_tofu(rec_u, dir_u, u2k(i_pax), u2k(it_pax), mis);
    } 
    else {
      u2_udir** dis_u = _unix_pdir(rec_u, dir_u, u2k(i_pax));

      if ( !*dis_u ) {
        *dis_u = malloc(sizeof(u2_udir));

        _unix_dir_forge(rec_u, *dis_u, dir_u, u2k(i_pax));
      }
      _unix_desk_sync_miso(rec_u, *dis_u, u2k(t_pax), mis);
    }
  }
  u2z(pax);
}

/* _unix_desk_sync_soba(): sync computed changes.
*/
static void
_unix_desk_sync_soba(u2_reck* rec_u, u2_udir* dir_u, u2_noun doz)
{
  u2_noun zod = doz;

  while ( u2_nul != zod ) {
    _unix_desk_sync_miso(rec_u, dir_u, u2k(u2h(u2h(zod))), u2k(u2t(u2h(zod))));
    zod = u2t(zod);
  }
  u2z(doz);
}

/* _unix_desk_sync_ergo(): sync desk changes to unix.
*/
static void
_unix_desk_sync_ergo(u2_reck* rec_u, 
                     u2_noun  who,
                     u2_noun  hox, 
                     u2_noun  syd, 
                     u2_noun  lok,
                     u2_uhot* hot_u) 
{
  u2_udir** dir_u = _unix_pdir(rec_u, &(hot_u->dir_u), syd);
  u2_noun   xun;

#if 0
  uL(fprintf(uH, "ergo %s %s %s\n", u2_cr_string(hox),
                                    u2_cr_string(syd),
                                    u2_cr_string(lok)));
#endif

  if ( !*dir_u ) {
    *dir_u = malloc(sizeof(u2_udir));

    xun = u2nc(u2_no, u2_nul);
    _unix_dir_forge(rec_u, *dir_u, &(hot_u->dir_u), u2k(syd));
  } else {
    xun = _unix_dir_arch(rec_u, *dir_u);
  }

  {
    u2_noun bur = _unix_desk_peek(rec_u, who, hox, syd, lok);

    if ( u2_no == u2_sing(xun, bur) ) {
      u2_noun doz = u2_dc("cyst", bur, xun);

      _unix_desk_sync_soba(rec_u, *dir_u, doz);
    }
    else {
      u2z(xun); u2z(bur);
    }
  }
}

/* u2_unix_ef_init(): update filesystem for new acquisition.
*/
void
u2_unix_ef_init(u2_reck* rec_u,
                u2_noun  who)
{
  _unix_hot_gain(rec_u, u2k(who), u2_yes);

  u2_reck_plan(rec_u,
               u2nq(c3__gold, c3__sync, u2k(rec_u->sen), u2_nul),
               u2nq(c3__into, who, u2_blip, u2nc(u2_yes, u2_nul)));
}

/* u2_unix_ef_ergo(): update filesystem, outbound.
*/
void
u2_unix_ef_ergo(u2_reck* rec_u,
                u2_noun  who,
                u2_noun  syd,
                u2_noun  rel)
{
  u2_noun  hox = u2_dc("scot", 'p', u2k(who));
  u2_noun  lok = u2_dc("scot", c3__ud, rel);
  u2_uhot* hot_u;

  hot_u = _unix_home(rec_u, u2k(who));

  if ( 0 != hot_u ) {
    _unix_desk_sync_ergo(rec_u, who, hox, syd, lok, hot_u);
  }
}

/* u2_unix_ef_look(): update the root.
*/
void
u2_unix_ef_look(u2_reck* rec_u)
{
  u2_unix* unx_u = &u2_Host.unx_u;
  u2_noun  won;
  u2_uhot* hot_u; 

  //  find owners without directories
  {
    for ( won = rec_u->own; u2_nul != won; won = u2t(won) ) {
      u2_noun who = u2h(won);
      mpz_t who_mp;

      u2_cr_mp(who_mp, who);
      for ( hot_u = unx_u->hot_u; 
            hot_u && (0 != mpz_cmp(who_mp, hot_u->who_mp));
            hot_u = hot_u->nex_u );

      mpz_clear(who_mp);
      if ( 0 == hot_u ) {
        _unix_hot_gain(rec_u, u2k(who), u2_no);
      }
    }
  }

  //  find directories without owners
  {
    u2_uhot** het_u = &(unx_u->hot_u);

    while ( 0 != (hot_u=*het_u) ) {
      for ( won = rec_u->own; u2_nul != won; won = u2t(won) ) {
        u2_noun who = u2h(won);
        mpz_t   who_mp;

        u2_cr_mp(who_mp, who);
        if ( 0 == mpz_cmp(who_mp, hot_u->who_mp) ) {
          break;
        }
        mpz_clear(who_mp);
      }

      if ( u2_nul == won ) {
        *het_u = hot_u->nex_u;
 
        // uL(fprintf(uH, "sync: lose %s\n", hot_u->dir_u.pax_c));
        _unix_hot_lose(rec_u, hot_u);

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
    u2_uhot* hot_u;

    for ( hot_u = unx_u->hot_u; hot_u; hot_u = hot_u->nex_u ) {
      _unix_ship_update(rec_u, hot_u);
    }
  }
}

/* u2_unix_io_init(): initialize unix sync.
*/
void 
u2_unix_io_init(u2_reck* rec_u)
{
  u2_unix* unx_u = &u2_Host.unx_u;

  u2_unix_acquire(u2_Host.cpu_c);

  ev_timer_init(&unx_u->tim_u, _lo_unix, 10000.0, 0.);
  unx_u->alm = u2_no;
  unx_u->sig_u = 0;
  unx_u->hot_u = 0;

  {
    u2_usig* sig_u;

    sig_u = malloc(sizeof(u2_usig));
    ev_signal_init(&sig_u->sil_u, _lo_sign, SIGINT);

    sig_u->nex_u = unx_u->sig_u;
    unx_u->sig_u = sig_u;
  }
  {
    u2_usig* sig_u;

    sig_u = malloc(sizeof(u2_usig));
    ev_signal_init(&sig_u->sil_u, _lo_sign, SIGTERM);

    sig_u->nex_u = unx_u->sig_u;
    unx_u->sig_u = sig_u;
  }
  {
    u2_usig* sig_u;

    sig_u = malloc(sizeof(u2_usig));
    ev_signal_init(&sig_u->sil_u, _lo_sign, SIGWINCH);

    sig_u->nex_u = unx_u->sig_u;
    unx_u->sig_u = sig_u;
  }
}

/* u2_unix_io_exit(): terminate unix I/O.
*/
void 
u2_unix_io_exit(u2_reck* rec_u)
{
  u2_unix_release(u2_Host.cpu_c);

  {
    u2_uhot* hot_u;

    for ( hot_u = u2_Host.unx_u.hot_u; hot_u; hot_u = hot_u->nex_u ) {
      u2_unix_release(hot_u->dir_u.pax_c);
    }
  }
}

/* u2_unix_io_spin(): start unix server(s).
*/
void
u2_unix_io_spin(u2_reck*        rec_u,
                struct ev_loop* lup_u)
{
  u2_unix* unx_u = &u2_Host.unx_u;

  if ( u2_yes == unx_u->alm ) {
    ev_timer_start(lup_u, &unx_u->tim_u);
  }

  {
    u2_usig* sig_u;

    for ( sig_u = unx_u->sig_u; sig_u; sig_u = sig_u->nex_u ) {
      ev_signal_start(lup_u, &sig_u->sil_u);
    }
  }
}

/* u2_unix_io_stop(): stop unix servers.
*/
void
u2_unix_io_stop(u2_reck*        rec_u,
                struct ev_loop* lup_u)
{
  u2_unix* unx_u = &u2_Host.unx_u;

  if ( u2_yes == unx_u->alm ) {
    ev_timer_stop(lup_u, &unx_u->tim_u);
  }

  {
    u2_usig* sig_u;

    for ( sig_u = unx_u->sig_u; sig_u; sig_u = sig_u->nex_u ) {
      ev_signal_stop(lup_u, &sig_u->sil_u);
    }
  }
}

/* u2_unix_io_poll(): update unix IO state.
*/
void
u2_unix_io_poll(u2_reck*        rec_u,
                struct ev_loop* lup_u)
{
  u2_unix* unx_u = &u2_Host.unx_u;

  {
    u2_noun wen = u2_reck_keep(rec_u, u2nt(c3__gold, c3__clay, u2_nul));
    
    if ( (u2_nul != wen) && 
         (u2_yes == u2du(wen)) &&
         (u2_yes == u2ud(u2t(wen))) )
    {
      double gap_g = u2_time_gap_double(u2k(rec_u->now), u2k(u2t(wen)));

      unx_u->alm = u2_yes;
      ev_timer_set(&unx_u->tim_u, gap_g, 0.);
    }
    else {
      unx_u->alm = u2_no;
    }
  }
}

/* u2_unix_io_time(): time event on unix channel.
*/
void
u2_unix_io_time(u2_reck*         rec_u,
                struct ev_timer* tim_u)
{
  u2_reck_plan
    (rec_u,
     u2nt(c3__gold, c3__clay, u2_nul),
     u2nc(c3__wake, u2_nul));
}

/* u2_unix_io_stat(): fs event on storage.
*/
void
u2_unix_io_stat(u2_reck*        rec_u,
                struct ev_stat* sat_u)
{
}

/* u2_unix_io_sign(): signal event.
*/
void
u2_unix_io_sign(u2_reck*          rec_u,
                struct ev_signal* sil_u)
{
  switch ( sil_u->signum ) {
    case SIGTERM: u2_Host.liv = u2_no; break;
    case SIGINT: u2_term_ef_ctlc(rec_u); break;
    case SIGWINCH: u2_term_ef_winc(rec_u); break;
  }
}
