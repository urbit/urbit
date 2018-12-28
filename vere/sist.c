/* v/sist.c
**
*/
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <uv.h>

#include "all.h"
#include "vere/vere.h"

#if defined(U3_OS_linux)
#define DEVRANDOM "/dev/urandom"
#else
#define DEVRANDOM "/dev/random"
#endif

/* u3_sist_pack(): write a blob to disk, transferring.
*/
c3_d
u3_sist_pack(c3_w tem_w, c3_w typ_w, c3_w* bob_w, c3_w len_w)
{
  u3_ulog* lug_u = &u3Z->lug_u;
  c3_d     tar_d;
  u3_ular  lar_u;

  tar_d = lug_u->len_d + len_w;

  lar_u.tem_w = tem_w;
  lar_u.typ_w = typ_w;
  lar_u.syn_w = u3r_mug_d(tar_d);
  lar_u.mug_w = u3r_mug_both(u3r_mug_words(bob_w, len_w),
                               u3r_mug_both(u3r_mug(lar_u.tem_w),
                                              u3r_mug(lar_u.typ_w)));
  lar_u.ent_d = u3A->ent_d;
  u3A->ent_d++;
  lar_u.len_w = len_w;

  if ( -1 == lseek64(lug_u->fid_i, 4ULL * tar_d, SEEK_SET) ) {
    uL(fprintf(uH, "sist_pack: seek failed, lseek: %s\n", strerror(errno)));
    c3_assert(0);
  }
  if ( sizeof(lar_u) != write(lug_u->fid_i, &lar_u, sizeof(lar_u)) ) {
    uL(fprintf(uH, "sist_pack: write failed, write: %s\n", strerror(errno)));
    c3_assert(0);
  }
  if ( -1 == lseek64(lug_u->fid_i, 4ULL * lug_u->len_d, SEEK_SET) ) {
    uL(fprintf(uH, "sist_pack: seek failed, lseek: %s\n", strerror(errno)));
    c3_assert(0);
  }
#if 0
  uL(fprintf(uH, "sist_pack: write %" PRIu64 ", %" PRIu64 ": lar ent %" PRIu64 ", len %d, mug %x\n",
                 lug_u->len_d,
                 tar_d,
                 lar_u.ent_d,
                 lar_u.len_w,
                 lar_u.mug_w));
#endif
  if ( (4 * len_w) != write(lug_u->fid_i, bob_w, (4 * len_w)) ) {
    uL(fprintf(uH, "sist_pack: write failed, write: %s\n", strerror(errno)));
    c3_assert(0);
  }
  lug_u->len_d += (c3_d)(lar_u.len_w + c3_wiseof(lar_u));
  free(bob_w);

  //  Sync.  Or, what goes by sync.
  {
    fsync(lug_u->fid_i);    //  fsync is almost useless, F_FULLFSYNC too slow
#if defined(U3_OS_linux)
    fdatasync(lug_u->fid_i);
#elif defined(U3_OS_osx)
    fcntl(lug_u->fid_i, F_FULLFSYNC);
#elif defined(U3_OS_bsd)
    fsync(lug_u->fid_i);
#else
#   error "port: datasync"
#endif
  }

  return u3A->ent_d;
}

/* u3_sist_put(): moronic key-value store put.
*/
void
u3_sist_put(const c3_c* key_c, const c3_y* val_y, size_t siz_i)
{
  c3_c ful_c[2048];
  c3_i ret_i;
  c3_i fid_i;

  ret_i = snprintf(ful_c, 2048, "%s/.urb/sis/_%s", u3_Host.dir_c, key_c);
  c3_assert(ret_i < 2048);

  if ( (fid_i = open(ful_c, O_CREAT | O_TRUNC | O_WRONLY, 0600)) < 0 ) {
    uL(fprintf(uH, "sist: could not put %s: %s\n", key_c, strerror(errno)));
    u3_lo_bail();
  }
  if ( (ret_i = write(fid_i, val_y, siz_i)) != siz_i ) {
    uL(fprintf(uH, "sist: could not write %s\n", key_c));
    if ( ret_i < 0 ) {
      uL(fprintf(uH, "write: %s\n", strerror(errno)));
    }
    u3_lo_bail();
  }
  ret_i = c3_sync(fid_i);
  if ( ret_i < 0 ) {
    uL(fprintf(uH, "sync: %s\n", strerror(errno)));
  }
  ret_i = close(fid_i);
  c3_assert(0 == ret_i);
}

/* u3_sist_has(): moronic key-value store existence check.
*/
ssize_t
u3_sist_has(const c3_c* key_c)
{
  c3_c        ful_c[2048];
  c3_i        ret_i;
  struct stat sat_u;

  ret_i = snprintf(ful_c, 2048, "%s/.urb/sis/_%s", u3_Host.dir_c, key_c);
  c3_assert(ret_i < 2048);

  if ( (ret_i = stat(ful_c, &sat_u)) < 0 ) {
    if ( errno == ENOENT ) {
      return -1;
    }
    else {
      uL(fprintf(uH, "sist: could not stat %s: %s\n", key_c, strerror(errno)));
      u3_lo_bail();
    }
  }
  else {
    return sat_u.st_size;
  }
  c3_assert(!"not reached");
}

/* u3_sist_get(): moronic key-value store get.
*/
void
u3_sist_get(const c3_c* key_c, c3_y* val_y)
{
  c3_c        ful_c[2048];
  c3_i        ret_i;
  c3_i        fid_i;
  struct stat sat_u;

  ret_i = snprintf(ful_c, 2048, "%s/.urb/sis/_%s", u3_Host.dir_c, key_c);
  c3_assert(ret_i < 2048);

  if ( (fid_i = open(ful_c, O_RDONLY)) < 0 ) {
    uL(fprintf(uH, "sist: could not get %s: %s\n", key_c, strerror(errno)));
    u3_lo_bail();
  }
  if ( (ret_i = fstat(fid_i, &sat_u)) < 0 ) {
    uL(fprintf(uH, "sist: could not stat %s: %s\n", key_c, strerror(errno)));
    u3_lo_bail();
  }
  if ( (ret_i = read(fid_i, val_y, sat_u.st_size)) != sat_u.st_size ) {
    uL(fprintf(uH, "sist: could not read %s\n", key_c));
    if ( ret_i < 0 ) {
      uL(fprintf(uH, "read: %s\n", strerror(errno)));
    }
    u3_lo_bail();
  }
  ret_i = close(fid_i);
  c3_assert(0 == ret_i);
}

/* u3_sist_nil(): moronic key-value store rm.
*/
void
u3_sist_nil(const c3_c* key_c)
{
  c3_c ful_c[2048];
  c3_i ret_i;

  ret_i = snprintf(ful_c, 2048, "%s/.urb/sis/_%s", u3_Host.dir_c, key_c);
  c3_assert(ret_i < 2048);

  if ( (ret_i = unlink(ful_c)) < 0 ) {
    if ( errno == ENOENT ) {
      return;
    }
    else {
      uL(fprintf(uH, "sist: could not unlink %s: %s\n", key_c,
                     strerror(errno)));
      u3_lo_bail();
    }
  }
}

/* _sist_suck(): past failure.
*/
static void
_sist_suck(u3_noun ovo, u3_noun gon)
{
  uL(fprintf(uH, "sing: ovum failed!\n"));
  {
    c3_c* hed_c = u3r_string(u3h(u3t(ovo)));

    uL(fprintf(uH, "fail %s\n", hed_c));
    free(hed_c);
  }

  u3_lo_punt(2, u3kb_flop(u3k(u3t(gon))));
  // u3_loom_exit();
#if 1
  u3_lo_exit();

  exit(1);
#else
  u3z(ovo); u3z(gon);
#endif
}

/* _sist_sing(): replay ovum from the past, time already set.
*/
static void
_sist_sing(u3_noun ovo)
{
  u3_noun gon = u3m_soft(0, u3v_poke, u3k(ovo));

  {
    u3_noun hed, tal;
    u3x_cell(gon, &hed, &tal);

    if ( u3_blip != hed ) {
      _sist_suck(ovo, gon);
    }
    else {
      u3_noun vir, cor;
      u3x_cell(tal, &vir, &cor);

      u3z(u3A->roc);
      u3A->roc = u3k(cor);

      {
        u3_noun tag, dat;
        u3x_trel(ovo, 0, &tag, &dat);

        if ( c3__boot == tag ) {
          while ( u3_nul != vir ) {
            u3_noun fav = u3t(u3h(vir));

            if ( c3__init == u3h(fav) ) {
              u3A->own = u3k(u3t(fav));
              u3A->fak = ( c3__fake == u3h(tag) ) ? c3y : c3n;
            }

            vir = u3t(vir);
          }
        }
      }
    }
  }

  u3z(gon);
  u3z(ovo);
}

/* _sist_cask(): ask for a passcode.
*/
static u3_noun
_sist_cask(c3_c* dir_c, u3_noun nun)
{
  c3_c   paw_c[60];
  u3_noun key;
  u3_utty* uty_u = c3_calloc(sizeof(u3_utty));
  uty_u->fid_i = 0;

  uH;

  // disable terminal echo when typing in passcode
  if ( 0 != tcgetattr(uty_u->fid_i, &uty_u->bak_u) ) {
    c3_assert(!"init-tcgetattr");
  }
  uty_u->raw_u = uty_u->bak_u;
  uty_u->raw_u.c_lflag &= ~ECHO;
  if ( 0 != tcsetattr(uty_u->fid_i, TCSADRAIN, &uty_u->raw_u) ) {
    c3_assert(!"init-tcsetattr");
  }

  while ( 1 ) {
    printf("passcode for %s%s? ~", dir_c, (c3y == nun) ? " [none]" : "");

    paw_c[0] = 0;
    c3_fpurge(stdin);
    fgets(paw_c, 59, stdin);
    printf("\n");

    if ( '\n' == paw_c[0] ) {
      if ( c3y == nun ) {
        key = 0; break;
      }
      else {
        continue;
      }
    }
    else {
      c3_c* say_c = c3_malloc(strlen(paw_c) + 2);
      u3_noun say;

      say_c[0] = '~';
      say_c[1] = 0;
      strncat(say_c, paw_c, strlen(paw_c) - 1);

      say = u3do("slay", u3i_string(say_c));
      free(say_c);

      if ( (u3_nul == say) ||
           (u3_blip != u3h(u3t(say))) ||
           ('p' != u3h(u3t(u3t(say)))) )
      {
        printf("invalid passcode\n");
        continue;
      }
      key = u3k(u3t(u3t(u3t(say))));

      u3z(say);
      break;
    }
  }
  if ( 0 != tcsetattr(uty_u->fid_i, TCSADRAIN, &uty_u->bak_u) ) {
    c3_assert(!"init-tcsetattr");
  }
  free(uty_u);
  uL(0);
  return key;
}

//  XX unused, but may be needed for brainwallet
//
#if 0
/* _sist_text(): ask for a name string.
*/
static u3_noun
_sist_text(c3_c* pom_c)
{
  c3_c   paw_c[180];
  u3_noun say;

  uH;
  while ( 1 ) {
    printf("%s: ", pom_c);

    paw_c[0] = 0;
    c3_fpurge(stdin);
    fgets(paw_c, 179, stdin);

    if ( '\n' == paw_c[0] ) {
      continue;
    }
    else {
      c3_w len_w = strlen(paw_c);

      if ( paw_c[len_w - 1] == '\n' ) {
        paw_c[len_w-1] = 0;
      }
      say = u3i_string(paw_c);
      break;
    }
  }
  uL(0);
  return say;
}

/* _sist_bask(): ask a yes or no question.
*/
static u3_noun
_sist_bask(c3_c* pop_c, u3_noun may)
{
  u3_noun yam;

  uH;
  while ( 1 ) {
    c3_c ans_c[3];

    printf("%s [y/n]? ", pop_c);
    ans_c[0] = 0;

    c3_fpurge(stdin);
    fgets(ans_c, 2, stdin);

    if ( (ans_c[0] != 'y') && (ans_c[0] != 'n') ) {
      continue;
    } else {
      yam = (ans_c[0] != 'n') ? c3y : c3n;
      break;
    }
  }
  uL(0);
  return yam;
}
#endif

/* u3_sist_rand(): fill a 512-bit (16-word) buffer.
*/
void
u3_sist_rand(c3_w* rad_w)
{
#if defined(U3_OS_bsd) && defined(__OpenBSD__)
  if (-1 == getentropy(rad_w, 64)) {
    c3_assert(!"lo_rand");
  }
#else
  c3_i fid_i = open(DEVRANDOM, O_RDONLY);

  if ( 64 != read(fid_i, (c3_y*) rad_w, 64) ) {
    c3_assert(!"lo_rand");
  }
  close(fid_i);
#endif
}

/* _sist_fast(): offer to save passcode by mug in home directory.
*/
static void
_sist_fast(u3_noun pas, c3_l key_l)
{
  c3_c    ful_c[2048];
  c3_c*   hom_c = u3_Host.dir_c;
  u3_noun gum   = u3dc("scot", 'p', key_l);
  c3_c*   gum_c = u3r_string(gum);
  u3_noun yek   = u3dc("scot", 'p', pas);
  c3_c*   yek_c = u3r_string(yek);

  printf("saving passcode in %s/.urb/code.%s\r\n", hom_c, gum_c);
  printf("(for real security, write it down and delete the file...)\r\n");
  {
    c3_i fid_i;

    snprintf(ful_c, 2048, "%s/.urb/code.%s", hom_c, gum_c);
    if ( (fid_i = open(ful_c, O_CREAT | O_TRUNC | O_WRONLY, 0600)) < 0 ) {
      uL(fprintf(uH, "fast: could not save %s\n", ful_c));
      u3_lo_bail();
    }
    write(fid_i, yek_c, strlen(yek_c));
    close(fid_i);
  }
  free(gum_c);
  u3z(gum);

  free(yek_c);
  u3z(yek);
}

/* _sist_staf(): try to load passcode by mug from home directory.
*/
static u3_noun
_sist_staf(c3_l key_l)
{
  c3_c    ful_c[2048];
  c3_c*   hom_c = u3_Host.dir_c;
  u3_noun gum   = u3dc("scot", 'p', key_l);
  c3_c*   gum_c = u3r_string(gum);
  u3_noun txt;

  snprintf(ful_c, 2048, "%s/.urb/code.%s", hom_c, gum_c);
  free(gum_c);
  u3z(gum);
  txt = u3_walk_safe(ful_c);

  if ( 0 == txt ) {
    uL(fprintf(uH, "staf: no passcode %s\n", ful_c));
    return 0;
  }
  else {
    // c3_c* txt_c = u3r_string(txt);
    u3_noun say = u3do("slay", txt);
    u3_noun pas;


    if ( (u3_nul == say) ||
         (u3_blip != u3h(u3t(say))) ||
         ('p' != u3h(u3t(u3t(say)))) )
    {
      uL(fprintf(uH, "staf: %s is corrupt\n", ful_c));
      u3z(say);
      return 0;
    }
    uL(fprintf(uH, "loaded passcode from %s\n", ful_c));
    pas = u3k(u3t(u3t(u3t(say))));

    u3z(say);
    return pas;
  }
}

/* _sist_fatt(): stretch a 64-bit passcode to make a 128-bit key.
*/
static u3_noun
_sist_fatt(c3_l sal_l, u3_noun pas)
{
  c3_w i_w;
  u3_noun key = pas;

  //  XX use scrypt() - this is a stupid iterated hash
  //
  for ( i_w = 0; i_w < 32768; i_w++ ) {
    key = u3dc("shaf", sal_l, key);
  }
  return key;
}

/* _sist_zest(): create a new, empty record.
*/
static void
_sist_zest()
{
  struct stat buf_b;
  c3_i        fid_i;
  c3_c        ful_c[8193];
  c3_l        sal_l;

  //  Create the record file.
  {
    c3_i pig_i = O_CREAT | O_WRONLY | O_EXCL;
#ifdef O_DSYNC
    pig_i |= O_DSYNC;
#endif
    snprintf(ful_c, 2048, "%s/.urb/egz.hope", u3_Host.dir_c);

    if ( ((fid_i = open(ful_c, pig_i, 0600)) < 0) ||
         (fstat(fid_i, &buf_b) < 0) )
    {
      uL(fprintf(uH, "can't create record (%s)\n", ful_c));
      u3_lo_bail();
    }
#ifdef F_NOCACHE
    if ( -1 == fcntl(fid_i, F_NOCACHE, 1) ) {
      uL(fprintf(uH, "zest: can't uncache %s: %s\n", ful_c, strerror(errno)));
      u3_lo_bail();
    }
#endif
    u3Z->lug_u.fid_i = fid_i;
  }

  //  Generate a 31-bit salt and 64-bit passcode.
  //
  {
    u3_noun pas;
    c3_w    rad_w[16];

    c3_rand(rad_w);
    sal_l = (0x7fffffff & rad_w[0]);
    pas = u3i_words(2, rad_w + 1);

    u3A->key = _sist_fatt(sal_l, u3k(pas));
    _sist_fast(pas, u3r_mug(u3A->key));
  }

  //  Write the header.
  {
    u3_uled led_u;

    led_u.mag_l = u3r_mug('g');
    led_u.kno_w = 163;

    if ( 0 == u3A->key ) {
      led_u.key_l = 0;
    } else {
      led_u.key_l = u3r_mug(u3A->key);

      c3_assert(!(led_u.key_l >> 31));
    }
    led_u.sal_l = sal_l;
    led_u.sev_l = u3A->sev_l;
    led_u.tno_l = 1;

    if ( sizeof(led_u) != write(fid_i, &led_u, sizeof(led_u)) ) {
      uL(fprintf(uH, "can't write record (%s)\n", ful_c));
      u3_lo_bail();
    }

    u3Z->lug_u.len_d = c3_wiseof(led_u);
  }

  //  Work through the boot events.
  //
  u3_raft_play();
}

/* _sist_rest_nuu(): upgrade log from previous format.
*/
static void
_sist_rest_nuu(u3_ulog* lug_u, u3_uled led_u, c3_c* old_c)
{
  c3_c    nuu_c[2048];
  u3_noun roe = u3_nul;
  c3_i    fid_i = lug_u->fid_i;
  c3_i    fud_i;
  c3_i    ret_i;
  c3_d    end_d = lug_u->len_d;

  uL(fprintf(uH, "rest: converting log from prior format\n"));

  c3_assert(led_u.mag_l == u3r_mug('f'));

  if ( -1 == lseek64(fid_i, 4ULL * end_d, SEEK_SET) ) {
    uL(fprintf(uH, "rest_nuu failed (a), lseek64: %s\n", strerror(errno)));
    u3_lo_bail();
  }

  while ( end_d != c3_wiseof(u3_uled) ) {
    c3_d    tar_d;
    u3_olar lar_u;
    c3_w*   img_w;
    u3_noun ron;

    tar_d = (end_d - (c3_d)c3_wiseof(u3_olar));

    if ( -1 == lseek64(fid_i, 4ULL * tar_d, SEEK_SET) ) {
      uL(fprintf(uH, "rest_nuu failed (b), lseek64: %s\n", strerror(errno)));
      u3_lo_bail();
    }
    if ( sizeof(u3_olar) != read(fid_i, &lar_u, sizeof(u3_olar)) ) {
      uL(fprintf(uH, "rest_nuu failed (c), read: %s\n", strerror(errno)));
      u3_lo_bail();
    }

    if ( lar_u.syn_w != u3r_mug_d(tar_d) ) {
      uL(fprintf(uH, "rest_nuu failed (d)\n"));
      u3_lo_bail();
    }

    img_w = c3_malloc(4 * lar_u.len_w);
    end_d = (tar_d - (c3_d)lar_u.len_w);

    if ( -1 == lseek64(fid_i, 4ULL * end_d, SEEK_SET) ) {
      uL(fprintf(uH, "rest_nuu failed (e), lseek64: %s\n", strerror(errno)));
      u3_lo_bail();
    }
    if ( (4 * lar_u.len_w) != read(fid_i, img_w, (4 * lar_u.len_w)) ) {
      uL(fprintf(uH, "rest_nuu failed (f), read: %s\n", strerror(errno)));
      u3_lo_bail();
    }

    ron = u3i_words(lar_u.len_w, img_w);
    free(img_w);

    if ( lar_u.mug_w != u3r_mug(ron) ) {
      uL(fprintf(uH, "rest_nuu failed (g)\n"));
      u3_lo_bail();
    }

    roe = u3nc(ron, roe);
  }

  if ( 0 != close(fid_i) ) {
    uL(fprintf(uH, "rest: could not close, close: %s\n", strerror(errno)));
    u3_lo_bail();
  }

  ret_i = snprintf(nuu_c, 2048, "%s/.urb/ham.hope", u3_Host.dir_c);
  c3_assert(ret_i < 2048);

  if ( (fud_i = open(nuu_c, O_CREAT | O_TRUNC | O_RDWR, 0600)) < 0 ) {
    uL(fprintf(uH, "rest: can't open record (%s), open: %s\n", nuu_c,
                   strerror(errno)));
    u3_lo_bail();
  }

  led_u.mag_l = u3r_mug('g');
  if ( (sizeof(led_u) != write(fud_i, &led_u, sizeof(led_u))) ) {
    uL(fprintf(uH, "rest: can't write header, write: %s\n", strerror(errno)));
    u3_lo_bail();
  }

  {
    c3_d ent_d = 1;

    c3_assert(end_d == c3_wiseof(u3_uled));
    while ( u3_nul != roe ) {
      u3_noun ovo = u3k(u3h(roe));
      u3_noun nex = u3k(u3t(roe));
      u3_ular lar_u;
      c3_w*   img_w;
      c3_d    tar_d;

      lar_u.len_w = u3r_met(5, ovo);
      tar_d = end_d + lar_u.len_w;
      lar_u.syn_w = u3r_mug(tar_d);
      lar_u.ent_d = ent_d;
      lar_u.tem_w = 0;
      lar_u.typ_w = c3__ov;
      lar_u.mug_w = u3r_mug_both(u3r_mug(ovo),
                                   u3r_mug_both(u3r_mug(0),
                                                  u3r_mug(c3__ov)));

      img_w = c3_malloc(lar_u.len_w << 2);
      u3r_words(0, lar_u.len_w, img_w, ovo);
      u3z(ovo);

      if ( (lar_u.len_w << 2) != write(fud_i, img_w, lar_u.len_w << 2) ) {
        uL(fprintf(uH, "rest_nuu failed (h), write: %s\n", strerror(errno)));
        u3_lo_bail();
      }
      if ( sizeof(u3_ular) != write(fud_i, &lar_u, sizeof(u3_ular)) ) {
        uL(fprintf(uH, "rest_nuu failed (i), write: %s\n", strerror(errno)));
        u3_lo_bail();
      }

      ent_d++;
      end_d = tar_d + c3_wiseof(u3_ular);
      u3z(roe); roe = nex;
    }
  }
  if ( 0 != rename(nuu_c, old_c) ) {
    uL(fprintf(uH, "rest_nuu failed (k), rename: %s\n", strerror(errno)));
    u3_lo_bail();
  }
  if ( -1 == lseek64(fud_i, sizeof(u3_uled), SEEK_SET) ) {
    uL(fprintf(uH, "rest_nuu failed (l), lseek64: %s\n", strerror(errno)));
    u3_lo_bail();
  }
  lug_u->fid_i = fud_i;
  lug_u->len_d = end_d;
}

/* _sist_rest(): restore from record, or exit.
*/
static void
_sist_rest()
{
  struct stat buf_b;
  c3_i        fid_i;
  c3_c        ful_c[2048];
  c3_d        old_d = u3A->ent_d;
  c3_d        las_d = 0;
  u3_noun     roe = u3_nul;
  u3_noun     sev_l, key_l, sal_l;
  u3_noun     ohh = c3n;

  if ( 0 != u3A->ent_d ) {
    u3_noun ent;
    c3_c*   ent_c;

    ent = u3i_chubs(1, &u3A->ent_d);
    ent = u3dc("scot", c3__ud, ent);
    ent_c = u3r_string(ent);
    uL(fprintf(uH, "rest: checkpoint to event %s\n", ent_c));
    free(ent_c);
    u3z(ent);
  }

  //  Open the fscking file.  Does it even exist?
  {
    c3_i pig_i = O_RDWR;
#ifdef O_DSYNC
    pig_i |= O_DSYNC;
#endif
    snprintf(ful_c, 2048, "%s/.urb/egz.hope", u3_Host.dir_c);
    if ( ((fid_i = open(ful_c, pig_i)) < 0) || (fstat(fid_i, &buf_b) < 0) ) {
      uL(fprintf(uH, "rest: can't open record (%s)\n", ful_c));
      u3_lo_bail();

      return;
    }
#ifdef F_NOCACHE
    if ( -1 == fcntl(fid_i, F_NOCACHE, 1) ) {
      uL(fprintf(uH, "rest: can't uncache %s: %s\n", ful_c, strerror(errno)));
      u3_lo_bail();

      return;
    }
#endif
    u3Z->lug_u.fid_i = fid_i;
    u3Z->lug_u.len_d = ((buf_b.st_size + 3ULL) >> 2ULL);
  }

  //  Check the fscking header.  It's probably corrupt.
  {
    u3_uled led_u;

    if ( sizeof(led_u) != read(fid_i, &led_u, sizeof(led_u)) ) {
      uL(fprintf(uH, "record (%s) is corrupt (a)\n", ful_c));
      u3_lo_bail();
    }

    if ( u3r_mug('f') == led_u.mag_l ) {
      _sist_rest_nuu(&u3Z->lug_u, led_u, ful_c);
      fid_i = u3Z->lug_u.fid_i;
    }
    else if (u3r_mug('g') != led_u.mag_l ) {
      uL(fprintf(uH, "record (%s) is obsolete (or corrupt)\n", ful_c));
      u3_lo_bail();
    }

    if ( led_u.kno_w != 163 ) {
      //  XX perhaps we should actually do something here
      //
      uL(fprintf(uH, "rest: (not) translating events (old %d, now %d)\n",
                     led_u.kno_w,
                     163));
    }
    sev_l = led_u.sev_l;
    sal_l = led_u.sal_l;
    key_l = led_u.key_l;

    {
      u3_noun old = u3dc("scot", c3__uv, sev_l);
      u3_noun nuu = u3dc("scot", c3__uv, u3A->sev_l);
      c3_c* old_c = u3r_string(old);
      c3_c* nuu_c = u3r_string(nuu);

      uL(fprintf(uH, "rest: old %s, new %s\n", old_c, nuu_c));
      free(old_c); free(nuu_c);

      u3z(old); u3z(nuu);
    }
    c3_assert(sev_l != u3A->sev_l);   //  1 in 2 billion, just retry
  }

  //  Oh, and let's hope you didn't forget the fscking passcode.
  {
    if ( 0 != key_l ) {
      u3_noun pas = _sist_staf(key_l);
      u3_noun key;

      while ( 1 ) {
        pas = pas ? pas : _sist_cask(u3_Host.dir_c, c3n);

        key = _sist_fatt(sal_l, pas);

        if ( u3r_mug(key) != key_l ) {
          uL(fprintf(uH, "incorrect passcode\n"));
          u3z(key);
          pas = 0;
        }
        else {
          u3z(u3A->key);
          u3A->key = key;
          break;
        }
      }
    }
  }

  //  Read in the fscking events.  These are probably corrupt as well.
  {
    c3_d    ent_d;
    c3_d    end_d;
    u3_noun rup = c3n;

    end_d = u3Z->lug_u.len_d;
    ent_d = 0;

    if ( -1 == lseek64(fid_i, 4ULL * end_d, SEEK_SET) ) {
      uL(fprintf(uH, "end_d %" PRIu64 ", lseek64: %s\n", end_d,
                     strerror(errno)));
      uL(fprintf(uH, "record (%s) is corrupt (c)\n", ful_c));
      u3_lo_bail();
    }

    while ( end_d != c3_wiseof(u3_uled) ) {
      c3_d    tar_d = (end_d - (c3_d)c3_wiseof(u3_ular));
      u3_ular lar_u;
      c3_w*   img_w;
      u3_noun ron;

      // uL(fprintf(uH, "rest: reading event at %" PRIx64 "\n", end_d));

      if ( -1 == lseek64(fid_i, 4ULL * tar_d, SEEK_SET) ) {
        uL(fprintf(uH, "record (%s) is corrupt (d)\n", ful_c));
        u3_lo_bail();
      }
      if ( sizeof(u3_ular) != read(fid_i, &lar_u, sizeof(u3_ular)) ) {
        uL(fprintf(uH, "record (%s) is corrupt (e)\n", ful_c));
        u3_lo_bail();
      }

      if ( lar_u.syn_w != u3r_mug_d(tar_d) ) {
        if ( c3n == rup ) {
          uL(fprintf(uH, "corruption detected; attempting to fix\n"));
          rup = c3y;
        }
        uL(fprintf(uH, "lar:%x mug:%x\n", lar_u.syn_w, u3r_mug_d(tar_d)));
        end_d--; u3Z->lug_u.len_d--;
        continue;
      }
      else if ( c3y == rup ) {
        uL(fprintf(uH, "matched at %x\n", lar_u.syn_w));
        rup = c3n;
      }

      if ( lar_u.ent_d == 0 ) {
        ohh = c3y;
      }

#if 0
      uL(fprintf(uH, "log: read: at %d, %d: lar ent %" PRIu64 ", len %d, mug %x\n",
                      (tar_w - lar_u.len_w),
                      tar_w,
                      lar_u.ent_d,
                      lar_u.len_w,
                      lar_u.mug_w));
#endif
      if ( end_d == u3Z->lug_u.len_d ) {
        ent_d = las_d = lar_u.ent_d;
      }
      else {
        if ( lar_u.ent_d != (ent_d - 1ULL) ) {
          uL(fprintf(uH, "record (%s) is corrupt (g)\n", ful_c));
          uL(fprintf(uH, "lar_u.ent_d %" PRIx64 ", ent_d %" PRIx64 "\n", lar_u.ent_d, ent_d));
          u3_lo_bail();
        }
        ent_d -= 1ULL;
      }
      end_d = (tar_d - (c3_d)lar_u.len_w);

      if ( ent_d < old_d ) {
        /*  change to continue to check all events  */
        break;
      }

      img_w = c3_malloc(4 * lar_u.len_w);

      if ( -1 == lseek64(fid_i, 4ULL * end_d, SEEK_SET) ) {
        uL(fprintf(uH, "record (%s) is corrupt (h)\n", ful_c));
        u3_lo_bail();
      }
      if ( (4 * lar_u.len_w) != read(fid_i, img_w, (4 * lar_u.len_w)) ) {
        uL(fprintf(uH, "record (%s) is corrupt (i)\n", ful_c));
        u3_lo_bail();
      }

      ron = u3i_words(lar_u.len_w, img_w);
      free(img_w);

      if ( lar_u.mug_w !=
            u3r_mug_both(u3r_mug(ron),
                           u3r_mug_both(u3r_mug(lar_u.tem_w),
                                          u3r_mug(lar_u.typ_w))) )
      {
        uL(fprintf(uH, "record (%s) is corrupt (j)\n", ful_c));
        u3_lo_bail();
      }

      if ( c3__ov != lar_u.typ_w ) {
        u3z(ron);
        continue;
      }

      if ( u3A->key ) {
        u3_noun dep;

        dep = u3dc("de:crub:crypto", u3k(u3A->key), ron);
        if ( c3n == u3du(dep) ) {
          uL(fprintf(uH, "record (%s) is corrupt (k)\n", ful_c));
          u3_lo_bail();
        }
        else {
          ron = u3k(u3t(dep));
          u3z(dep);
        }
      }

      roe = u3nc(u3ke_cue(ron), roe);
    }
    u3A->ent_d = c3_max(las_d + 1ULL, old_d);
  }

  if ( u3_nul == roe ) {
    //  Nothing in the log that was not also in the checkpoint.
    //
    c3_assert(u3A->ent_d == old_d);
    if ( las_d + 1 != old_d ) {
      uL(fprintf(uH, "checkpoint and log disagree! las:%" PRIu64 " old:%" PRIu64 "\n",
                     las_d + 1, old_d));
      uL(fprintf(uH, "Some events appear to be missing from the log.\n"
                     "Please contact the authorities, "
                     "and do not delete your pier!\n"));
      u3_lo_bail();
    }
  }
  else {
    u3_noun rou = roe;
    c3_w    xno_w;

    //  Execute the fscking things.  This is pretty much certain to crash.
    //
    uL(fprintf(uH, "rest: replaying through event %" PRIu64 "\n", las_d));
    fprintf(uH, "---------------- playback starting----------------\n");

    xno_w = 0;
    while ( u3_nul != roe ) {
      u3_noun i_roe = u3h(roe);
      u3_noun t_roe = u3t(roe);
      u3_noun now = u3h(i_roe);
      u3_noun ovo = u3t(i_roe);

      u3v_time(u3k(now));
      if ( (c3y == u3_Host.ops_u.vno) &&
           ( (c3__veer == u3h(u3t(ovo)) ||
             (c3__vega == u3h(u3t(ovo)))) ) )
      {
        fprintf(stderr, "replay: skipped veer\n");
      }
      else {
        _sist_sing(u3k(ovo));
        fputc('.', stderr);
      }

      // fprintf(stderr, "playback: sing: %d\n", xno_w));

      roe = t_roe;
      xno_w++;

      if ( 0 == (xno_w % 1000) ) {
        uL(fprintf(uH, "{%d}\n", xno_w));
        // u3_lo_grab("rest", rou, u3_none);
      }
    }
    u3z(rou);
  }
  uL(fprintf(stderr, "\n---------------- playback complete----------------\r\n"));

#if 0
  //  If you see this error, your record is totally fscking broken!
  //  Which probably serves you right.  Please consult a consultant.
  {
    if ( u3_nul == u3A->own ) {
      uL(fprintf(uH, "record did not install a master!\n"));
      u3_lo_bail();
    }
    u3A->our = u3k(u3h(u3A->own));
    u3A->pod = u3dc("scot", 'p', u3k(u3A->our)));
  }

  //  Now, who the fsck are you?  No, really.
  {
    u3_noun who;
    c3_c*   fil_c;
    c3_c*   who_c;

    if ( (fil_c = strrchr(u3_Host.dir_c, '/')) ) {
      fil_c++;
    } else fil_c = u3_Host.dir_c;

    who = u3dc("scot", 'p', u3k(u3A->our)));
    who_c = u3r_string(who);
    u3z(who);

    if ( strncmp(fil_c, who_c + 1, strlen(fil_c)) ) {
      uL(fprintf(uH, "record master (%s) does not match filename!\n", who_c));
      u3_lo_bail();
    }
    free(who_c);
  }
#endif

  //  Increment sequence numbers. New logs start at 1.
  if ( c3y == ohh ) {
    uL(fprintf(uH, "rest: bumping ent_d\n"));
    u3_ular lar_u;
    c3_d    end_d;
    c3_d    tar_d;

    u3A->ent_d++;
    end_d = u3Z->lug_u.len_d;
    while ( end_d != c3_wiseof(u3_uled) ) {
      tar_d = end_d - c3_wiseof(u3_ular);
      if ( -1 == lseek64(fid_i, 4ULL * tar_d, SEEK_SET) ) {
        uL(fprintf(uH, "bumping sequence numbers failed (a)\n"));
        u3_lo_bail();
      }
      if ( sizeof(lar_u) != read(fid_i, &lar_u, sizeof(lar_u)) ) {
        uL(fprintf(uH, "bumping sequence numbers failed (b)\n"));
        u3_lo_bail();
      }
      lar_u.ent_d++;
      if ( -1 == lseek64(fid_i, 4ULL * tar_d, SEEK_SET) ) {
        uL(fprintf(uH, "bumping sequence numbers failed (c)\n"));
        u3_lo_bail();
      }
      if ( sizeof(lar_u) != write(fid_i, &lar_u, sizeof(lar_u)) ) {
        uL(fprintf(uH, "bumping sequence numbers failed (d)\n"));
        u3_lo_bail();
      }
      end_d = tar_d - lar_u.len_w;
    }
  }

  //  Rewrite the header.  Will probably corrupt the record.
  {
    u3_uled led_u;

    led_u.mag_l = u3r_mug('g');
    led_u.sal_l = sal_l;
    led_u.sev_l = u3A->sev_l;
    led_u.key_l = u3A->key ? u3r_mug(u3A->key) : 0;
    led_u.kno_w = 163;         //  may need actual translation!
    led_u.tno_l = 1;

    if ( (-1 == lseek64(fid_i, 0, SEEK_SET)) ||
         (sizeof(led_u) != write(fid_i, &led_u, sizeof(led_u))) )
    {
      uL(fprintf(uH, "record (%s) failed to rewrite\n", ful_c));
      u3_lo_bail();
    }
  }

  //  Hey, fscker!  It worked.
  {
    u3_term_ef_boil();
  }
}

/* sist_key(): parse a private key-file.
*/
static u3_noun
sist_key(u3_noun des)
{
  u3_noun sed, who;

  u3_noun eds = u3dc("slaw", c3__uw, u3k(des));

  if ( u3_nul == eds ) {
    c3_c* sed_c = u3r_string(des);
    fprintf(stderr, "dawn: invalid private keys: %s\r\n", sed_c);
    free(sed_c);
    u3_lo_bail();
  }

  if ( 0 == u3_Host.ops_u.who_c ) {
    fprintf(stderr, "dawn: -w required\r\n");
    u3_lo_bail();
  }

  u3_noun woh = u3i_string(u3_Host.ops_u.who_c);
  u3_noun whu = u3dc("slaw", 'p', u3k(woh));

  if ( u3_nul == whu ) {
    fprintf(stderr, "dawn: invalid ship specificed with -w %s\r\n",
                                               u3_Host.ops_u.who_c);
    u3_lo_bail();
  }

  // +seed:able:jael: private key file
  sed = u3ke_cue(u3k(u3t(eds)));
  who = u3h(sed);

  if ( c3n == u3r_sing(who, u3t(whu)) ) {
    u3_noun how = u3dc("scot", 'p', u3k(who));
    c3_c* how_c = u3r_string(u3k(how));
    fprintf(stderr, "dawn: mismatch between -w %s and -K %s\r\n",
                                               u3_Host.ops_u.who_c, how_c);

    u3z(how);
    free(how_c);
    u3_lo_bail();
  }

  u3z(woh); u3z(whu); u3z(des); u3z(eds);

  return sed;
}

/* u3_sist_boot(): restore or create.
*/
void
u3_sist_boot(void)
{
  //  iterate entropy
  //
  {
    c3_w    eny_w[16];
    u3_noun eny;

    c3_rand(eny_w);
    eny = u3i_words(16, eny_w);

    u3v_plan(u3nt(u3_blip, c3__arvo, u3_nul), u3nc(c3__wack, u3k(eny)));
    u3z(eny);
  }

  if ( c3n == u3_Host.ops_u.nuu ) {
    _sist_rest();

    if ( c3y == u3A->fak ) {
      c3_c* who_c = u3r_string(u3dc("scot", 'p', u3k(u3A->own)));
      fprintf(stderr, "fake: %s\r\n", who_c);
      free(who_c);

      // XX review persistent options

      // disable networking
      u3_Host.ops_u.net = c3n;
      // disable battery hashes
      u3_Host.ops_u.has = c3y;
      u3C.wag_w |= u3o_hashless;
    }

    //  process pending events
    //
    u3_raft_play();
  }
  else {
    u3_noun pig, who;

    if ( 0 != u3_Host.ops_u.fak_c ) {
      u3_noun whu = u3dc("slaw", 'p', u3i_string(u3_Host.ops_u.fak_c));

      if ( (u3_nul == whu) ) {
        fprintf(stderr, "fake: invalid ship: %s\r\n", u3_Host.ops_u.fak_c);
        u3_lo_bail();
      }
      else {
        u3_noun rac = u3do("clan:title", u3k(u3t(whu)));

        if ( c3__pawn == rac ) {
          fprintf(stderr, "fake comets are disallowed\r\n");
          u3_lo_bail();
        }

        u3z(rac);
      }

      fprintf(stderr, "fake: %s\r\n", u3_Host.ops_u.fak_c);

      u3A->fak = c3y;
      who = u3k(u3t(whu));
      pig = u3nc(c3__fake, u3k(who));

      u3z(whu);
    }
    else {
      u3_noun sed;

      if ( 0 != u3_Host.ops_u.key_c ) {
        u3_noun des = u3m_file(u3_Host.ops_u.key_c);
        sed = sist_key(des);
      }
      else if ( 0 != u3_Host.ops_u.gen_c ) {
        u3_noun des = u3i_string(u3_Host.ops_u.gen_c);
        sed = sist_key(des);
      }
      else {
        sed = u3_dawn_come(u3_nul);
      }

      u3A->fak = c3n;
      pig = u3_dawn_vent(u3k(sed));
      who = u3k(u3h(u3h(u3t(pig))));

      u3z(sed);
    }

    u3A->own = who;

    //  set single-home
    //
    u3v_plan(u3nt(u3_blip, c3__arvo, u3_nul), u3nc(c3__whom, u3k(who)));

    // initialize ames
    {
      u3_noun tuf = (c3y == u3A->fak) ? u3_nul : u3h(u3t(u3t(u3t(u3t(pig)))));
      // with a fake event to bring up listeners and configure domains
      u3_ames_ef_turf(u3k(tuf));
      // and real effect to set the output duct
      u3_ames_ef_bake();
    }

    // initialize %behn
    u3_behn_ef_bake();

    // Authenticate and initialize terminal.
    u3_term_ef_bake(pig);

    // queue initial filesystem sync
    //
    // from the Arvo directory if specified
    if ( 0 != u3_Host.ops_u.arv_c ) {
      u3_unix_ef_initial_into();
    }
    // otherwise from the pill
    else {
      c3_c ful_c[2048];

      snprintf(ful_c, 2048, "%s/.urb/urbit.pill", u3_Host.dir_c);

      {
        u3_noun sys = u3ke_cue(u3m_file(ful_c));
        u3_noun fil;

        u3x_trel(sys, 0, 0, &fil);
        u3v_plow(u3k(fil));

        u3z(sys);
      }
    }

    // Create the event log
    _sist_zest();
  }
}
