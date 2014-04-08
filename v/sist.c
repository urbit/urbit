/* v/sist.c
**
** This file is in the public domain.
*/
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/errno.h>
#include <unistd.h>
#include <uv.h>

#include "all.h"
#include "v/vere.h"

#if defined(U2_OS_linux)
#include <stdio_ext.h>
#define fpurge(fd) __fpurge(fd)
#define DEVRANDOM "/dev/urandom"
#else
#define DEVRANDOM "/dev/random"
#endif


/* u2_sist_pack(): write a blob to disk, transferring.
*/
c3_d
u2_sist_pack(u2_reck* rec_u, c3_w tem_w, c3_w typ_w, c3_w* bob_w, c3_w len_w)
{
  u2_ulog* lug_u = &u2R->lug_u;
  c3_d     tar_d;
  u2_ular  lar_u;

  tar_d = lug_u->len_d + len_w;

  lar_u.tem_w = tem_w;
  lar_u.typ_w = typ_w;
  lar_u.syn_w = u2_cr_mug((c3_w)tar_d);
  lar_u.mug_w = u2_cr_mug_both(u2_cr_mug_words(bob_w, len_w),
                               u2_cr_mug_both(u2_cr_mug(lar_u.tem_w),
                                              u2_cr_mug(lar_u.typ_w)));
  lar_u.ent_d = rec_u->ent_d;
  rec_u->ent_d++;
  lar_u.len_w = len_w;

  if ( -1 == lseek64(lug_u->fid_i, 4ULL * tar_d, SEEK_SET) ) {
    perror("lseek");
    uL(fprintf(uH, "sist_pack: seek failed\n"));
    c3_assert(0);
  }
  if ( sizeof(lar_u) != write(lug_u->fid_i, &lar_u, sizeof(lar_u)) ) {
    perror("write");
    uL(fprintf(uH, "sist_pack: write failed\n"));
    c3_assert(0);
  }
  if ( -1 == lseek64(lug_u->fid_i, 4ULL * lug_u->len_d, SEEK_SET) ) {
    perror("lseek");
    uL(fprintf(uH, "sist_pack: seek failed\n"));
    c3_assert(0);
  }
#if 0
  uL(fprintf(uH, "sist_pack: write %llu, %llu: lar ent %llu, len %d, mug %x\n",
                 lug_u->len_d,
                 tar_d,
                 lar_u.ent_d,
                 lar_u.len_w,
                 lar_u.mug_w));
#endif
  if ( (4 * len_w) != write(lug_u->fid_i, bob_w, (4 * len_w)) ) {
    perror("write");
    uL(fprintf(uH, "sist_pack: write failed\n"));
    c3_assert(0);
  }
  lug_u->len_d += (c3_d)(lar_u.len_w + c3_wiseof(lar_u));

  free(bob_w);

  return rec_u->ent_d;
}

/* u2_sist_put(): moronic key-value store put.
*/
void
u2_sist_put(const c3_c* key_c, const c3_y* val_y, size_t siz_i)
{
  c3_c ful_c[2048];
  c3_i ret_i;
  c3_i fid_i;

  ret_i = snprintf(ful_c, 2048, "%s/sis/_%s", u2_Host.cpu_c, key_c);
  c3_assert(ret_i < 2048);

  if ( (fid_i = open(ful_c, O_CREAT | O_TRUNC | O_WRONLY, 0600)) < 0 ) {
    uL(fprintf(uH, "sist: could not put %s\n", key_c));
    perror("open");
    u2_lo_bail(u2A);
  }
  if ( (ret_i = write(fid_i, val_y, siz_i)) != siz_i ) {
    uL(fprintf(uH, "sist: could not write %s\n", key_c));
    if ( ret_i < 0 ) {
      perror("write");
    }
    u2_lo_bail(u2A);
  }
  ret_i = c3_sync(fid_i);
  if ( ret_i < 0 ) {
    perror("sync");
  }
  ret_i = close(fid_i);
  c3_assert(0 == ret_i);
}

/* u2_sist_has(): moronic key-value store existence check.
*/
ssize_t
u2_sist_has(const c3_c* key_c)
{
  c3_c        ful_c[2048];
  c3_i        ret_i;
  struct stat sat_u;

  ret_i = snprintf(ful_c, 2048, "%s/sis/_%s", u2_Host.cpu_c, key_c);
  c3_assert(ret_i < 2048);

  if ( (ret_i = stat(ful_c, &sat_u)) < 0 ) {
    if ( errno == ENOENT ) {
      return -1;
    }
    else {
      uL(fprintf(uH, "sist: could not stat %s\n", key_c));
      perror("stat");
      u2_lo_bail(u2A);
    }
  }
  else {
    return sat_u.st_size;
  }
  c3_assert(!"not reached");
}

/* u2_sist_get(): moronic key-value store get.
*/
void
u2_sist_get(const c3_c* key_c, c3_y* val_y)
{
  c3_c        ful_c[2048];
  c3_i        ret_i;
  c3_i        fid_i;
  struct stat sat_u;

  ret_i = snprintf(ful_c, 2048, "%s/sis/_%s", u2_Host.cpu_c, key_c);
  c3_assert(ret_i < 2048);

  if ( (fid_i = open(ful_c, O_RDONLY)) < 0 ) {
    uL(fprintf(uH, "sist: could not get %s\n", key_c));
    perror("open");
    u2_lo_bail(u2A);
  }
  if ( (ret_i = fstat(fid_i, &sat_u)) < 0 ) {
    uL(fprintf(uH, "sist: could not stat %s\n", key_c));
    perror("fstat");
    u2_lo_bail(u2A);
  }
  if ( (ret_i = read(fid_i, val_y, sat_u.st_size)) != sat_u.st_size ) {
    uL(fprintf(uH, "sist: could not read %s\n", key_c));
    if ( ret_i < 0 ) {
      perror("read");
    }
    u2_lo_bail(u2A);
  }
  ret_i = close(fid_i);
  c3_assert(0 == ret_i);
}

/* u2_sist_nil(): moronic key-value store rm.
*/
void
u2_sist_nil(const c3_c* key_c)
{
  c3_c ful_c[2048];
  c3_i ret_i;

  ret_i = snprintf(ful_c, 2048, "%s/sis/_%s", u2_Host.cpu_c, key_c);
  c3_assert(ret_i < 2048);

  if ( (ret_i = unlink(ful_c)) < 0 ) {
    if ( errno == ENOENT ) {
      return;
    }
    else {
      uL(fprintf(uH, "sist: could not unlink %s\n", key_c));
      perror("unlink");
      u2_lo_bail(u2A);
    }
  }
}

/* _sist_suck(): past failure.
*/
static void
_sist_suck(u2_reck* rec_u, u2_noun ovo, u2_noun gon)
{
  uL(fprintf(uH, "sing: ovum failed!\n"));
  {
    c3_c* hed_c = u2_cr_string(u2h(u2t(ovo)));

    uL(fprintf(uH, "fail %s\n", hed_c));
    free(hed_c);
  }

  u2_lo_punt(2, u2_ckb_flop(u2k(u2t(gon))));
  u2_loom_exit();
  u2_lo_exit();

  exit(1);
}

/* _sist_sing(): replay ovum from the past, time already set.
*/
static void
_sist_sing(u2_reck* rec_u, u2_noun ovo)
{
  u2_noun gon = u2_lo_soft(rec_u, 0, u2_reck_poke, u2k(ovo));

  if ( u2_blip != u2h(gon) ) {
    _sist_suck(rec_u, ovo, gon);
  }
  else {
    u2_noun vir = u2k(u2h(u2t(gon)));
    u2_noun cor = u2k(u2t(u2t(gon)));
    u2_noun nug;

    u2z(gon);
    nug = u2_reck_nick(rec_u, vir, cor);

    if ( u2_blip != u2h(nug) ) {
      _sist_suck(rec_u, ovo, nug);
    }
    else {
      vir = u2h(u2t(nug));
      cor = u2k(u2t(u2t(nug)));

      while ( u2_nul != vir ) {
        u2_noun fex = u2h(vir);
        u2_noun fav = u2t(fex);

        if ( (c3__init == u2h(fav)) || (c3__inuk == u2h(fav)) ) {
          rec_u->own = u2nc(u2k(u2t(fav)), rec_u->own);
        }
        vir = u2t(vir);
      }
      u2z(nug);
      u2z(rec_u->roc);
      rec_u->roc = cor;
    }
    u2z(ovo);
  }
}


/* _sist_home(): create ship directory.
*/
static void
_sist_home(u2_reck* rec_u)
{
  c3_c    ful_c[2048];

  //  Create subdirectories.
  //
  {
    mkdir(u2_Host.cpu_c, 0700);

    snprintf(ful_c, 2048, "%s/get", u2_Host.cpu_c);
    if ( 0 != mkdir(ful_c, 0700) ) {
      perror(ful_c);
      u2_lo_bail(rec_u);
    }

    snprintf(ful_c, 2048, "%s/put", u2_Host.cpu_c);
    if ( 0 != mkdir(ful_c, 0700) ) {
      perror(ful_c);
      u2_lo_bail(rec_u);
    }

    snprintf(ful_c, 2048, "%s/sis", u2_Host.cpu_c);
    if ( 0 != mkdir(ful_c, 0700) ) {
      perror(ful_c);
      u2_lo_bail(rec_u);
    }
  }

  //  Copy urbit.pill.
  //
  {
    snprintf(ful_c, 2048, "cp %s/urbit.pill %s",
                    u2_Host.ops_u.hom_c, u2_Host.cpu_c);
    if ( 0 != system(ful_c) ) {
      uL(fprintf(uH, "could not %s\n", ful_c));
      u2_lo_bail(rec_u);
    }
  }
}

/* _sist_cask(): ask for a passcode.
*/
static u2_noun
_sist_cask(u2_reck* rec_u, c3_c* dir_c, u2_bean nun)
{
  c3_c   paw_c[60];
  u2_noun key;

  uH;
  while ( 1 ) {
    printf("passcode for %s%s? ~", dir_c, (u2_yes == nun) ? " [none]" : "");

    paw_c[0] = 0;
    c3_fpurge(stdin);
    fgets(paw_c, 59, stdin);

    if ( '\n' == paw_c[0] ) {
      if ( u2_yes == nun ) {
        key = 0; break;
      }
      else {
        continue;
      }
    }
    else {
      c3_c* say_c = c3_malloc(strlen(paw_c) + 2);
      u2_noun say;

      say_c[0] = '~';
      say_c[1] = 0;
      strncat(say_c, paw_c, strlen(paw_c) - 1);

      say = u2_do("slay", u2_ci_string(say_c));
      if ( (u2_nul == say) ||
           (u2_blip != u2h(u2t(say))) ||
           ('p' != u2h(u2t(u2t(say)))) )
      {
        printf("invalid passcode\n");
        continue;
      }
      key = u2k(u2t(u2t(u2t(say))));

      u2z(say);
      break;
    }
  }
  uL(0);
  return key;
}

/* _sist_text(): ask for a name string.
*/
static u2_noun
_sist_text(u2_reck* rec_u, c3_c* pom_c)
{
  c3_c   paw_c[60];
  u2_noun say;

  uH;
  while ( 1 ) {
    printf("%s: ", pom_c);

    paw_c[0] = 0;
    fpurge(stdin);
    fgets(paw_c, 59, stdin);

    if ( '\n' == paw_c[0] ) {
      continue;
    }
    else {
      c3_w len_w = strlen(paw_c);

      if ( paw_c[len_w - 1] == '\n' ) {
        paw_c[len_w-1] = 0;
      }
      say = u2_ci_string(paw_c);
      break;
    }
  }
  uL(0);
  return say;
}

#if 0
/* _sist_bask(): ask a yes or no question.
*/
static u2_bean
_sist_bask(c3_c* pop_c, u2_bean may)
{
  u2_bean yam;

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
      yam = (ans_c[0] != 'n') ? u2_yes : u2_no;
      break;
    }
  }
  uL(0);
  return yam;
}
#endif

/* _sist_rand(): fill a 256-bit (8-word) buffer.
*/
static void
_sist_rand(u2_reck* rec_u, c3_w* rad_w)
{
  c3_i fid_i = open(DEVRANDOM, O_RDONLY);

  if ( 32 != read(fid_i, (c3_y*) rad_w, 32) ) {
    c3_assert(!"lo_rand");
  }
  close(fid_i);
}

/* _sist_fast(): offer to save passcode by mug in home directory.
*/
static void
_sist_fast(u2_reck* rec_u, u2_noun pas, c3_l key_l)
{
  c3_c    ful_c[2048];
  c3_c*   hom_c = getenv("HOME");
  u2_noun gum   = u2_dc("scot", 'p', key_l);
  c3_c*   gum_c = u2_cr_string(gum);
  u2_noun yek   = u2_dc("scot", 'p', pas);
  c3_c*   yek_c = u2_cr_string(yek);

  printf("saving passcode in %s/.urbit/%s.txt\r\n", hom_c, gum_c);
  printf("(for real security, write it down and delete the file...)\r\n");
  {
    c3_i fid_i;

    snprintf(ful_c, 2048, "%s/.urbit", hom_c);
    mkdir(ful_c, 0700);

    snprintf(ful_c, 2048, "%s/.urbit/%s.txt", hom_c, gum_c);
    if ( (fid_i = open(ful_c, O_CREAT | O_TRUNC | O_WRONLY, 0600)) < 0 ) {
      uL(fprintf(uH, "fast: could not save %s\n", ful_c));
      u2_lo_bail(rec_u);
    }
    write(fid_i, yek_c, strlen(yek_c));
    close(fid_i);
  }
  free(gum_c);
  u2z(gum);

  free(yek_c);
  u2z(yek);
}

/* _sist_staf(): try to load passcode by mug from home directory.
*/
static u2_noun
_sist_staf(u2_reck* rec_u, c3_l key_l)
{
  c3_c    ful_c[2048];
  c3_c*   hom_c = getenv("HOME");
  u2_noun gum   = u2_dc("scot", 'p', key_l);
  c3_c*   gum_c = u2_cr_string(gum);
  u2_noun txt;

  snprintf(ful_c, 2048, "%s/.urbit/%s.txt", hom_c, gum_c);
  free(gum_c);
  u2z(gum);
  txt = u2_walk_safe(ful_c);

  if ( 0 == txt ) {
    uL(fprintf(uH, "staf: no passcode %s\n", ful_c));
    return 0;
  }
  else {
    // c3_c* txt_c = u2_cr_string(txt);
    u2_noun say = u2_do("slay", txt);
    u2_noun pas;


    if ( (u2_nul == say) ||
         (u2_blip != u2h(u2t(say))) ||
         ('p' != u2h(u2t(u2t(say)))) )
    {
      uL(fprintf(uH, "staf: %s is corrupt\n", ful_c));
      u2z(say);
      return 0;
    }
    uL(fprintf(uH, "loaded passcode from %s\n", ful_c));
    pas = u2k(u2t(u2t(u2t(say))));

    u2z(say);
    return pas;
  }
}

/* _sist_fatt(): stretch a 64-bit passcode to make a 128-bit key.
*/
static u2_noun
_sist_fatt(c3_l sal_l, u2_noun pas)
{
  c3_w i_w;
  u2_noun key = pas;

  //  XX use scrypt() - this is a stupid iterated hash
  //
  for ( i_w = 0; i_w < 32768; i_w++ ) {
    key = u2_dc("shaf", sal_l, key);
  }
  return key;
}

/* _sist_zest(): create a new, empty record.
*/
static void
_sist_zest(u2_reck* rec_u)
{
  struct stat buf_b;
  c3_i        fid_i;
  c3_c        ful_c[8193];
  c3_l        sal_l;

  //  Create the ship directory.
  //
  _sist_home(rec_u);

  //  Create the record file.
  {
    snprintf(ful_c, 2048, "%s/egz.hope", u2_Host.cpu_c);

    if ( ((fid_i = open(ful_c, O_CREAT | O_WRONLY | O_EXCL, 0600)) < 0) ||
         (fstat(fid_i, &buf_b) < 0) )
    {
      uL(fprintf(uH, "can't create record (%s)\n", ful_c));
      u2_lo_bail(rec_u);
    }
    u2R->lug_u.fid_i = fid_i;
  }

  //  Generate a 31-bit salt.
  //
  {
    c3_w rad_w[8];

    _sist_rand(rec_u, rad_w);
    sal_l = (0x7fffffff & rad_w[0]);
  }

  //  Create and save a passcode.
  //
  {
    c3_w rad_w[8];
    u2_noun pas;

    _sist_rand(rec_u, rad_w);
    pas = u2_ci_words(2, rad_w);

    rec_u->key = _sist_fatt(sal_l, u2k(pas));
    _sist_fast(rec_u, pas, u2_mug(rec_u->key));
  }

  //  Write the header.
  {
    u2_uled led_u;

    led_u.mag_l = u2_mug('g');
    led_u.kno_w = rec_u->kno_w;

    if ( 0 == rec_u->key ) {
      led_u.key_l = 0;
    } else {
      led_u.key_l = u2_mug(rec_u->key);

      c3_assert(!(led_u.key_l >> 31));
    }
    led_u.sal_l = sal_l;
    led_u.sev_l = rec_u->sev_l;
    led_u.tno_l = 1;

    if ( sizeof(led_u) != write(fid_i, &led_u, sizeof(led_u)) ) {
      uL(fprintf(uH, "can't write record (%s)\n", ful_c));
      u2_lo_bail(rec_u);
    }

    u2R->lug_u.len_d = c3_wiseof(led_u);
  }

  //  Work through the boot events.
  u2_raft_work(rec_u);
}

/* _sist_make(): boot from scratch.
*/
static void
_sist_make(u2_reck* rec_u, u2_noun fav)
{
  //  Authenticate and initialize terminal.
  //
  u2_term_ef_bake(fav);

  //  Create the ship directory.
  //
  _sist_zest(rec_u);
}

/* _sist_rest_nuu(): upgrade log from previous format.
*/
static void
_sist_rest_nuu(u2_ulog* lug_u, u2_uled led_u, c3_c* old_c)
{
  c3_c    nuu_c[2048];
  u2_noun roe = u2_nul;
  c3_i    fid_i = lug_u->fid_i;
  c3_i    fud_i;
  c3_i    ret_i;
  c3_d    end_d = lug_u->len_d;

  uL(fprintf(uH, "rest: converting log from prior format\n"));

  c3_assert(led_u.mag_l == u2_mug('f'));

  if ( -1 == lseek64(fid_i, 4ULL * end_d, SEEK_SET) ) {
    uL(fprintf(uH, "rest_nuu failed (a)\n"));
    perror("lseek64");
    u2_lo_bail(u2A);
  }

  while ( end_d != c3_wiseof(u2_uled) ) {
    c3_d    tar_d;
    u2_olar lar_u;
    c3_w*   img_w;
    u2_noun ron;

    tar_d = (end_d - (c3_d)c3_wiseof(u2_olar));

    if ( -1 == lseek64(fid_i, 4ULL * tar_d, SEEK_SET) ) {
      uL(fprintf(uH, "rest_nuu failed (b)\n"));
      perror("lseek64");
      u2_lo_bail(u2A);
    }
    if ( sizeof(u2_olar) != read(fid_i, &lar_u, sizeof(u2_olar)) ) {
      uL(fprintf(uH, "rest_nuu failed (c)\n"));
      perror("read");
      u2_lo_bail(u2A);
    }

    if ( lar_u.syn_w != u2_mug((c3_w)tar_d) ) {
      uL(fprintf(uH, "rest_nuu failed (d)\n"));
      u2_lo_bail(u2A);
    }

    img_w = c3_malloc(4 * lar_u.len_w);
    end_d = (tar_d - (c3_d)lar_u.len_w);

    if ( -1 == lseek64(fid_i, 4ULL * end_d, SEEK_SET) ) {
      uL(fprintf(uH, "rest_nuu failed (e)\n"));
      perror("lseek64");
      u2_lo_bail(u2A);
    }
    if ( (4 * lar_u.len_w) != read(fid_i, img_w, (4 * lar_u.len_w)) ) {
      uL(fprintf(uH, "rest_nuu failed (f)\n"));
      perror("read");
      u2_lo_bail(u2A);
    }

    ron = u2_ci_words(lar_u.len_w, img_w);
    free(img_w);

    if ( lar_u.mug_w != u2_cr_mug(ron) ) {
      uL(fprintf(uH, "rest_nuu failed (g)\n"));
      u2_lo_bail(u2A);
    }

    roe = u2nc(ron, roe);
  }

  if ( 0 != close(fid_i) ) {
    uL(fprintf(uH, "rest: could not close\n"));
    perror("close");
    u2_lo_bail(u2A);
  }

  ret_i = snprintf(nuu_c, 2048, "%s/ham.hope", u2_Host.cpu_c);
  c3_assert(ret_i < 2048);

  if ( (fud_i = open(nuu_c, O_CREAT | O_TRUNC | O_RDWR, 0600)) < 0 ) {
    uL(fprintf(uH, "rest: can't open record (%s)\n", nuu_c));
    perror("open");
    u2_lo_bail(u2A);
  }

  led_u.mag_l = u2_mug('g');
  if ( (sizeof(led_u) != write(fud_i, &led_u, sizeof(led_u))) ) {
    uL(fprintf(uH, "rest: can't write header\n"));
    perror("write");
    u2_lo_bail(u2A);
  }

  {
    c3_d ent_d = 1;

    c3_assert(end_d == c3_wiseof(u2_uled));
    while ( u2_nul != roe ) {
      u2_noun ovo = u2k(u2h(roe));
      u2_noun nex = u2k(u2t(roe));
      u2_ular lar_u;
      c3_w*   img_w;
      c3_d    tar_d;

      lar_u.len_w = u2_cr_met(5, ovo);
      tar_d = end_d + lar_u.len_w;
      lar_u.syn_w = u2_cr_mug(tar_d);
      lar_u.ent_d = ent_d;
      lar_u.tem_w = 0;
      lar_u.typ_w = c3__ov;
      lar_u.mug_w = u2_cr_mug_both(u2_cr_mug(ovo),
                                   u2_cr_mug_both(u2_cr_mug(0),
                                                  u2_cr_mug(c3__ov)));

      img_w = c3_malloc(lar_u.len_w << 2);
      u2_cr_words(0, lar_u.len_w, img_w, ovo);
      u2z(ovo);

      if ( (lar_u.len_w << 2) != write(fud_i, img_w, lar_u.len_w << 2) ) {
        uL(fprintf(uH, "rest_nuu failed (h)\n"));
        perror("write");
        u2_lo_bail(u2A);
      }
      if ( sizeof(u2_ular) != write(fud_i, &lar_u, sizeof(u2_ular)) ) {
        uL(fprintf(uH, "rest_nuu failed (i)\n"));
        perror("write");
        u2_lo_bail(u2A);
      }

      ent_d++;
      end_d = tar_d + c3_wiseof(u2_ular);
      u2z(roe); roe = nex;
    }
  }
  if ( 0 != rename(nuu_c, old_c) ) {
    uL(fprintf(uH, "rest_nuu failed (k)\n"));
    perror("rename");
    u2_lo_bail(u2A);
  }
  if ( -1 == lseek64(fud_i, sizeof(u2_uled), SEEK_SET) ) {
    uL(fprintf(uH, "rest_nuu failed (l)\n"));
    perror("lseek64");
    u2_lo_bail(u2A);
  }
  lug_u->fid_i = fud_i;
  lug_u->len_d = end_d;
}

/* _sist_rest(): restore from record, or exit.
*/
static void
_sist_rest(u2_reck* rec_u)
{
  struct stat buf_b;
  c3_i        fid_i;
  c3_c        ful_c[2048];
  c3_d        old_d = rec_u->ent_d;
  c3_d        las_d = 0;
  u2_noun     roe = u2_nul;
  u2_noun     sev_l, tno_l, key_l, sal_l;
  u2_bean     ohh = u2_no;

  //  XX delete me -- needed for piers created on old code.
  {
    snprintf(ful_c, 2048, "%s/sis", u2_Host.cpu_c);

    if ( 0 != mkdir(ful_c, 0700) ) {
      if ( errno != EEXIST ) {
        perror(ful_c);
        u2_lo_bail(rec_u);
      }
    }
  }

  if ( 0 != rec_u->ent_d ) {
    u2_noun ent;
    c3_c*   ent_c;

    ent = u2_ci_chubs(1, &rec_u->ent_d);
    ent = u2_dc("scot", c3__ud, ent);
    ent_c = u2_cr_string(ent);
    uL(fprintf(uH, "rest: checkpoint to event %s\n", ent_c));
    free(ent_c);
    u2z(ent);
  }

  //  Open the fscking file.  Does it even exist?
  {
    snprintf(ful_c, 2048, "%s/egz.hope", u2_Host.cpu_c);

    if ( ((fid_i = open(ful_c, O_RDWR)) < 0) ||
         (fstat(fid_i, &buf_b) < 0) )
    {
      uL(fprintf(uH, "rest: can't open record (%s)\n", ful_c));
      u2_lo_bail(rec_u);

      return;
    }
    u2R->lug_u.fid_i = fid_i;
    u2R->lug_u.len_d = ((buf_b.st_size + 3ULL) >> 2ULL);
  }

  //  Check the fscking header.  It's probably corrupt.
  {
    u2_uled led_u;

    if ( sizeof(led_u) != read(fid_i, &led_u, sizeof(led_u)) ) {
      uL(fprintf(uH, "record (%s) is corrupt (a)\n", ful_c));
      u2_lo_bail(rec_u);
    }

    if ( u2_mug('f') == led_u.mag_l ) {
      _sist_rest_nuu(&u2R->lug_u, led_u, ful_c);
      fid_i = u2R->lug_u.fid_i;
    }
    else if (u2_mug('g') != led_u.mag_l ) {
      uL(fprintf(uH, "record (%s) is obsolete (or corrupt)\n", ful_c));
      u2_lo_bail(rec_u);
    }

    if ( led_u.kno_w != rec_u->kno_w ) {
      //  XX perhaps we should actually do something here
      //
      uL(fprintf(uH, "rest: (not) translating events (old %d, now %d)\n",
                     led_u.kno_w,
                     rec_u->kno_w));
    }
    sev_l = led_u.sev_l;
    sal_l = led_u.sal_l;
    key_l = led_u.key_l;
    tno_l = led_u.tno_l;

    {
      u2_noun old = u2_dc("scot", c3__uv, sev_l);
      u2_noun nuu = u2_dc("scot", c3__uv, rec_u->sev_l);
      c3_c* old_c = u2_cr_string(old);
      c3_c* nuu_c = u2_cr_string(nuu);

      uL(fprintf(uH, "rest: old %s, new %s\n", old_c, nuu_c));
      free(old_c); free(nuu_c);

      u2z(old); u2z(nuu);
    }
    c3_assert(sev_l != rec_u->sev_l);   //  1 in 2 billion, just retry
  }

  //  Oh, and let's hope you didn't forget the fscking passcode.
  {
    if ( 0 != key_l ) {
      u2_noun pas = _sist_staf(rec_u, key_l);
      u2_noun key;

      while ( 1 ) {
        pas = pas ? pas : _sist_cask(rec_u, u2_Host.cpu_c, u2_no);

        key = _sist_fatt(sal_l, pas);

        if ( u2_mug(key) != key_l ) {
          uL(fprintf(uH, "incorrect passcode\n"));
          u2z(key);
          pas = 0;
        }
        else {
          u2z(rec_u->key);
          rec_u->key = key;
          break;
        }
      }
    }
  }

  //  Read in the fscking events.  These are probably corrupt as well.
  {
    c3_d ent_d;
    c3_d end_d;

    end_d = u2R->lug_u.len_d;
    ent_d = 0;

    if ( -1 == lseek64(fid_i, 4ULL * end_d, SEEK_SET) ) {
      fprintf(stderr, "end_d %llx\n", end_d);
      perror("lseek");
      uL(fprintf(uH, "record (%s) is corrupt (c)\n", ful_c));
      u2_lo_bail(rec_u);
    }

    while ( end_d != c3_wiseof(u2_uled) ) {
      c3_d    tar_d = (end_d - (c3_d)c3_wiseof(u2_ular));
      u2_ular lar_u;
      c3_w*   img_w;
      u2_noun ron;

      // uL(fprintf(uH, "rest: reading event at %llx\n", end_d));

      if ( -1 == lseek64(fid_i, 4ULL * tar_d, SEEK_SET) ) {
        uL(fprintf(uH, "record (%s) is corrupt (d)\n", ful_c));
        u2_lo_bail(rec_u);
      }
      if ( sizeof(u2_ular) != read(fid_i, &lar_u, sizeof(u2_ular)) ) {
        uL(fprintf(uH, "record (%s) is corrupt (e)\n", ful_c));
        u2_lo_bail(rec_u);
      }

      if ( lar_u.syn_w != u2_mug((c3_w)tar_d) ) {
        uL(fprintf(uH, "record (%s) is corrupt (f)\n", ful_c));
        u2_lo_bail(rec_u);
      }

      if ( lar_u.ent_d == 0 ) {
        ohh = u2_yes;
      }

#if 0
      uL(fprintf(uH, "log: read: at %d, %d: lar ent %llu, len %d, mug %x\n",
                      (tar_w - lar_u.len_w),
                      tar_w,
                      lar_u.ent_d,
                      lar_u.len_w,
                      lar_u.mug_w));
#endif
      if ( end_d == u2R->lug_u.len_d ) {
        ent_d = las_d = lar_u.ent_d;
      }
      else {
        if ( lar_u.ent_d != (ent_d - 1ULL) ) {
          uL(fprintf(uH, "record (%s) is corrupt (g)\n", ful_c));
          uL(fprintf(uH, "lar_u.ent_d %llx, ent_d %llx\n", lar_u.ent_d, ent_d));
          u2_lo_bail(rec_u);
        }
        ent_d -= 1ULL;
      }
      end_d = (tar_d - (c3_d)lar_u.len_w);

      if ( ent_d < old_d ) {
        //  XX this could be a break if we didn't want to see the sequence
        //  number of the first event.
        continue;
      }

      img_w = c3_malloc(4 * lar_u.len_w);

      if ( -1 == lseek64(fid_i, 4ULL * end_d, SEEK_SET) ) {
        uL(fprintf(uH, "record (%s) is corrupt (h)\n", ful_c));
        u2_lo_bail(rec_u);
      }
      if ( (4 * lar_u.len_w) != read(fid_i, img_w, (4 * lar_u.len_w)) ) {
        uL(fprintf(uH, "record (%s) is corrupt (i)\n", ful_c));
        u2_lo_bail(rec_u);
      }

      ron = u2_ci_words(lar_u.len_w, img_w);
      free(img_w);

      if ( lar_u.mug_w !=
            u2_cr_mug_both(u2_cr_mug(ron),
                           u2_cr_mug_both(u2_cr_mug(lar_u.tem_w),
                                          u2_cr_mug(lar_u.typ_w))) )
      {
        uL(fprintf(uH, "record (%s) is corrupt (j)\n", ful_c));
        u2_lo_bail(rec_u);
      }

      if ( c3__ov != lar_u.typ_w ) {
        u2z(ron);
        continue;
      }

      if ( rec_u->key ) {
        u2_noun dep;

        dep = u2_dc("de:crya", u2k(rec_u->key), ron);
        if ( u2_no == u2du(dep) ) {
          uL(fprintf(uH, "record (%s) is corrupt (k)\n", ful_c));
          u2_lo_bail(rec_u);
        }
        else {
          ron = u2k(u2t(dep));
          u2z(dep);
        }
      }
      roe = u2nc(u2_cke_cue(ron), roe);
    }
    rec_u->ent_d = c3_max(las_d + 1ULL, old_d);
  }

  if ( u2_nul == roe ) {
    //  Nothing in the log that was not also in the checkpoint.
    //
    c3_assert(rec_u->ent_d == old_d);
    c3_assert((las_d + 1ULL) == old_d);
  }
  else {
    u2_noun rou = roe;
    c3_w    xno_w;

    //  Execute the fscking things.  This is pretty much certain to crash.
    //
    uL(fprintf(uH, "rest: replaying through event %d\n", las_d));
    fprintf(uH, "---------------- playback starting----------------\n");

    xno_w = 0;
    while ( u2_nul != roe ) {
      u2_noun i_roe = u2h(roe);
      u2_noun t_roe = u2t(roe);
      u2_noun now = u2h(i_roe);
      u2_noun ovo = u2t(i_roe);

      u2_reck_wind(rec_u, u2k(now));
      if ( (u2_yes == u2_Host.ops_u.vno) &&
           (c3__veer == u2h(u2t(ovo))) ) {
        fprintf(stderr, "replay: skipped veer\n");
      }
      else if ( u2_yes == u2_Host.ops_u.fog &&
                u2_nul == t_roe ) {
        fprintf(stderr, "replay: -Xwtf, skipped last event\n");
      }
      else {
        _sist_sing(rec_u, u2k(ovo));
        fputc('.', stderr);
      }

      // fprintf(stderr, "playback: sing: %d\n", xno_w));

      roe = t_roe;
      xno_w++;

      if ( 0 == (xno_w % 1000) ) {
        uL(fprintf(uH, "{%d}\n", xno_w));
        u2_lo_grab("rest", rou, u2_none);
      }
    }
    u2z(rou);
  }
  uL(fprintf(stderr, "\n---------------- playback complete----------------\n"));

#if 0
  //  If you see this error, your record is totally fscking broken!
  //  Which probably serves you right.  Please consult a consultant.
  {
    if ( u2_nul == rec_u->own ) {
      uL(fprintf(uH, "record did not install a master!\n"));
      u2_lo_bail(rec_u);
    }
    rec_u->our = u2k(u2h(rec_u->own));
    rec_u->pod = u2_dc("scot", 'p', u2k(rec_u->our)));
  }

  //  Now, who the fsck are you?  No, really.
  {
    u2_noun who;
    c3_c*   fil_c;
    c3_c*   who_c;

    if ( (fil_c = strrchr(u2_Host.cpu_c, '/')) ) {
      fil_c++;
    } else fil_c = u2_Host.cpu_c;

    who = u2_dc("scot", 'p', u2k(rec_u->our)));
    who_c = u2_cr_string(who);
    u2z(who);

    if ( strncmp(fil_c, who_c + 1, strlen(fil_c)) ) {
      uL(fprintf(uH, "record master (%s) does not match filename!\n", who_c));
      u2_lo_bail(rec_u);
    }
    free(who_c);
  }
#endif

  //  Increment sequence numbers. New logs start at 1.
  if ( u2_yes == ohh ) {
    uL(fprintf(uH, "rest: bumping ent_d, don't panic.\n"));
    u2_ular lar_u;
    c3_d    end_d;
    c3_d    tar_d;

    rec_u->ent_d++;
    end_d = u2R->lug_u.len_d;
    while ( end_d != c3_wiseof(u2_uled) ) {
      tar_d = end_d - c3_wiseof(u2_ular);
      if ( -1 == lseek64(fid_i, 4ULL * tar_d, SEEK_SET) ) {
        uL(fprintf(uH, "bumping sequence numbers failed (a)\n"));
        u2_lo_bail(rec_u);
      }
      if ( sizeof(lar_u) != read(fid_i, &lar_u, sizeof(lar_u)) ) {
        uL(fprintf(uH, "bumping sequence numbers failed (b)\n"));
        u2_lo_bail(rec_u);
      }
      lar_u.ent_d++;
      if ( -1 == lseek64(fid_i, 4ULL * tar_d, SEEK_SET) ) {
        uL(fprintf(uH, "bumping sequence numbers failed (c)\n"));
        u2_lo_bail(rec_u);
      }
      if ( sizeof(lar_u) != write(fid_i, &lar_u, sizeof(lar_u)) ) {
        uL(fprintf(uH, "bumping sequence numbers failed (d)\n"));
        u2_lo_bail(rec_u);
      }
      end_d = tar_d - lar_u.len_w;
    }
  }

  //  Rewrite the header.  Will probably corrupt the record.
  {
    u2_uled led_u;

    led_u.mag_l = u2_mug('g');
    led_u.sal_l = sal_l;
    led_u.sev_l = rec_u->sev_l;
    led_u.key_l = rec_u->key ? u2_mug(rec_u->key) : 0;
    led_u.kno_w = rec_u->kno_w;         //  may need actual translation!
    led_u.tno_l = 1;

    if ( (-1 == lseek64(fid_i, 0, SEEK_SET)) ||
         (sizeof(led_u) != write(fid_i, &led_u, sizeof(led_u))) )
    {
      uL(fprintf(uH, "record (%s) failed to rewrite\n", ful_c));
      u2_lo_bail(rec_u);
    }
  }

  //  Hey, fscker!  It worked.
  {
    u2_term_ef_boil(tno_l);
  }
}

/* _sist_zen(): get OS entropy.
*/
static u2_noun
_sist_zen(u2_reck* rec_u)
{
  c3_w rad_w[8];

  _sist_rand(rec_u, rad_w);
  return u2_ci_words(8, rad_w);
}

/* u2_sist_boot(): restore or create.
*/
void
u2_sist_boot(void)
{
  uL(fprintf(uH, "sist: booting\n"));
  if ( u2_yes == u2_Host.ops_u.nuu ) {
    u2_noun pig = u2_none;

    if ( 0 == u2_Host.ops_u.imp_c ) {
      c3_c get_c[2049];
      snprintf(get_c, 2048, "%s/get", u2_Host.cpu_c);
      if ( 0 == access(get_c, 0) ) {
          uL(fprintf(uH, "pier: already built\n"));
          u2_lo_bail(u2A);
      }
      u2_noun ten = _sist_zen(u2A);
      uL(fprintf(uH, "generating 2048-bit RSA pair...\n"));

      pig = u2nq(c3__make, u2_nul, 11, ten);
    }
    else {
      u2_noun imp = u2_ci_string(u2_Host.ops_u.imp_c);
      u2_noun whu = u2_dc("slaw", 'p', u2k(imp));

      if ( (u2_nul == whu) ) {
        fprintf(stderr, "czar: incorrect format\r\n");
        u2_lo_bail(u2A);
      }
      else {
        u2_noun gen = _sist_text(u2A, "generator");
        u2_noun gun = u2_dc("slaw", c3__uw, gen);

        if ( u2_nul == gun ) {
          fprintf(stderr, "czar: incorrect format\r\n");
          u2_lo_bail(u2A);
        }
        pig = u2nt(c3__sith, u2k(u2t(whu)), u2k(u2t(gun)));

        u2z(whu); u2z(gun);
      }
      u2z(imp);
    }
    _sist_make(u2A, pig);
  }
  else {
    _sist_rest(u2A);
  }
}
