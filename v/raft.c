/* v/raft.c
**
** This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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


/* _raft_election_rand(): pseudorandom component of election timeout.
*/
static c3_w
_raft_election_rand()
{
  return ((float) rand() / RAND_MAX) * 150;
}

/* _raft_readname(): parse a raft host:port peer name.
*/
static u2_bean
_raft_readname(u2_ropt* rop_u, const c3_c* str_c, c3_w siz_w)
{
  u2_rnam* nam_u = malloc(sizeof(*nam_u));
  c3_c*    col_c;
  c3_w     por_w;
  c3_w     nam_w;

  nam_u->str_c = malloc(siz_w + 1);
  strncpy(nam_u->str_c, str_c, siz_w);
  nam_u->str_c[siz_w] = '\0';
  //fprintf(stderr, "raft: peer %s\n", nam_u->str_c);

  if ( 0 == (col_c = strchr(nam_u->str_c, ':')) ) {
    fprintf(stderr, "raft: invalid name %s\n", str_c);
    return u2_no;
  }
  else {
    nam_w = col_c - nam_u->str_c;
    nam_u->nam_c = malloc(nam_w + 1);
    strncpy(nam_u->nam_c, nam_u->str_c, nam_w);
    nam_u->nam_c[nam_w] = '\0';

    por_w = atol(col_c + 1);
    if ( !(por_w > 0 && por_w < 65536) ) {
      fprintf(stderr, "raft: invalid port '%s'\n", col_c + 1);
      return u2_no;
    }
    else nam_u->por_s = por_w;
    //fprintf(stderr, "raft: peer %s:%d\n", nam_u->nam_c, nam_u->por_s);

    nam_u->nex_u = rop_u->nam_u;
    rop_u->nam_u = nam_u;
    return u2_yes;
  }
}

u2_bean
u2_raft_readopt(u2_ropt* rop_u, const c3_c* arg_c)
{
  c3_c* com_c;

  while ( 0 != (com_c = strchr(arg_c, ',')) ) {
    if ( u2_no == _raft_readname(rop_u, arg_c, com_c - arg_c) ) {
      return u2_no;
    } else arg_c = com_c + 1;
  }
  return _raft_readname(rop_u, arg_c, strlen(arg_c));
}

static void
_raft_listen_cb(uv_stream_t* wax_u, c3_i sas_i)
{
}

static void
_raft_time_cb(uv_timer_t* tim_u, c3_i sas_i)
{
  u2_raft* raf_u = tim_u->data;
  //uL(fprintf(uH, "raft: time\n"));
}

/* _raft_foll_init(): begin, follower mode.
*/
static void
_raft_foll_init(u2_raft* raf_u)
{
  uL(fprintf(uH, "raft: starting follower\n"));

  if ( 0 != uv_tcp_init(u2L, &raf_u->wax_u) ) {
    uL(fprintf(uH, "raft: init: %s\n", uv_strerror(uv_last_error(u2L))));
    c3_assert(0);
  }

  // Bind the listener.
  {
    struct sockaddr_in add_u;

    memset(&add_u, 0, sizeof(add_u));
    add_u.sin_family = AF_INET;
    add_u.sin_addr.s_addr = htonl(INADDR_ANY);
    add_u.sin_port = htons(u2_Host.ops_u.rop_u.por_s);

    if ( 0 != uv_tcp_bind(&raf_u->wax_u, add_u) ) {
      uL(fprintf(uH, "raft: bind: %s\n", uv_strerror(uv_last_error(u2L))));
      c3_assert(0);
    }
    else {
      if ( 0 != uv_listen((uv_stream_t*)&raf_u->wax_u, 16, _raft_listen_cb) ) {
        uL(fprintf(uH, "raft: listen: %s\n", uv_strerror(uv_last_error(u2L))));
        c3_assert(0);
      }
      else {
        uL(fprintf(uH, "raft: on TCP %d\n", u2_Host.ops_u.rop_u.por_s));
      }
    }
  }

  // Start the initial election timeout.
  {
    uv_timer_init(u2L, &raf_u->tim_u);
    raf_u->tim_u.data = raf_u;
    uv_timer_start(&raf_u->tim_u, _raft_time_cb, _raft_election_rand(), 0);
  }
}

/* _raft_lone_init(): begin, single-instance mode.
*/
static void
_raft_lone_init(u2_raft* raf_u)
{
  uL(fprintf(uH, "raft: single-instance mode\n"));
}

void
u2_raft_io_init()
{
  u2_raft* raf_u = u2R;

  if ( 0 == u2_Host.ops_u.rop_u.por_s ) {
    _raft_lone_init(raf_u);
  }
  else {
    _raft_foll_init(raf_u);
  }
}

c3_w
u2_raft_push(u2_raft* raf_u, c3_w* bob_w, c3_w len_w)
{
  uL(fprintf(uH, "raft: pushing\n"));
  return 0;
}

/* _raft_pack(): write a blob to disk, retaining.
*/
static void
_raft_pack(u2_reck* rec_u, c3_w* bob_w, c3_w len_w)
{
  u2_ulog* lug_u = &u2R->lug_u;
  c3_d     tar_d;
  u2_ular  lar_u;

  tar_d = lug_u->len_d + len_w;

  lar_u.syn_w = u2_cr_mug((c3_w)tar_d);
  lar_u.mug_w = u2_cr_mug_words(bob_w, len_w);
  //lar_u.tem_w = raf_u->tem_w;
  lar_u.ent_w = rec_u->ent_w;   //  XX is this right?
  lar_u.len_w = len_w;

  if ( -1 == lseek64(lug_u->fid_i, 4ULL * tar_d, SEEK_SET) ) {
    perror("lseek");
    uL(fprintf(uH, "raft_pack: seek failed\n"));
    c3_assert(0);
  }
  if ( sizeof(lar_u) != write(lug_u->fid_i, &lar_u, sizeof(lar_u)) ) {
    perror("write");
    uL(fprintf(uH, "raft_pack: write failed\n"));
    c3_assert(0);
  }
  if ( -1 == lseek64(lug_u->fid_i, 4ULL * lug_u->len_d, SEEK_SET) ) {
    perror("lseek");
    uL(fprintf(uH, "raft_pack: seek failed\n"));
    c3_assert(0);
  }
#if 1
  uL(fprintf(uH, "raft_pack: write %ull, %ull: lar ent %d, len %d, mug %x\n",
                 lug_u->len_d,
                 tar_d,
                 lar_u.ent_w,
                 lar_u.len_w,
                 lar_u.mug_w));
#endif
  if ( (4 * len_w) != write(lug_u->fid_i, bob_w, (4 * len_w)) ) {
    perror("write");
    uL(fprintf(uH, "raft_pack: write failed\n"));
    c3_assert(0);
  }
  lug_u->len_d += (c3_d)(lar_u.len_w + c3_wiseof(lar_u));
}


/* _lo_home(): create ship directory.
*/
static void
_lo_home(u2_reck* rec_u)
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

/* _lo_cask(): ask for a passcode.
*/
static u2_noun
_lo_cask(u2_reck* rec_u, c3_c* dir_c, u2_bean nun)
{
  c3_c   paw_c[60];
  u2_noun key;

  uH;
  while ( 1 ) {
    printf("passcode for %s%s? ~", dir_c, (u2_yes == nun) ? " [none]" : "");

    paw_c[0] = 0;
    fpurge(stdin);
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
      c3_c* say_c = malloc(strlen(paw_c) + 2);
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

/* _lo_text(): ask for a name string.
*/
static u2_noun
_lo_text(u2_reck* rec_u, c3_c* pom_c)
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
/* _lo_bask(): ask a yes or no question.
*/
static u2_bean
_lo_bask(c3_c* pop_c, u2_bean may)
{
  u2_bean yam;

  uH;
  while ( 1 ) {
    c3_c ans_c[3];

    printf("%s [y/n]? ", pop_c);
    ans_c[0] = 0;

    fpurge(stdin);
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

/* _lo_rand(): fill a 256-bit (8-word) buffer.
*/
static void
_lo_rand(u2_reck* rec_u, c3_w* rad_w)
{
  c3_i fid_i = open(DEVRANDOM, O_RDONLY);

  if ( 32 != read(fid_i, (c3_y*) rad_w, 32) ) {
    c3_assert(!"lo_rand");
  }
  close(fid_i);
}

/* _lo_fast(): offer to save passcode by mug in home directory.
*/
static void
_lo_fast(u2_reck* rec_u, u2_noun pas, c3_l key_l)
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

/* _lo_staf(): try to load passcode by mug from home directory.
*/
static u2_noun
_lo_staf(u2_reck* rec_u, c3_l key_l)
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

/* _lo_fatt(): stretch a 64-bit passcode to make a 128-bit key.
*/
static u2_noun
_lo_fatt(c3_l sal_l, u2_noun pas)
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

/* _lo_zest(): create a new, empty record.
*/
static void
_lo_zest(u2_reck* rec_u)
{
  struct stat buf_b;
  c3_i        fid_i;
  c3_c        ful_c[8193];
  c3_l        sal_l;

  //  Create the ship directory.
  //
  _lo_home(rec_u);

  //  Create the record file.
  {
    snprintf(ful_c, 2048, "%s/egz.hope", u2_Host.cpu_c);

    if ( ((fid_i = open(ful_c, O_CREAT | O_WRONLY | O_EXCL, 0600)) < 0) ||
         (fstat(fid_i, &buf_b) < 0) )
    {
      uL(fprintf(uH, "can't create record (%s)\n", ful_c));
      u2_lo_bail(rec_u);
    }
    u2_Host.lug_u.fid_i = fid_i;
  }

  //  Generate a 31-bit salt.
  //
  {
    c3_w rad_w[8];

    _lo_rand(rec_u, rad_w);
    sal_l = (0x7fffffff & rad_w[0]);
  }

  //  Create and save a passcode.
  //
  {
    c3_w rad_w[8];
    u2_noun pas;

    _lo_rand(rec_u, rad_w);
    pas = u2_ci_words(2, rad_w);

    rec_u->key = _lo_fatt(sal_l, u2k(pas));
    _lo_fast(rec_u, pas, u2_mug(rec_u->key));
  }

  //  Write the header.
  {
    u2_uled led_u;

    led_u.mag_l = u2_mug('f');
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

    u2_Host.lug_u.len_d = c3_wiseof(led_u);
  }

  //  Save the boot events.
  {
    u2_noun nor = u2_ckb_flop(rec_u->roe);

    rec_u->roe = nor;

    while ( u2_nul != nor ) {
      _lo_pack(rec_u, u2k(u2h(nor)));
      rec_u->ent_w += 1;
      nor = u2t(nor);
    }
    u2z(rec_u->roe);
    rec_u->roe = 0;
  }

#if 0
  //  Copy the egz into ham, the factory default.
  {
    snprintf(ful_c, 8193, "rm -f %s/~ham.hope; cp %s/~egz.hope %s/~ham.hope",
                   u2_Host.cpu_c, u2_Host.cpu_c, u2_Host.cpu_c);

    if ( 0 != system(ful_c) ) {
      uL(fprintf(uH, "zest: could not save ham\n"));
      u2_lo_bail(rec_u);
    }
  }
#endif
}

/* _lo_rest(): restore from record, or exit.
*/
static void
_lo_rest(u2_reck* rec_u)
{
  struct stat buf_b;
  c3_i        fid_i;
  c3_c        ful_c[2048];
  c3_w        old_w = rec_u->ent_w;
  c3_w        las_w = 0;
  u2_noun     roe = u2_nul;
  u2_noun     sev_l, tno_l, key_l, sal_l;

  if ( 0 != rec_u->ent_w ) {
    u2_noun ent = u2_dc("scot", c3__ud, rec_u->ent_w);
    c3_c* ent_c = u2_cr_string(ent);
    uL(fprintf(uH, "rest: checkpoint to event %s\n", ent_c));
    free(ent_c);
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
    u2_Host.lug_u.fid_i = fid_i;
    u2_Host.lug_u.len_d = ((buf_b.st_size + 3ULL) >> 2ULL);
  }

  //  Check the fscking header.  It's probably corrupt.
  {
    u2_uled led_u;

    if ( sizeof(led_u) != read(fid_i, &led_u, sizeof(led_u)) ) {
      uL(fprintf(uH, "record (%s) is corrupt (a)\n", ful_c));
      u2_lo_bail(rec_u);
    }

    if ( u2_mug('f') != led_u.mag_l ) {
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
      u2_noun pas = _lo_staf(rec_u, key_l);
      u2_noun key;

      while ( 1 ) {
        pas = pas ? pas : _lo_cask(rec_u, u2_Host.cpu_c, u2_no);

        key = _lo_fatt(sal_l, pas);

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
    c3_w ent_w;
    c3_d end_d;

    end_d = u2_Host.lug_u.len_d;
    ent_w = 0;

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

#if 0
      uL(fprintf(uH, "log: read: at %d, %d: lar ent %d, len %d, mug %x\n",
                      (tar_w - lar_u.len_w),
                      tar_w,
                      lar_u.ent_w,
                      lar_u.len_w,
                      lar_u.mug_w));
#endif
      img_w = malloc(4 * lar_u.len_w);

      if ( end_d == u2_Host.lug_u.len_d ) {
        ent_w = las_w = lar_u.ent_w;
      }
      else {
        if ( lar_u.ent_w != (ent_w - 1) ) {
          uL(fprintf(uH, "record (%s) is corrupt (g)\n", ful_c));
          uL(fprintf(uH, "lar_u.ent_w %x, ent_w %x\n", lar_u.ent_w, ent_w));
          u2_lo_bail(rec_u);
        }
        ent_w -= 1;
      }
      end_d = (tar_d - (c3_d)lar_u.len_w);

      if ( ent_w < old_w ) {
        free(img_w);
        break;
      }

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

      if ( u2_mug(ron) != lar_u.mug_w ) {
        uL(fprintf(uH, "record (%s) is corrupt (j)\n", ful_c));
        u2_lo_bail(rec_u);
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
    rec_u->ent_w = c3_max(las_w + 1, old_w);
  }

  if ( u2_nul == roe ) {
    //  Nothing in the log that was not also in the checkpoint.
    //
    c3_assert(rec_u->ent_w == old_w);
    c3_assert((las_w + 1) == old_w);
  }
  else {
    u2_noun rou = roe;
    c3_w    xno_w;

    //  Execute the fscking things.  This is pretty much certain to crash.
    //
    uL(fprintf(uH, "rest: replaying through event %d\n", las_w));
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
        uL(fprintf(uH, "replay: skipped veer\n"));
      }
      else {
        _lo_sing(rec_u, u2k(ovo));
      }

      fputc('.', stderr);
      // uL(fprintf(uH, "playback: sing: %d\n", xno_w));

      roe = t_roe;
      xno_w++;

      if ( 0 == (xno_w % 1000) ) {
        uL(fprintf(uH, "{%d}\n", xno_w));
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

  //  Rewrite the header.  Will probably corrupt the record.
  {
    u2_uled led_u;

    led_u.mag_l = u2_mug('f');
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

/* _lo_zen(): get OS entropy.
*/
static u2_noun
_lo_zen(u2_reck* rec_u)
{
  c3_w rad_w[8];

  _lo_rand(rec_u, rad_w);
  return u2_ci_words(8, rad_w);
}
