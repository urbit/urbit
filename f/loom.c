/* f/loom.c
**
** This file is in the public domain.
*/
#include "all.h"
#include <sys/uio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sigsegv.h>
#include <termios.h>
#include <uv.h>
#include "v/vere.h"

/* _loom_stop(): signal handler to stop computation.
*/
static void
_loom_stop(c3_i sig)
{
}

static c3_i
_loom_sigsegv_handler(void* adr_v, c3_i ser_i)
{
  if ( ser_i ) {
    c3_w*    bas_w = (c3_w*)(void *)(U2_OS_LoomBase);
    c3_w     off_w = (((c3_c*) adr_v) - (c3_c*)U2_OS_LoomBase) >> 2;
    c3_w     pag_w = off_w >> LoomPageWords;
    c3_w     win_w;
    u2_cheg* ceg_u;

    if ( pag_w > LoomAllPages ) {
      fprintf(stderr, "address %p out of loom!\r\n", adr_v);
      return 0;
    }
    c3_assert((u2_page_neat == LoomChem[pag_w].lif_e) ||
              (u2_page_none == LoomChem[pag_w].lif_e) );

    LoomChem[pag_w].lif_e = u2_page_tref;
    LoomChem[pag_w].mug_e = 0;

    for ( ceg_u = &LoomSegmentA; ceg_u; ceg_u = ceg_u->nex_u ) {
      if ( (pag_w >= ceg_u->bot_w) &&
           (win_w=(pag_w - ceg_u->bot_w)) < ceg_u->len_w )
      {
        if ( win_w >= ceg_u->pgs_w ) {
          ceg_u->pgs_w = win_w + 1;
        }
        break;
      }
    }
    if ( 0 == ceg_u ) {
      fprintf(stderr, "page %d is not in a segment!\n", pag_w);
      return 1;
    }

    if ( -1 == mprotect((void *)(bas_w + (pag_w << LoomPageWords)),
                        (1 << (LoomPageWords + 2)),
                        (PROT_READ | PROT_WRITE)) )
    {
      perror("mprotect");
      exit(1);
    }
    return 1;
  }
  return 0;
}

/* _loom_read(): full blocking read.
*/
static u2_bean
_loom_read(c3_i fid_i, void* buf_w, c3_w len_w)
{
  return ((4 * len_w) == read(fid_i, buf_w, (4 * len_w))) ? u2_yes : u2_no;
}

/* _loom_write(): full blocking write.
*/
static u2_bean
_loom_write(c3_i fid_i, void* buf_w, c3_w len_w)
{
  if ((4 * len_w) != write(fid_i, buf_w, (4 * len_w))) {
    fprintf(stderr, "couldn't write %d\n", fid_i);
    perror("huh?");
    exit(1);
  }
  else {
    return u2_yes;
  }
//  return ((4 * len_w) == write(fid_i, buf_w, (4 * len_w))) ? u2_yes : u2_no;
}

/* u2_loom_clip():
**
**   Clip top and bottom.
*/
void
u2_loom_clip(c3_w bot_w, c3_w top_w)
{
}

/* _loom_deploy(): load and attempt to validate.
*/
static u2_bean
_loom_deploy(void)
{
  u2_cheg* ceg_u;
  c3_w     num_w = 0;

  for ( ceg_u = &LoomSegmentA; ceg_u; ceg_u = ceg_u->nex_u ) {
    u2_chef chf_u;
    c3_w i_w;

    /* Load control segment.
    */
    {
      if ( u2_no == _loom_read(ceg_u->ctl_i, &chf_u, c3_wiseof(chf_u)) ) {
        printf("deploy no a\n");
        return u2_no;
      }
      c3_assert(chf_u.bot_w == ceg_u->bot_w);

      if ( u2_no == _loom_read(ceg_u->ctl_i,
                               (u2_chit*) &LoomChem[ceg_u->bot_w],
                               chf_u.pgs_w) )
      {
        printf("deploy no b\n");
        return u2_no;
      }
      ceg_u->pgs_w = chf_u.pgs_w;
    }

    /* Load data segment.
    */
    {
      c3_w  liv_w = chf_u.pgs_w;
      c3_w  res_w = (ceg_u->len_w - liv_w);
      c3_w* vil_w = ((c3_w*)U2_OS_LoomBase) + (ceg_u->bot_w << LoomPageWords);
      c3_w* den_w = (vil_w + (liv_w << LoomPageWords));

      if ( liv_w ) {
        if ( -1 == (c3_ps)mmap(vil_w,
                               (liv_w << (LoomPageWords + 2)),
                               PROT_READ,
                               (MAP_FIXED | MAP_PRIVATE),
                               ceg_u->dat_i, 0) )
        {
          printf("deploy no c\n");
          return u2_no;
        }
      }
      if ( res_w ) {
        if ( -1 == (c3_ps)mmap(den_w,
                               res_w << (LoomPageWords + 2),
                               PROT_READ,
                               (MAP_ANON | MAP_FIXED | MAP_PRIVATE),
                               -1, 0) )
        {
          printf("deploy no d\n");
          return u2_no;
        }
      }
    }

    /* Validate with little checksum.
    */
    for ( i_w = 0; i_w < chf_u.pgs_w; i_w++ ) {
      c3_w  pag_w = (i_w + ceg_u->bot_w);
      c3_w* gaw_w = ((c3_w*)U2_OS_LoomBase) + (pag_w << LoomPageWords);

      if ( u2_page_none == LoomChem[pag_w].lif_e ) {
        continue;
      }
      num_w++;

      if ( u2_page_neat != LoomChem[pag_w].lif_e ) {
        printf("deploy no e\n");
        return u2_no;
      }
      if ( (0x3fffffff & u2_mug_words(gaw_w, (1 << LoomPageWords))) !=
           LoomChem[pag_w].mug_e )
      {
        printf("mismatched mug at page %d\n", pag_w);
        printf("actual data: %x\n",
            (0x3fffffff & u2_mug_words(gaw_w, (1 << LoomPageWords))));
        printf("control mug: %x\n", LoomChem[pag_w].mug_e);

        printf("deploy no f\n");
        return u2_no;
      }
    }
#if 0
    fprintf(stderr, "load: %s bot_w %x, pgs_w %d, num_w %d\n",
            ceg_u->nam_c, chf_u.bot_w, chf_u.pgs_w, num_w);
#endif
  }
  printf("loom: loaded %dMB\n", (num_w >> 6));
  return u2_yes;
}

/* u2_loom_exit(): return checkpoint files to their natural size.
*/
void
u2_loom_exit(void)
{
  u2_cheg* ceg_u;

  for ( ceg_u = &LoomSegmentA; ceg_u; ceg_u = ceg_u->nex_u ) {
    ftruncate(ceg_u->dat_i, (ceg_u->pgs_w << (LoomPageWords + 2)));
  }
}

/* u2_loom_save(): checkpoint at current date, with hat and mat.
*/
u2_bean
u2_loom_save(c3_w ent_w)
{
  u2_cheg* ceg_u;
  c3_w tot_w = 0;

  // uL(fprintf(uH, "# saving at event %u...\n", ent_w));
  u2_wr_check_save();

  for ( ceg_u = &LoomSegmentA; ceg_u; ceg_u = ceg_u->nex_u ) {
    u2_chef chf_u;
    c3_w i_w;
    c3_w num_w = 0;

    chf_u.ent_w = ent_w;
    chf_u.ven_w = LoomVersion;
    chf_u.bot_w = ceg_u->bot_w;
    chf_u.pgs_w = ceg_u->pgs_w;

    /* Save data.
    */
    {
      for ( i_w = 0; i_w < ceg_u->pgs_w; i_w++ ) {
        c3_w pag_w  = ceg_u->bot_w + i_w;
        c3_w* mem_w = ((c3_w*)U2_OS_LoomBase) + (pag_w << LoomPageWords);

        if ( u2_page_tref == LoomChem[pag_w].lif_e ) {
          lseek(ceg_u->dat_i, (i_w << (LoomPageWords + 2)), SEEK_SET);
          if ( u2_no == _loom_write(ceg_u->dat_i,
                                    mem_w,
                                    (1 << LoomPageWords)) ) {
            fprintf(stderr, "save no a\r\n");
            return u2_no;
          }
          LoomChem[pag_w].mug_e = u2_mug_words(mem_w, (1 << LoomPageWords));
          LoomChem[pag_w].lif_e = u2_page_neat;

          num_w++;
        }
      }
      // ftruncate(ceg_u->dat_i, (ceg_u->pgs_w << (LoomPageWords + 2)));
    }

#if 0
    fprintf(stderr, "save: %s bot_w %x, pgs_w %d, num_w %d\r\n",
                     ceg_u->nam_c, chf_u.bot_w, chf_u.pgs_w, num_w);
#endif

    /* Save control file.
    */
    lseek(ceg_u->ctl_i, 0, SEEK_SET);
    if ( u2_no == _loom_write(ceg_u->ctl_i, (c3_w*)&chf_u, c3_wiseof(chf_u)) ) {
      fprintf(stderr, "save no b\r\n");
      return u2_no;
    }
    if ( u2_no == _loom_write(ceg_u->ctl_i,
                              (u2_chit*)(LoomChem + ceg_u->bot_w),
                              ceg_u->pgs_w) ) {
      fprintf(stderr, "save no c\r\n");
      return u2_no;
    }

    /* Catch future changes.
    */
    if ( -1 == mprotect(((c3_w*)U2_OS_LoomBase) +
                          (ceg_u->bot_w << LoomPageWords),
                        (ceg_u->len_w << (LoomPageWords + 2)),
                        PROT_READ) )
    {
      fprintf(stderr, "save no d\r\n");
      return u2_no;
    }
    for ( i_w = 0; i_w < ceg_u->len_w; i_w++ ) {
      LoomChem[i_w + ceg_u->bot_w].lif_e = u2_page_neat;
    }
    tot_w += num_w;
  }

  /* sync the mfer
  */
  for ( ceg_u = &LoomSegmentA; ceg_u; ceg_u = ceg_u->nex_u ) {
#if defined(U2_OS_linux)
    fdatasync(ceg_u->ctl_i);
    fdatasync(ceg_u->dat_i);
#elif defined(U2_OS_osx)
    fcntl(ceg_u->ctl_i, F_FULLFSYNC);
    fcntl(ceg_u->dat_i, F_FULLFSYNC);
#elif defined(U2_OS_bsd)
    fsync(ceg_u->ctl_i);
    fsync(ceg_u->dat_i);
#else
    #error "port: datasync"
#endif
  }

#if 0
    uL(fprintf(uH, "loom: wrote %uMB\n", (tot_w >> 6)));
#endif

  return u2_yes;
}

/* _loom_start(): set up interrupts, etc.
*/
static void
_loom_start(void)
{
  if ( 0 != sigsegv_install_handler(_loom_sigsegv_handler) ) {
    fprintf(stderr, "sigsegv install failed\n");
    exit(1);
  }

  {
    struct rlimit rlm;
#define LOOM_STACK (65536 << 10)
    getrlimit(RLIMIT_STACK, &rlm);
    rlm.rlim_cur = rlm.rlim_max > LOOM_STACK ? LOOM_STACK : rlm.rlim_max;
    if ( 0 != setrlimit(RLIMIT_STACK, &rlm) ) {
      perror("stack");
      exit(1);
    }
#undef LOOM_STACK

    getrlimit(RLIMIT_NOFILE, &rlm);
    rlm.rlim_cur = 4096;
    if ( 0 != setrlimit(RLIMIT_NOFILE, &rlm) ) {
      perror("file limit");
      //  no exit, not a critical limit
    }

    getrlimit(RLIMIT_CORE, &rlm);
    rlm.rlim_cur = RLIM_INFINITY;
    if ( 0 != setrlimit(RLIMIT_CORE, &rlm) ) {
      perror("core limit");
      //  no exit, not a critical limit
    }
  }
  signal(SIGINT, _loom_stop);
}

/* u2_loom_load():
**
**   Try to load the loom from a checkpoint.
*/
u2_bean
u2_loom_load(void)
{
  if ( u2_yes == _loom_deploy() ) {
    _loom_start();
    return u2_yes;
  }
  else {
    munmap((c3_w*)U2_OS_LoomBase, (LoomAllPages << (LoomPageWords + 2)));

    return u2_no;
  }
}

/* u2_loom_boot():
**
**   Instantiate the loom.
*/
void
u2_loom_boot(void)
{
  void *map;

  map = mmap((void *)U2_OS_LoomBase,
             (HalfSize << 3),
             PROT_READ,
             (MAP_ANON | MAP_FIXED | MAP_PRIVATE),
             -1, 0);

  if ( -1 == (c3_ps)map ) {
    map = mmap((void *)0,
               (HalfSize << 3),
               PROT_READ,
                MAP_ANON | MAP_PRIVATE,
               -1, 0);

    if ( -1 == (c3_ps)map ) {
      fprintf(stderr, "map failed twice\n");
    } else {
      fprintf(stderr, "map failed - try U2_OS_LoomBase %p\n", map);
    }
    exit(1);
  }
  printf("loom: mapped %dMB\n", (1 << (LoomBits - 18)));

  _loom_start();
}

/* u2_mean():
**
**   Attempt to deconstruct `a` by axis, noun pairs; 0 terminates.
**   Axes must be sorted in tree order.
*/
  struct _mean_pair {
    c3_w    axe_w;
    u2_noun* som;
  };

  static c3_w
  _mean_cut(c3_w               len_w,
            struct _mean_pair* prs_m)
  {
    c3_w i_w, cut_t, cut_w;

    cut_t = c3_false;
    cut_w = 0;
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      c3_w axe_w = prs_m[i_w].axe_w;

      if ( (cut_t == c3_false) && (3 == u2_ax_cap(axe_w)) ) {
        cut_t = c3_true;
        cut_w = i_w;
      }
      prs_m[i_w].axe_w = u2_ax_mas(axe_w);
    }
    return cut_t ? cut_w : i_w;
  }

  static u2_bean
  _mean_extract(u2_noun            som,
                c3_w               len_w,
                struct _mean_pair* prs_m)
  {
    if ( len_w == 0 ) {
      return u2_yes;
    }
    else if ( (len_w == 1) && (1 == prs_m[0].axe_w) ) {
      *prs_m->som = som;
      return u2_yes;
    }
    else {
      if ( u2_no == u2_dust(som) ) {
        return u2_no;
      } else {
        c3_w cut_w = _mean_cut(len_w, prs_m);

        return u2_and
          (_mean_extract(u2_h(som), cut_w, prs_m),
           _mean_extract(u2_t(som), (len_w - cut_w), (prs_m + cut_w)));
      }
    }
  }

u2_bean
u2_mean(u2_noun som,
        ...)
{
  va_list            ap;
  c3_w               len_w;
  struct _mean_pair* prs_m;

  c3_assert(u2_none != som);

  /* Count.
  */
  len_w = 0;
  {
    va_start(ap, som);
    while ( 1 ) {
      if ( 0 == va_arg(ap, c3_w) ) {
        break;
      }
      va_arg(ap, u2_noun*);
      len_w++;
    }
    va_end(ap);
  }
  prs_m = alloca(len_w * sizeof(struct _mean_pair));

  /* Install.
  */
  {
    c3_w i_w;

    va_start(ap, som);
    for ( i_w = 0; i_w < len_w; i_w++ ) {
      prs_m[i_w].axe_w = va_arg(ap, c3_w);
      prs_m[i_w].som = va_arg(ap, u2_noun*);
    }
    va_end(ap);
  }

  /* Extract.
  */
  return _mean_extract(som, len_w, prs_m);
}

/* _frag_word(): fast fragment/branch prediction for top word.
*/
static u2_weak
_frag_word(c3_w a_w, u2_noun b)
{
  c3_assert(0 != a_w);

  {
    c3_w dep_w = u2_ax_dep(a_w);

    while ( dep_w ) {
      if ( u2_no == u2_dust(b) ) {
        return u2_none;
      }
      else {
        c3_w x = (1 & (a_w >> (dep_w - 1)));

        b = *u2_at_ray(1 + x + u2_pom_a(b));
        dep_w--;
      }
    }
    return b;
  }
}

/* _frag_deep(): fast fragment/branch for deep words.
*/
static u2_weak
_frag_deep(c3_w a_w, u2_noun b)
{
  c3_w dep_w = 32;

  while ( dep_w ) {
    if ( u2_no == u2_dust(b) ) {
      return u2_none;
    }
    else {
      c3_w x = (1 & (a_w >> (dep_w - 1)));

      b = *u2_at_ray(1 + x + u2_pom_a(b));
      dep_w--;
    }
  }
  return b;
}

/* _frag_phat(): fragment for fat.
*/
static u2_weak
_frag_phat(u2_noun a, u2_noun b)
{
  c3_w len_w = *u2_at_pug_len(a);

  b = _frag_word(*u2_at_pug_buf(a, (len_w - 1)), b);
  len_w -= 1;

  while ( len_w ) {
    b = _frag_deep(*u2_at_pug_buf(a, (len_w - 1)), b);

    if ( u2_none == b ) {
      return b;
    } else {
      len_w--;
    }
  }
  return b;
}

/* u2_frag():
**
**   Return fragment (a) of (b), or u2_none if not applicable.
*/
u2_weak
u2_frag(u2_atom a,
        u2_noun b)
{
  c3_assert(u2_none != a);
  c3_assert(u2_none != b);

  if ( _0 == a ) {
    return u2_none;
  }

  if ( u2_fly_is_cat(a) ) {
    return _frag_word(a, b);
  }
  else {
    if ( !u2_dog_is_pug(a) ) {
      return u2_none;
    }
    else {
      return _frag_phat(a, b);
#if 0
      c3_w  fol_w = (u2_met(0, a) - 1);
      c3_w  i_w;
      mpz_t a_mp;

      c3_assert(u2_none != b);
      u2_mp(a_mp, a);

      for ( i_w=0; i_w < fol_w; i_w++ ) {
        c3_w lum_w = (fol_w - (i_w + 1));

        if ( u2_no == u2_dust(b) ) {
          mpz_clear(a_mp);

          return u2_none;
        }
        else {
          if ( (mpz_tstbit(a_mp, lum_w) == 0) ) {
            b = u2_h(b);
          } else {
            b = u2_t(b);
          }
        }
      }
      mpz_clear(a_mp);
      return b;
#endif
    }
  }
}

/* u2_mug():
**
**   Compute and/or recall the mug (31-bit FNV1a hash) of (a).
*/
static __inline__ c3_w
_mug_fnv(c3_w has_w)
{
  return (has_w * ((c3_w)16777619));
}

static __inline__ c3_w
_mug_out(c3_w has_w)
{
  return (has_w >> 31) ^ (has_w & 0x7fffffff);
}

static __inline__ c3_w
_mug_both(c3_w lef_w, c3_w rit_w)
{
  c3_w bot_w = _mug_fnv(lef_w ^ _mug_fnv(rit_w));
  c3_w out_w = _mug_out(bot_w);

  if ( 0 != out_w ) {
    return out_w;
  }
  else {
    return _mug_both(lef_w, ++rit_w);
  }
}

c3_w
u2_mug_both(c3_w lef_w, c3_w rit_w)
{
  return _mug_both(lef_w, rit_w);
}

static __inline__ c3_w
_mug_bytes_in(c3_w off_w, c3_w nby_w, c3_y* byt_y)
{
  c3_w i_w;

  for ( i_w = 0; i_w < nby_w; i_w++ ) {
    off_w = _mug_fnv(off_w ^ byt_y[i_w]);
  }
  return off_w;
}

static c3_w
_mug_bytes(c3_w off_w, c3_w nby_w, c3_y* byt_y)
{
  c3_w has_w = _mug_bytes_in(off_w, nby_w, byt_y);
  c3_w out_w = _mug_out(has_w);

  if ( 0 != out_w ) {
    return out_w;
  }
  else {
    return _mug_bytes(++off_w, nby_w, byt_y);
  }
}

static __inline__ c3_w
_mug_words_in_buf(c3_w off_w, c3_w nwd_w, u2_noun veb)
{
  if ( 0 == nwd_w ) {
    return off_w;
  } else {
    c3_w i_w, x_w;

    for ( i_w = 0; i_w < (nwd_w - 1); i_w++ ) {
      x_w = *u2_at_pug_buf(veb, i_w);
      {
        c3_y a_y = (x_w & 0xff);
        c3_y b_y = ((x_w >> 8) & 0xff);
        c3_y c_y = ((x_w >> 16) & 0xff);
        c3_y d_y = ((x_w >> 24) & 0xff);

        off_w = _mug_fnv(off_w ^ a_y);
        off_w = _mug_fnv(off_w ^ b_y);
        off_w = _mug_fnv(off_w ^ c_y);
        off_w = _mug_fnv(off_w ^ d_y);
      }
    }
    x_w = *u2_at_pug_buf(veb, (nwd_w - 1));

    if ( x_w ) {
      off_w = _mug_fnv(off_w ^ (x_w & 0xff));
      x_w >>= 8;

      if ( x_w ) {
        off_w = _mug_fnv(off_w ^ (x_w & 0xff));
        x_w >>= 8;

        if ( x_w ) {
          off_w = _mug_fnv(off_w ^ (x_w & 0xff));
          x_w >>= 8;

          if ( x_w ) {
            off_w = _mug_fnv(off_w ^ (x_w & 0xff));
          }
        }
      }
    }
  }
  return off_w;
}

static __inline__ c3_w
_mug_words_in(c3_w off_w, c3_w nwd_w, const c3_w* wod_w)
{
  if ( 0 == nwd_w ) {
    return off_w;
  } else {
    c3_w i_w, x_w;

    for ( i_w = 0; i_w < (nwd_w - 1); i_w++ ) {
      x_w = wod_w[i_w];
      {
        c3_y a_y = (x_w & 0xff);
        c3_y b_y = ((x_w >> 8) & 0xff);
        c3_y c_y = ((x_w >> 16) & 0xff);
        c3_y d_y = ((x_w >> 24) & 0xff);

        off_w = _mug_fnv(off_w ^ a_y);
        off_w = _mug_fnv(off_w ^ b_y);
        off_w = _mug_fnv(off_w ^ c_y);
        off_w = _mug_fnv(off_w ^ d_y);
      }
    }
    x_w = wod_w[nwd_w - 1];

    if ( x_w ) {
      off_w = _mug_fnv(off_w ^ (x_w & 0xff));
      x_w >>= 8;

      if ( x_w ) {
        off_w = _mug_fnv(off_w ^ (x_w & 0xff));
        x_w >>= 8;

        if ( x_w ) {
          off_w = _mug_fnv(off_w ^ (x_w & 0xff));
          x_w >>= 8;

          if ( x_w ) {
            off_w = _mug_fnv(off_w ^ (x_w & 0xff));
          }
        }
      }
    }
  }
  return off_w;
}

static c3_w
_mug_words(c3_w off_w, c3_w nwd_w, const c3_w* wod_w)
{
  c3_w has_w = _mug_words_in(off_w, nwd_w, wod_w);
  c3_w out_w = _mug_out(has_w);

  if ( 0 != out_w ) {
    return out_w;
  }
  else {
    return _mug_words(++off_w, nwd_w, wod_w);
  }
}

static c3_w
_mug_words_buf(c3_w off_w, c3_w nwd_w, u2_noun veb)
{
  c3_w has_w = _mug_words_in_buf(off_w, nwd_w, veb);
  c3_w out_w = _mug_out(has_w);

  if ( 0 != out_w ) {
    return out_w;
  }
  else {
    return _mug_words_buf(++off_w, nwd_w, veb);
  }
}

c3_w
u2_mug(u2_noun veb)
{
  c3_assert(u2_none != veb);

  if ( u2_fly_is_cat(veb) ) {
    c3_w x_w = veb;

    return _mug_words(2166136261, (veb ? 1 : 0), &x_w);
  } else {
    if ( *u2_at_dog_mug(veb) ) {
      c3_w mug_w = *u2_at_dog_mug(veb);

      return mug_w;
    }
    else {
      c3_w mug_w;

      if ( u2_dog_is_pom(veb) ) {
        u2_noun hed = *u2_at_pom_hed(veb);
        u2_noun tel = *u2_at_pom_tel(veb);

        mug_w = u2_mug_cell(hed, tel);
      }
      else {
        c3_w len_w = *u2_at_pug_len(veb);

        mug_w = _mug_words_buf(2166136261, len_w, veb);
      }

      *u2_at_dog_mug(veb) = mug_w;
      return mug_w;
    }
  }
}

/* u2_mug_words():
**
**   Compute the mug of `buf`, `len`, LSW first.
*/
c3_w
u2_mug_words(const c3_w *buf_w,
             c3_w        len_w)
{
  return _mug_words(2166136261, len_w, buf_w);
}

/* u2_mug_string():
**
**   Compute the mug of `a`, LSB first.
*/
c3_w
u2_mug_string(const c3_c *a_c)
{
  return _mug_bytes(2166136261, strlen(a_c), (c3_y *)a_c);
}

/* u2_mug_cell():
**
**   Compute the mug of the cell `[hed tel]`.
*/
c3_w
u2_mug_cell(u2_noun hed,
            u2_noun tel)
{
  c3_w   lus_w = u2_mug(hed);
  c3_w   biq_w = u2_mug(tel);

  return u2_mug_both(lus_w, biq_w);
}

/* u2_mug_trel():
**
**   Compute the mug of `[a b c]`.
*/
c3_w
u2_mug_trel(u2_noun a,
            u2_noun b,
            u2_noun c)
{
  return u2_mug_both(u2_mug(a), u2_mug_both(u2_mug(b), u2_mug(c)));
}

/* u2_mug_qual():
**
**   Compute the mug of `[a b c d]`.
*/
c3_w
u2_mug_qual(u2_noun a,
            u2_noun b,
            u2_noun c,
            u2_noun d)
{
  return u2_mug_both(u2_mug(a),
                     u2_mug_both(u2_mug(b),
                                 u2_mug_both(u2_mug(c), u2_mug(d))));
}

#ifdef U2_PROFILE
  c3_w X;   // XX not thread-safe
  c3_w FUN;
#endif

/* _sing_x():
**
**   Yes iff (a) and (b) are the same noun.
*/
static u2_bean
_sing_x(u2_noun a,
        u2_noun b)
{
  c3_assert(u2_none != a);
  c3_assert(u2_none != b);

  if ( a == b ) {
    return u2_yes;
  }
  else {
#ifdef U2_PROFILE
    X++;
#endif

    if ( u2_fly_is_atom(a) ) {
      if ( !u2_fly_is_atom(b) ||
           u2_fly_is_cat(a) ||
           u2_fly_is_cat(b) )
      {
        return u2_no;
      }
      else {
        if ( *u2_at_dog_mug(a) &&
             *u2_at_dog_mug(b) &&
             (*u2_at_dog_mug(a) != *u2_at_dog_mug(b)) )
        {
          return u2_no;
        }
        else {
          c3_w w_rez = *u2_at_pug_len(a);
          c3_w w_mox = *u2_at_pug_len(b);

          if ( w_rez != w_mox ) {
            return u2_no;
          }
          else {
            c3_w i_w;

            for ( i_w = 0; i_w < w_rez; i_w++ ) {
              if ( (*u2_at_pug_buf(a, i_w)) !=
                   (*u2_at_pug_buf(b, i_w)) )
              {
                return u2_no;
              }
            }
            return u2_yes;
          }
        }
      }
    }
    else {
      if ( u2_fly_is_atom(b) ) {
        return u2_no;
      }
      else {
        if ( *u2_at_dog_mug(a) &&
             *u2_at_dog_mug(b) &&
             (*u2_at_dog_mug(a) != *u2_at_dog_mug(b)) )
        {
          return u2_no;
        }
        else {
          if ( u2_no == _sing_x(u2_h(a), u2_h(b)) ) {
            return u2_no;
          }
          else if ( u2_no == _sing_x(u2_t(a), u2_t(b)) ) {
            return u2_no;
          }
#if 0
          if ( *u2_at_dog_mug(a) && *u2_at_dog_mug(b) ) {
            fprintf(stderr, "duplicate %x %x\r\n", a, b);
          }
#endif
          return u2_yes;
        }
      }
    }
  }
}

#ifdef U2_PROFILE
#if 0
/* _weight(): count subordinate nouns.
*/
static c3_w
_weight(u2_noun n)
{
  if ( u2_yes == u2_stud(n) ) {
    return 1;
  } else {
    return _weight(u2_h(n)) + _weight(u2_t(n));
  }
}
#endif
#endif

/* u2_sing():
**
**   Yes iff (a) and (b) are the same noun.
*/
u2_bean
u2_sing(u2_noun a,
        u2_noun b)
{
#ifndef U2_PROFILE
  return _sing_x(a, b);
#else
  X=0;
  {
    u2_bean sit = _sing_x(a, b);

    if ( (u2_yes == sit) && (a != b) ) {
      u2_tx_did_nod(0, X);
    }
    if ( FUN && (X > 10) ) {
      // printf("mug %x, X %d\n", u2_mug(a), X);
      // if ( u2_mug(a) == 0xe5c2279 )
      // { printf("a %x, b %x\n", a, b); c3_assert(0); }
    }
    return sit;
  }
#endif
}

u2_bean
u2_fing(u2_noun a,
        u2_noun b)
{
  return (a == b) ? u2_yes : u2_no;
}

/* u2_sing_cell():
**
**   Yes iff `[p q]` and `b` are the same noun.
*/
u2_bean
u2_sing_cell(u2_noun p,
             u2_noun q,
             u2_noun b)
{
  return u2_and(u2_dust(b),
                u2_and(u2_sing(p, u2_h(b)),
                       u2_sing(q, u2_t(b))));
}
u2_bean
u2_fing_cell(u2_noun p,
             u2_noun q,
             u2_noun b)
{
  return u2_and(u2_dust(b),
                u2_and(u2_fing(p, u2_h(b)),
                       u2_fing(q, u2_t(b))));
}

/* u2_sing_mixt():
**
**   Yes iff `[p q]` and `b` are the same noun.
*/
u2_bean
u2_sing_mixt(const c3_c* p_c,
             u2_noun     q,
             u2_noun     b)
{
  return u2_and(u2_dust(b),
                u2_and(u2_sing_c(p_c, u2_h(b)),
                       u2_sing(q, u2_t(b))));
}
u2_bean
u2_fing_mixt(const c3_c* p_c,
             u2_noun     q,
             u2_noun     b)
{
  return u2_and(u2_dust(b),
                u2_and(u2_sing_c(p_c, u2_h(b)),
                       u2_fing(q, u2_t(b))));
}

/* u2_sing_trel():
**
**   Yes iff `[p q r]` and `b` are the same noun.
*/
u2_bean
u2_sing_trel(u2_noun p,
             u2_noun q,
             u2_noun r,
             u2_noun b)
{
  return u2_and(u2_dust(b),
                u2_and(u2_sing(p, u2_h(b)),
                       u2_sing_cell(q, r, u2_t(b))));
}
u2_bean
u2_fing_trel(u2_noun p,
             u2_noun q,
             u2_noun r,
             u2_noun b)
{
  return u2_and(u2_dust(b),
                u2_and(u2_fing(p, u2_h(b)),
                       u2_fing_cell(q, r, u2_t(b))));
}

/* u2_sing_qual():
**
**   Yes iff `[p q r]` and `b` are the same noun.
*/
u2_bean
u2_sing_qual(u2_noun p,
             u2_noun q,
             u2_noun r,
             u2_noun s,
             u2_noun b)
{
  return u2_and(u2_dust(b),
                u2_and(u2_sing(p, u2_h(b)),
                       u2_sing_trel(q, r, s, u2_t(b))));
}
u2_bean
u2_fing_qual(u2_noun p,
             u2_noun q,
             u2_noun r,
             u2_noun s,
             u2_noun b)
{
  return u2_and(u2_dust(b),
                u2_and(u2_fing(p, u2_h(b)),
                       u2_fing_trel(q, r, s, u2_t(b))));
}

/* u2_nord():
**
**   Return 0, 1 or 2 if `a` is below, equal to, or above `b`.
*/
u2_atom
u2_nord(u2_noun a,
        u2_noun b)
{
  c3_assert(u2_none != a);
  c3_assert(u2_none != b);

  if ( a == b ) {
    return _1;
  }
  else {
    if ( u2_fly_is_atom(a) ) {
      if ( !u2_fly_is_atom(b) ) {
        return _0;
      } else {
        if ( u2_fly_is_cat(a) ) {
          if ( u2_fly_is_cat(b) ) {
            return (a < b) ? 0 : 2;
          }
          else return 0;
        }
        else if ( u2_fly_is_cat(b) ) {
          return 2;
        }
        else {
          c3_w w_rez = *u2_at_pug_len(a);
          c3_w w_mox = *u2_at_pug_len(b);

          if ( w_rez != w_mox ) {
            return (w_rez < w_mox) ? _0 : _2;
          }
          else {
            c3_w i_w;

            for ( i_w = 0; i_w < w_rez; i_w++ ) {
              c3_w ai_w = *u2_at_pug_buf(a, i_w);
              c3_w bi_w = *u2_at_pug_buf(b, i_w);

              if ( ai_w != bi_w ) {
                return (ai_w < bi_w) ? _0 : _2;
              }
            }
            return _1;
          }
        }
      }
    } else {
      if ( u2_fly_is_atom(b) ) {
        return _2;
      } else {
        u2_atom c = u2_nord(u2_h(a), u2_h(b));

        if ( _1 == c ) {
          return u2_nord(u2_t(a), u2_t(b));
        } else {
          return c;
        }
      }
    }
  }
}

/* u2_sing_c():
**
**   Yes iff (b) is the same noun as the C string a_c.
*/
u2_bean
u2_sing_c(const c3_c* a_c,
          u2_noun     b)
{
  c3_assert(u2_none != b);

  if ( !u2_fly_is_atom(b) ) {
    return u2_no;
  }
  else {
    c3_w w_sof = strlen(a_c);
    c3_w i_w;

    for ( i_w = 0; i_w < w_sof; i_w++ ) {
      if ( u2_byte(i_w, b) != a_c[i_w] ) {
        return u2_no;
      }
    }
    return u2_yes;
  }
}

/* u2_as_bush():
**
**   Factor [a] as a bush [b.[p q] c].
*/
u2_bean
u2_as_bush(u2_noun  a,
           u2_noun* b,
           u2_noun* c)
{
  c3_assert(u2_none != a);

  if ( u2_fly_is_atom(a) ) {
    return u2_no;
  }
  else {
    *b = u2_h(a);

    if ( u2_fly_is_atom(*b) ) {
      return u2_no;
    } else {
      *c = u2_t(a);
      return u2_yes;
    }
  }
}

/* u2_as_cell():
**
**   Factor (a) as a cell (b c).
*/
u2_bean
u2_as_cell(u2_noun  a,
           u2_noun* b,
           u2_noun* c)
{
  c3_assert(u2_none != a);

  if ( u2_fly_is_atom(a) ) {
    return u2_no;
  }
  else {
    if ( b ) *b = u2_h(a);
    if ( c ) *c = u2_t(a);
    return u2_yes;
  }
}

/* u2_as_hext():
**
**   Factor (a) as a hext (b c d e f g)
*/
u2_bean
u2_as_hext(u2_noun  a,
           u2_noun* b,
           u2_noun* c,
           u2_noun* d,
           u2_noun* e,
           u2_noun* f,
           u2_noun* g)
{
  u2_noun guf;

  if ( (u2_yes == u2_as_cell(a, b, &guf)) &&
       (u2_yes == u2_as_quil(guf, c, d, e, f, g)) ) {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_as_p():
**
**   & [0] if [a] is of the form [b *c].
*/
u2_bean
u2_as_p(u2_noun  a,
        u2_noun  b,
        u2_noun* c)
{
  u2_noun feg, nux;

  if ( (u2_yes == u2_as_cell(a, &feg, &nux)) &&
       (u2_yes == u2_sing(feg, b)) )
  {
    *c = nux;
    return u2_yes;
  }
  else return u2_no;
}

/* u2_as_pq():
**
**   & [0] if [a] is of the form [b *c d].
*/
u2_bean
u2_as_pq(u2_noun  a,
         u2_noun  b,
         u2_noun* c,
         u2_noun* d)
{
  u2_noun nux;

  if ( (u2_yes == u2_as_p(a, b, &nux)) &&
       (u2_yes == u2_as_cell(nux, c, d)) )
  {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_as_pqr():
**
**   & [0] if [a] is of the form [b *c *d *e].
*/
u2_bean
u2_as_pqr(u2_noun  a,
          u2_noun  b,
          u2_noun* c,
          u2_noun* d,
          u2_noun* e)
{
  u2_noun nux;

  if ( (u2_yes == u2_as_p(a, b, &nux)) &&
       (u2_yes == u2_as_trel(nux, c, d, e)) )
  {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_as_pqrs():
**
**   & [0] if [a] is of the form [b *c *d *e *f].
*/
u2_bean
u2_as_pqrs(u2_noun  a,
           u2_noun  b,
           u2_noun* c,
           u2_noun* d,
           u2_noun* e,
           u2_noun* f)
{
  u2_noun nux;

  if ( (u2_yes == u2_as_p(a, b, &nux)) &&
       (u2_yes == u2_as_qual(nux, c, d, e, f)) )
  {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_as_qual():
**
**   Factor (a) as a qual (b c d e).
*/
u2_bean
u2_as_qual(u2_noun  a,
           u2_noun* b,
           u2_noun* c,
           u2_noun* d,
           u2_noun* e)
{
  u2_noun guf;

  if ( (u2_yes == u2_as_cell(a, b, &guf)) &&
       (u2_yes == u2_as_trel(guf, c, d, e)) ) {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_as_quil():
**
**   Factor (a) as a quil (b c d e f).
*/
u2_bean
u2_as_quil(u2_noun  a,
           u2_noun* b,
           u2_noun* c,
           u2_noun* d,
           u2_noun* e,
           u2_noun* f)
{
  u2_noun guf;

  if ( (u2_yes == u2_as_cell(a, b, &guf)) &&
       (u2_yes == u2_as_qual(guf, c, d, e, f)) ) {
    return u2_yes;
  }
  else return u2_no;
}

/* u2_as_trel():
**
**   Factor (a) as a trel (b c d).
*/
u2_bean
u2_as_trel(u2_noun a,
           u2_noun *b,
           u2_noun *c,
           u2_noun *d)
{
  u2_noun guf;

  if ( (u2_yes == u2_as_cell(a, b, &guf)) &&
       (u2_yes == u2_as_cell(guf, c, d)) ) {
    return u2_yes;
  }
  else {
    return u2_no;
  }
}

/* u2_sh():
**
**   Return the head of (a), safely.
*/
u2_noun
u2_sh(u2_cell a)
{
  return (a == u2_none) && (u2_yes == u2_dust(a)) ? a : u2_h(a);
}

/* u2_st():
**
**   Return the tail of (a), safely.
*/
u2_noun
u2_st(u2_noun a)
{
  return (a == u2_none) && (u2_yes == u2_dust(a)) ? a : u2_t(a);
}

/* u2_met():
**
**   Return the size of (b) in bits, rounded up to
**   (1 << a_y).
**
**   For example, (a_y == 3) returns the size in bytes.
*/
c3_w
u2_met(c3_y    a_y,
       u2_atom b)
{
  c3_assert(u2_none != b);
  c3_assert(u2_fly_is_atom(b));

  if ( b == 0 ) {
    return 0;
  }
  else {
    /* gal_w: number of words besides (daz_w) in (b).
    ** daz_w: top word in (b).
    */
    c3_w gal_w;
    c3_w daz_w;

    if ( u2_fly_is_cat(b) ) {
      gal_w = 0;
      daz_w = b;
    }
    else {
      gal_w = (*u2_at_pug_len(b)) - 1;
      daz_w = (*u2_at_pug_buf(b, gal_w));
    }

    switch ( a_y ) {
      case 0:
      case 1:
      case 2: {
        /* col_w: number of bits in (daz_w)
        ** bif_w: number of bits in (b)
        */
        c3_w bif_w, col_w;

        col_w = c3_bits_word(daz_w);
        bif_w = col_w + (gal_w << 5);

        return (bif_w + ((1 << a_y) - 1)) >> a_y;
      }
      case 3: {
        return  (gal_w << 2)
              + ((daz_w >> 24) ? 4 : (daz_w >> 16) ? 3 : (daz_w >> 8) ? 2 : 1);
      }
      case 4: {
        return  (gal_w << 1)
              + ((daz_w >> 16) ? 2 : 1);
      }
      default: {
        c3_y gow_y = (a_y - 5);

        return ((gal_w + 1) + ((1 << gow_y) - 1)) >> gow_y;
      }
    }
  }
}

/* u2_bit():
**
**   Return bit (a_w) of (b).
*/
c3_b
u2_bit(c3_w    a_w,
       u2_atom b)
{
  c3_assert(u2_none != b);
  c3_assert(u2_fly_is_atom(b));

  if ( u2_fly_is_cat(b) ) {
    if ( a_w >= 31 ) {
      return 0;
    }
    else return (1 & (b >> a_w));
  }
  else {
    c3_y vut_y = (a_w & 31);
    c3_w pix_w = (a_w >> 5);

    if ( pix_w >= *u2_at_pug_len(b) ) {
      return 0;
    }
    else {
      c3_w nys_w = *u2_at_pug_buf(b, pix_w);

      return (1 & (nys_w >> vut_y));
    }
  }
}

/* u2_byte():
**
**   Return byte (a_w) of (b).
*/
c3_y
u2_byte(c3_w    a_w,
        u2_atom b)
{
  c3_assert(u2_none != b);
  c3_assert(u2_fly_is_atom(b));

  if ( u2_fly_is_cat(b) ) {
    if ( a_w > 3 ) {
      return 0;
    }
    else return (255 & (b >> (a_w << 3)));
  }
  else {
    c3_y vut_y = (a_w & 3);
    c3_w pix_w = (a_w >> 2);

    if ( pix_w >= *u2_at_pug_len(b) ) {
      return 0;
    }
    else {
      c3_w nys_w = *u2_at_pug_buf(b, pix_w);

      return (255 & (nys_w >> (vut_y << 3)));
    }
  }
}

/* u2_bytes():
**
**  Copy bytes (a_w) through (a_w + b_w - 1) from (d) to (c).
*/
void
u2_bytes(c3_w    a_w,
         c3_w    b_w,
         c3_y*   c_y,
         u2_atom d)
{
  c3_w i_w;

  c3_assert(u2_none != d);

  /* Efficiency: don't call u2_word().
  */
  for ( i_w = 0; i_w < b_w; i_w++ ) {
    c_y[i_w] = u2_byte((a_w + i_w), d);
  }
}

/* u2_mp():
**
**   Copy (b) into (a_mp).
*/
void
u2_mp(mpz_t   a_mp,
      u2_atom b)
{
  c3_assert(u2_none != b);
  c3_assert(u2_fly_is_atom(b));

  if ( u2_fly_is_cat(b) ) {
    mpz_init_set_ui(a_mp, b);
  }
  else {
    c3_w len_w = *u2_at_pug_len(b);

    /* Slight deficiency in the GMP API.
    */
    c3_assert(!(len_w >> 27));
    mpz_init2(a_mp, len_w << 5);

    /* Efficiency: horrible.
    */
    {
      c3_w *buf_w = alloca(len_w << 2);
      c3_w i_w;

      for ( i_w=0; i_w < len_w; i_w++ ) {
        buf_w[i_w] = *u2_at_pug_buf(b, i_w);
      }
      mpz_import(a_mp, len_w, -1, 4, 0, 0, buf_w);
    }
  }
}

/* u2_word():
**
**   Return word (a_w) of (b).
*/
c3_w
u2_word(c3_w    a_w,
        u2_atom b)
{
  c3_assert(u2_none != b);
  c3_assert(u2_fly_is_atom(b));

  if ( u2_fly_is_cat(b) ) {
    if ( a_w > 0 ) {
      return 0;
    }
    else return b;
  }
  else {
    if ( a_w >= *u2_at_pug_len(b) ) {
      return 0;
    }
    else return *u2_at_pug_buf(b, a_w);
  }
}

/* u2_chub():
**
**   Return double-word (a_w) of (b).
*/
c3_d
u2_chub(c3_w  a_w,
        u2_atom b)
{
  c3_w wlo_w = u2_word(a_w * 2, b);
  c3_w whi_w = u2_word(1 + (a_w * 2), b);

  return (((uint64_t)whi_w) << 32ULL) | ((uint64_t)wlo_w);
}

/* u2_words():
**
**  Copy words (a_w) through (a_w + b_w - 1) from (d) to (c).
*/
void
u2_words(c3_w    a_w,
         c3_w    b_w,
         c3_w*   c_w,
         u2_atom d)
{
  c3_w i_w;

  c3_assert(u2_none != d);

  /* Efficiency: don't call u2_word().
  */
  for ( i_w = 0; i_w < b_w; i_w++ ) {
    c_w[i_w] = u2_word((a_w + i_w), d);
  }
}

/* u2_chop():
**
**   Into the bloq space of `met`, from position `fum` for a
**   span of `wid`, to position `tou`, XOR from atom `src`
**   into ray `dst`.
*/
void
u2_chop(c3_g    met_g,
        c3_w    fum_w,
        c3_w    wid_w,
        c3_w    tou_w,
        u2_ray  dst_r,
        u2_atom src)
{
  c3_w i_w;

  c3_assert(u2_none != src);
  c3_assert(u2_fly_is_atom(src));

  if ( met_g < 5 ) {
    c3_w san_w = (1 << met_g);
    c3_w mek_w = ((1 << san_w) - 1);
    c3_w baf_w = (fum_w << met_g);
    c3_w bat_w = (tou_w << met_g);

    // XX: efficiency: poor.  Iterate by words.
    //
    for ( i_w = 0; i_w < wid_w; i_w++ ) {
      c3_w waf_w = (baf_w >> 5);
      c3_g raf_g = (baf_w & 31);
      c3_w wat_w = (bat_w >> 5);
      c3_g rat_g = (bat_w & 31);
      c3_w hop_w;

      hop_w = u2_atom_word(src, waf_w);
      hop_w = (hop_w >> raf_g) & mek_w;

      *u2_at_ray(dst_r + wat_w) ^= (hop_w << rat_g);

      baf_w += san_w;
      bat_w += san_w;
    }
  }
  else {
    c3_w len_w = (u2_fly_is_cat(src) ? 1 : *u2_at_pug_len(src));
    c3_g hut_g = (met_g - 5);
    c3_w san_w = (1 << hut_g);
    c3_w j_w;

    for ( i_w = 0; i_w < wid_w; i_w++ ) {
      c3_w wuf_w = (fum_w + i_w) << hut_g;
      c3_w wut_w = (tou_w + i_w) << hut_g;

      for ( j_w = 0; j_w < san_w; j_w++ ) {
        *u2_at_ray(dst_r + wut_w + j_w) ^=
            ((wuf_w + j_w) >= len_w)
              ? 0
              : u2_atom_word(src, wuf_w + j_w);
      }
    }
  }
}
