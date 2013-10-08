/* f/coal.c
**
** This file is in the public domain.
*/
#include "all.h"
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <dirent.h>

#include "../gen191/pit.h"

  /**  Jet dependencies.  Minimize these.
  **/
#   define Pt5Y   k_191__mood__hoon

  /**  Jet dependencies.  Minimize these.
  **/
#   define Pt3Y   k_191__mood__hoon
#   define Pt4Y   k_191__mood__hoon
#   define Pt5Y   k_191__mood__hoon

    u2_noun j2_mbc(Pt3Y, gor)(u2_wire, u2_noun a, u2_noun b);
    u2_noun j2_mcc(Pt4Y, by, get)(u2_wire, u2_noun a, u2_noun b);
    u2_noun j2_mcc(Pt4Y, by, put)(u2_wire, u2_noun a, u2_noun b, u2_noun c);
    u2_noun j2_mby(Pt5Y, jam)(u2_wire, u2_noun a);
    u2_noun j2_mby(Pt5Y, trip)(u2_wire, u2_noun a);

#   define _coal_cue  j2_mby(Pt5Y, cue)
#   define _coal_jam  j2_mby(Pt5Y, jam)
#   define _coal_trip j2_mby(Pt5Y, trip)

#   define _coal_gor  j2_mbc(Pt3Y, gor)
#   define _coal_by_gas  j2_mcc(Pt4Y, by, gas)
#   define _coal_by_get  j2_mcc(Pt4Y, by, get)
#   define _coal_by_has  j2_mcc(Pt4Y, in, has)
#   define _coal_by_put  j2_mcc(Pt4Y, by, put)
#   define _coal_in_gas  j2_mcc(Pt4Y, in, gas)
#   define _coal_in_has  j2_mcc(Pt4Y, in, has)
#   define _coal_in_tap  j2_mcc(Pt4Y, in, tap)


/* u2_cf_path(): assemble local path with noun thap and ext.
*/
  /* _cf_path_1: write at/inpath.
  */
  static c3_w
  _cf_path_1(c3_c*   buf_c,
             c3_w    pos_w,
             u2_noun hut)                                         //  retain
  {
    if ( u2_no == u2_cr_du(hut) ) {
      c3_w met_w = u2_cr_met(3, hut);

      if ( buf_c ) u2_cr_bytes(0, met_w, (c3_y*)(buf_c + pos_w), hut);
      return (pos_w + met_w);
    } 
    else {
      c3_w met_w  = u2_cr_met(3, u2h(hut));
      c3_w end_w  = _cf_path_1(buf_c, pos_w, u2t(hut));
      u2_noun san = u2h(hut);

      if ( buf_c ) buf_c[end_w] = '/';
      end_w++;

      // little security thing - last ditch
      //
      if ( c3_s2('.', '.') == san ) {
        san = c3_s3('.','.','.');
      }
      if ( buf_c ) u2_cr_bytes(0, met_w, (c3_y*)(buf_c + end_w), san);
      end_w += met_w;

      return end_w;
    }
  }

u2_noun
u2_cf_path(c3_c* top_c, 
           c3_c* ext_c, 
           u2_noun tah)
{
  c3_w    top_w = strlen(top_c);
  c3_w    len_w = _cf_path_1(0, (top_w + 1), tah);
  c3_c*   buf_c = malloc(len_w + (ext_c ? (1 + strlen(ext_c)) : 0) + 1);
  c3_w    pos_w;
  u2_noun pas;

  strcpy(buf_c, top_c);
  pos_w = top_w;
  buf_c[pos_w++] = '/';

  pos_w = _cf_path_1(buf_c, pos_w, tah);

  if ( ext_c ) {
    buf_c[pos_w++] = '.'; 
    strcpy(buf_c + pos_w, ext_c);
  } else {
    buf_c[pos_w] = 0;
  }

  pas = u2_ci_string(buf_c);
  free(buf_c);

  u2_cz(tah);
  return pas;
}
  
/* u2_cf_flat_date(): date for `pas`.  Unix time * 10^6, or 0.
*/
c3_d
u2_cf_flat_date(u2_noun pas)
{
  c3_c*       pas_c = u2_cr_string(pas);
  struct stat pas_s;

  u2_cz(pas);
  if ( stat(pas_c, &pas_s) < 0 ) {
    free(pas_c);

    return 0;
  } else {
    free(pas_c);
#if defined(U2_OS_linux)
    return ( ((c3_d)pas_s.st_mtime) );
#elif defined(U2_OS_osx)
    return ( ((c3_d)pas_s.st_mtimespec.tv_sec) +
             ((c3_d)(pas_s.st_mtimespec.tv_nsec / 1000)) );
#elif defined(U2_OS_bsd)
    return ( ((c3_d)pas_s.st_mtim.tv_sec) +
             ((c3_d)(pas_s.st_mtim.tv_nsec / 1000)) );
#else
    #error "port: filetime"
#endif
  }
}

/* u2_cf_flat_load(): load `mod` at `pas`.  Bail on error.
*/
u2_weak
u2_cf_flat_load(u2_noun mod,
                u2_noun pas)
{
  c3_assert(c3__atom == mod);
  {
    c3_c*       pas_c = u2_cr_string(pas);
    c3_i        fid_i;
    struct stat sat_s;
    c3_w        fln_w;
    c3_c*       fil_c;
    u2_atom     fil;

    fid_i = open(pas_c, O_RDONLY, 0644);
    free(pas_c);
    u2_cz(pas);

    if ( (fid_i < 0) || (fstat(fid_i, &sat_s) < 0) ) {
      perror(pas_c);
      return u2_none;
    }

    fln_w = sat_s.st_size;
    fil_c = malloc(sat_s.st_size);

    if ( fln_w != read(fid_i, fil_c, fln_w) ) {
      return u2_none;
    }
    close(fid_i);

    fil = u2_ci_bytes(fln_w, (c3_y *)fil_c); 
    free(fil_c);

    return fil;
  }
}

/* u2_cf_list(): list all the files in directory `pas`.  List of cask.
*/
u2_noun 
u2_cf_list(u2_noun pas)
{
  c3_c* pas_c = u2_cr_string(pas);

  u2z(pas);
  {
    u2_noun lis = u2_nul;
    DIR *dir_d = opendir(pas_c);

    if ( !dir_d ) {
      free(pas_c);
      return u2_nul;
    }
    else {
      while ( 1 ) {
        struct dirent ent_n;
        struct dirent *out_n;

        if ( readdir_r(dir_d, &ent_n, &out_n) != 0 ) {
          perror(pas_c);
          return u2_cm_bail(c3__fail);
        } 
        else if ( !out_n ) {
          break;
        }
        else lis = u2nc(u2_ci_string(out_n->d_name), lis);
      }

      free(pas_c);
      return lis;
    }
  }
}

/* u2_cf_flat_save(): save `som` as `mod` at `pas`. 
*/
u2_bean
u2_cf_flat_save(u2_noun mod, 
                u2_noun pas,
                u2_noun som)
{
  c3_assert(c3__atom == mod);
  {
    c3_c*       pas_c = u2_cr_string(pas);
    c3_i    fid_i;
    c3_w    fln_w;
    c3_y*   fil_y;

    fid_i = open(pas_c, O_WRONLY | O_CREAT, 0666);
    free(pas_c);
    u2_cz(pas);

    if ( fid_i < 0 ) {
      perror(pas_c);
      u2_cz(som);
      return u2_no;
    }

    fln_w = u2_met(3, som);
    fil_y = malloc(fln_w);
    u2_cr_bytes(0, fln_w, fil_y, som);
    u2_cz(som);
 
    if ( fln_w != write(fid_i, fil_y, fln_w) ) {
      return u2_no;
    }
    close(fid_i);

    return u2_yes;
  }
}

#if 1
/* u2_cn_mung():
**
**   Call `(function sample)`.
*/
u2_noun
u2_cn_mung(u2_noun fun,
           u2_noun sam)
{
  u2_noun pro = u2_bn_mong(u2_Wire, fun, sam);

  u2_cz(fun);
  return pro;
}
#endif

/* u2_ci_string():
**
**   u2_ci_bytes(strlen(a_c), (c3_y *)a_c);
*/
u2_atom
u2_ci_string(const c3_c* a_c)
{
  return u2_bn_string(u2_Wire, a_c);
}

/* u2_ci_mp(): construct atom from GMP.  Caller transfers a_mp.
*/
u2_atom
u2_ci_mp(mpz_t a_mp)
{
  return u2_rl_mp(u2_Wire, a_mp);
}

/* u2_ci_tape(): from a C string, to a list of bytes.
*/
u2_atom
u2_ci_tape(const c3_c* txt_c)
{
  if ( !*txt_c ) {
    return u2_nul;
  } else return u2nc(*txt_c, u2_ci_tape(txt_c + 1));
}

/* u2_cr_string(): `a` as malloced C string.
*/
c3_c* 
u2_cr_string(u2_atom a)
{
  c3_w  met_w = u2_cr_met(3, a);
  c3_c* str_c = malloc(met_w + 1);

  u2_cr_bytes(0, met_w, (c3_y*)str_c, a);
  str_c[met_w] = 0;
  return str_c;
}

/* u2_cr_tape(): `a`, a list of bytes, as malloced C string.
*/
c3_y* 
u2_cr_tape(u2_noun a)
{
  u2_noun b;
  c3_w    i_w;
  c3_y    *a_y;

  for ( i_w = 0, b=a; u2_yes == u2du(b); i_w++, b=u2t(b) )
    ;
  a_y = malloc(i_w + 1);

  for ( i_w = 0, b=a; u2_yes == u2du(b); i_w++, b=u2t(b) ) {
    a_y[i_w] = u2h(b);
  }
  a_y[i_w] = 0;

  return a_y;
}

/* u2_ci_bytes():
**
**   Construct `a` bytes from `b`, LSB first, as an atom.
*/
u2_atom
u2_ci_bytes(c3_w        a_w,
            const c3_y* b_y)
{
  return u2_bn_bytes(u2_Wire, a_w, b_y);
}

/* u2_ci_words():
**
**   Construct `a` words from `b`, LSW first, as an atom.
*/
u2_atom
u2_ci_words(c3_w        a_w,
            const c3_w* b_w)
{
  return u2_bn_words(u2_Wire, a_w, b_w);
}

/* u2_ci_chubs():
**
**   Construct `a` double-words from `b`, LSD first, as an atom.
*/
u2_atom
u2_ci_chubs(c3_w        a_w,
            const c3_d* b_d)
{
  //  XX considerably suboptimal
  {
    c3_w *b_w = malloc(a_w * 8);
    c3_w i_w;
    u2_atom p;

    for ( i_w = 0; i_w < a_w; i_w++ ) {
      b_w[(2 * i_w)] = b_d[i_w] & 0xffffffffULL;
      b_w[(2 * i_w) + 1] = b_d[i_w] >> 32ULL;
    }
    p = u2_ci_words((a_w * 2), b_w);
    free(b_w);
    return p;
  }
}

/* u2_cm_trip(): descend into a memory region.
**
**   Memory allocated in the heap above is senior & frozen.
*/
void
u2_cm_trip()
{
#if 0
  if ( u2_no == u2_rl_leap(u2_Wire, c3__rock) ) {
    u2_cm_bail(c3__fail);
  }
#endif
}

/* u2_cm_chin(): ascend out of a memory region.
**
**   Memory allocated in the heap below is junior & volatile.
*/
void
u2_cm_chin()
{
#if 0
  u2_rl_fall(u2_Wire);
#endif
}

/* u2_cm_bury(): store fresh or volatile noun `som` to freezer.
*/
u2_weak
u2_cm_bury(u2_weak som)
{
  if ( u2_none == som ) return som;

  if ( u2_no == u2_rl_junior(u2_wire_bas_r(u2_Wire), som) ) {
    return som;
  }
  else {
    u2_noun pro = u2_rl_take(u2_wire_bas_r(u2_Wire), som);

    u2_cz(som);
    return pro;
  }
}

/* u2_cm_rind(): open and produce a new jump buffer.
*/
void*
u2_cm_rind()
{
  u2_ray kit_r = u2_rl_ralloc(u2_Wire, c3_wiseof(u2_loom_kite));

  u2_kite_par_r(kit_r) = u2_wire_kit_r(u2_Wire);
  u2_wire_kit_r(u2_Wire) = kit_r;

  //  Save the old stack and actions.
  //
  u2_kite_tax(kit_r) = u2k(u2_wire_tax(u2_Wire));
  u2_kite_don(kit_r) = u2k(u2_wrac_at(u2_Wire, duz.don)); 

  return u2_at_cord(u2_kite_buf_r(kit_r), c3_wiseof(jmp_buf));
}

/* _cm_jack(): steal the trace as of the current kite.
*/
static u2_noun
_cm_jack(u2_noun old, u2_noun nuw)
{
  u2_noun cur = nuw;

  if ( nuw == old ) {
    u2z(old); return u2_nul;
  }
  else while ( 1 ) {
    if ( u2ft(cur) == old ) {
      u2z(old);
      u2ft(cur) = u2_nul;
      
      return nuw;
    } else {
      cur = u2ft(cur);
    }
  }
}

/* _cm_depth()
*/
static c3_w
_cm_depth(u2_noun old, u2_noun nuw)
{
  c3_w dep_w = 0;

  while ( nuw != old ) {
    c3_assert(u2_yes == u2du(nuw)); 
    nuw = u2t(nuw);

    dep_w++;
  }
  return dep_w;
}

/* u2_cm_wail(): produce and reset the local trace, without bailing.
*/
u2_noun 
u2_cm_wail()
{
  u2_ray  kit_r = u2_wire_kit_r(u2_Wire);
  u2_noun old   = u2_kite_tax(u2_wire_kit_r(u2_Wire));
  u2_noun nuw   = u2_wire_tax(u2_Wire);
  u2_noun jaq;
 
  jaq = _cm_jack(old, nuw);

  // c3_assert(1 == u2_rl_refs(u2_Wire, old));
  u2_wire_tax(u2_Wire) = old;
  u2_kite_tax(kit_r) = u2k(old);
  // c3_assert(1 == u2_rl_refs(u2_Wire, jaq));

  return jaq;
}

static c3_w _num = 0;

/* u2_cm_bail(): bail out to the local trap.  Does not return.
*/
  extern u2_noun u2_Flag_Abort;
u2_noun
u2_cm_bail(c3_l how_l)
{
  u2_ray kit_r = u2_wire_kit_r(u2_Wire);

  if ( u2_yes == u2_Flag_Abort ) {
    if ( c3__fail == how_l ) { c3_assert(0); }
    c3_assert(0);
  }
  u2_tx_sys_bit(u2_Wire, u2_yes);

  // fprintf(stderr, "bail\n");
  // if ( _num == 0 ) { c3_assert(0); } else _num--;

  {
    u2_noun jaq;
    jmp_buf buf_f;

    // Reset the old stack trace, pulling off the local top.
    //
    jaq = u2_cm_wail();

    // Reset the old action trace.
    {
      u2z(u2_wrac_at(u2_Wire, duz.don));
      u2_wrac_at(u2_Wire, duz.don) = u2_kite_don(kit_r);
    }

    // Copy out the jump buffer; free the old kite.
    {
      memcpy((void *)buf_f,
             u2_at_cord(u2_kite_buf_r(kit_r), c3_wiseof(jmp_buf)),
             sizeof(jmp_buf));

      u2_wire_kit_r(u2_Wire) = u2_kite_par_r(kit_r);
      u2_rl_rfree(u2_Wire, kit_r);
    }
    
    // Longjmp with the how-trace pair.  XX: no workee with 64-bit nouns.
    //
    {
      _longjmp(buf_f, u2nc(how_l, jaq));
    }
  }
  return 0;
}

/* u2_cm_bowl(): bail out with preset report.
*/
u2_noun
u2_cm_bowl(u2_noun how)
{
  u2_ray kit_r = u2_wire_kit_r(u2_Wire);

  u2_tx_sys_bit(u2_Wire, u2_yes);

  {
    u2_noun jaq;
    jmp_buf buf_f;

    // Reset the old stack trace, pulling off the local top.
    //
    jaq = u2_cm_wail();

    // Reset the old action trace.
    {
      u2z(u2_wrac_at(u2_Wire, duz.don));
      u2_wrac_at(u2_Wire, duz.don) = u2_kite_don(kit_r);
    }

    // Copy out the jump buffer; free the old kite.
    {
      memcpy((void *)buf_f,
             u2_at_cord(u2_kite_buf_r(kit_r), c3_wiseof(jmp_buf)),
             sizeof(jmp_buf));

      u2_wire_kit_r(u2_Wire) = u2_kite_par_r(kit_r);
      u2_rl_rfree(u2_Wire, kit_r);
    }
    
    // Longjmp with the how-trace pair.  XX: no workee with 64-bit nouns.
    //
    {
      u2z(jaq);
      _longjmp(buf_f, how);
    }
  }
  return 0;
}

/* u2_cm_done(): terminate trap.
*/
void
u2_cm_done()
{
  u2_ray kit_r = u2_wire_kit_r(u2_Wire);

  c3_assert(kit_r != 0);

  u2z(u2_kite_tax(kit_r));
  u2z(u2_kite_don(kit_r));
  u2_wire_kit_r(u2_Wire) = u2_kite_par_r(kit_r);

  u2_rl_rfree(u2_Wire, kit_r);
}

/* u2_cm_sweep(): return bytes leaked; match bytes saved.
*/
c3_w
u2_cm_sweep(c3_w sav_w)
{
  return u2_rl_gc_sweep(u2_Wire, sav_w);
}

/* u2_cm_purge(): purge memo cache.
*/
void
u2_cm_purge()
{
  u2_rl_drain(u2_Wire);
}

/* u2_cm_mark_noun(): mark individual noun.
*/
c3_w
u2_cm_mark_noun(u2_noun som)
{
  return u2_rl_gc_mark_noun(u2_Wire, som);
}

/* u2_cm_mark_ray(): mark a root for gc (do not use again before gc)
*/
c3_w
u2_cm_mark_ray(u2_ray ray_r)
{
  return u2_rl_gc_mark_ptr(u2_Wire, ray_r);
}

/* u2_cm_mark_internal(): mark all coal internals
*/
c3_w
u2_cm_mark_internal()
{
  return u2_wr_mark(u2_Wire);
}
 
/* u2_cm_trac(): extract and clear stack trace.
*/
u2_noun
u2_cm_trac()
{
  u2_noun tax = u2_wire_tax(u2_Wire);

  u2_wire_tax(u2_Wire) = u2_nul;
  return tax;
}

/* u2_cm_push(): push `mon` on trace stack.
*/
void
u2_cm_push(u2_noun mon)
{
  u2_wire_tax(u2_Wire) = u2nc(mon, u2_wire_tax(u2_Wire));
}

/* u2_cm_bean(): push `[%bean roc]` on trace stack.
*/
void
u2_cm_bean(u2_noun roc)
{
  u2_cm_push(u2nc(c3__bean, roc));
}

/* u2_cm_drop(): drop from meaning stack.
*/
void
u2_cm_drop()
{
  u2_noun tax = u2_wire_tax(u2_Wire);

  c3_assert(u2_nul != tax);
  u2_wire_tax(u2_Wire) = u2_ct(u2t(tax));
  u2_cz(tax);
}

/* u2_cm_foul():
*/
u2_noun
u2_cm_foul(const c3_c* err_c)
{
  u2_cm_bean(u2_ci_string(err_c));
  fprintf(stderr, "foul: %s\n", err_c);

  return u2_bl_error(u2_Wire, err_c); 
}

/* u2_cn_cell(): produce the cell `[a b]`.
*/
u2_noun
u2_cn_cell(u2_noun a,
           u2_noun b)
{
  return u2_bn_cell(u2_Wire, a, b);
}

/* u2_cn_trel(): produce the cell `[a b c]`.
*/
u2_noun
u2_cn_trel(u2_noun a,
           u2_noun b,
           u2_noun c)
{
  return u2_bn_trel(u2_Wire, a, b, c);
}

/* u2_cn_qual(): produce the cell `[a b c d]`.
*/
u2_noun
u2_cn_qual(u2_noun a,
           u2_noun b,
           u2_noun c,
           u2_noun d)
{
  return u2_bn_qual(u2_Wire, a, b, c, d);
}

/* u2_cka_add(): a + b.
*/
u2_noun
u2_cka_add(u2_noun a, u2_noun b)
{
  u2_noun c = j2_mbc(Pt1, add)(u2_Wire, a, b);

  u2_cz(a); u2_cz(b);
  return c;
}

/* u2_cka_sub(): a + b.
*/
u2_noun
u2_cka_sub(u2_noun a, u2_noun b)
{
  u2_noun c = j2_mbc(Pt1, sub)(u2_Wire, a, b);

  u2_cz(a); u2_cz(b);
  return c;
}

/* u2_cka_gth(): a + b.
*/
u2_noun
u2_cka_gth(u2_noun a, u2_noun b)
{
  u2_noun c = j2_mbc(Pt1, gth)(u2_Wire, a, b);

  u2_cz(a); u2_cz(b);
  return c;
}

/* u2_cka_mul(): a * b.
*/
u2_noun
u2_cka_mul(u2_noun a, u2_noun b)
{
  u2_noun c = j2_mbc(Pt1, mul)(u2_Wire, a, b);

  u2_cz(a); u2_cz(b);
  return c;
}

/* u2_cka_lte(): a * b.
*/
u2_noun
u2_cka_lte(u2_noun a, u2_noun b)
{
  u2_noun c = j2_mbc(Pt1, lte)(u2_Wire, a, b);

  u2_cz(a); u2_cz(b);
  return c;
}

/* u2_ckb_lent(): length of list `a`.
*/
u2_noun
u2_ckb_lent(u2_noun a)
{
  u2_noun b = j2_mbc(Pt2, lent)(u2_Wire, a);

  u2_cz(a);
  return b;
}

/* u2_ckb_flop(): reverse list `a`.
*/
u2_noun
u2_ckb_flop(u2_noun a)
{
  u2_noun b = j2_mbc(Pt2, flop)(u2_Wire, a);

  u2_cz(a);
  return b;
}

/* u2_ckb_weld(): concatenate lists `a` before `b`.
*/
u2_noun
u2_ckb_weld(u2_noun a, u2_noun b)
{
  u2_noun c = j2_mbc(Pt2, weld)(u2_Wire, a, b);

  u2_cz(a); u2_cz(b);
  return c;
}

/* u2_ckc_lsh(): left shift.
*/
u2_noun
u2_ckc_lsh(u2_noun a, u2_noun b, u2_noun c)
{
  u2_noun d = j2_mbc(Pt3, lsh)(u2_Wire, a, b, c);

  u2_cz(a); u2_cz(b); u2_cz(c);
  return d;
}

/* u2_ckc_rsh(): right shift.
*/
u2_noun
u2_ckc_rsh(u2_noun a, u2_noun b, u2_noun c)
{
  u2_noun d = j2_mbc(Pt3, rsh)(u2_Wire, a, b, c);

  u2_cz(a); u2_cz(b); u2_cz(c);
  return d;
}

/* u2_ckd_by_get(): map get for key `b` in map `a` with u2_none.
*/
u2_weak
u2_ckd_by_get(u2_noun a, u2_noun b)
{
  u2_noun c = _coal_by_get(u2_Wire, a, b);

  u2_cz(a); u2_cz(b);
  if ( u2_no == u2_cr_du(c) ) {
    u2_cz(c); 
    return u2_none;
  } else {
    u2_noun pro = u2_ct(u2t(c));

    u2_cz(c);
    return pro;
  }
}

/* u2_ckd_by_got(): map get for key `b` in map `a` with fail.
*/
u2_noun
u2_ckd_by_got(u2_noun a, u2_noun b)
{
  u2_weak c = u2_ckd_by_get(a, b);

  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  } 
  else return c;
}

/* u2_ckd_by_put(): map put for key `b`, value `c` in map `a`.
*/
u2_weak
u2_ckd_by_put(u2_noun a, u2_noun b, u2_noun c)
{
  // Bizarre asymmetry in old jets.
  //
  // (Mysterious comment in old glue code.)
  //
  u2_noun pro = _coal_by_put(u2_Wire, a, b, c);

  u2_cz(a); u2_cz(b); u2_cz(c);
  return pro;
}

/* u2_ckd_by_gas(): list to map.
*/
u2_noun
u2_ckd_by_gas(u2_noun a, u2_noun b)
{
  u2_weak c = _coal_by_gas(u2_Wire, a, b);

  u2_cz(a); u2_cz(b);
  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  } 
  else return c;
}

/* u2_ckd_in_gas(): list to map.
*/
u2_noun
u2_ckd_in_gas(u2_noun a, u2_noun b)
{
  u2_weak c = _coal_in_gas(u2_Wire, a, b);

  u2_cz(a); u2_cz(b);
  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  } 
  else return c;
}

/* u2_ckd_by_has(): test for presence.
*/
u2_bean
u2_ckd_by_has(u2_noun a, u2_noun b)
{
  u2_weak c = _coal_by_has(u2_Wire, a, b);

  u2_cz(a); u2_cz(b);
  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  } 
  else return c;
}

/* u2_ckd_in_has(): test for presence.
*/
u2_bean
u2_ckd_in_has(u2_noun a, u2_noun b)
{
  u2_weak c = _coal_in_has(u2_Wire, a, b);

  u2_cz(a); u2_cz(b);
  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  } 
  else return c;
}

/* u2_ckd_in_tap(): map/set convert to list.  (solves by_tap also.)
*/
u2_noun
u2_ckd_in_tap(u2_noun a, u2_noun b)
{
  u2_weak c = _coal_in_tap(u2_Wire, a, b);

  u2_cz(a); u2_cz(b);
  if ( u2_none == c ) {
    return u2_cm_bail(c3__exit);
  } 
  else return c;
}

/* u2_cke_cue(): expand saved pill.
*/
  static u2_noun                                                  //  produce
  _cue_in(u2_wire wir_r,
          u2_atom a,                                              //  retain
          u2_atom b,                                              //  retain
          u2_ray  t_r)                                            //  retain
  {
    u2_noun p, q;

    if ( _0 == j2_mbc(Pt3, cut)(wir_r, 0, b, 1, a) ) {
      u2_noun x = j2_mbc(Pt1, inc)(wir_r, b);
      u2_noun c = j2_mby(Pt5, rub)(wir_r, x, a);

      p = j2_mbc(Pt1, inc)(wir_r, u2_h(c));
      q = u2_rx(wir_r, u2_t(c));
      q = u2_cs_save(wir_r, t_r, 0, b, q);

      u2_rz(wir_r, c);
      u2_rz(wir_r, x);
    }
    else {
      u2_noun c = j2_mbc(Pt1, add)(wir_r, _2, b);
      u2_noun l = j2_mbc(Pt1, inc)(wir_r, b);

      if ( _0 == j2_mbc(Pt3, cut)(wir_r, 0, l, 1, a) ) {
        u2_noun u, v, w;
        u2_noun x, y;

        u = _cue_in(wir_r, a, c, t_r);
        x = j2_mbc(Pt1, add)(wir_r, u2_h(u), c);
        v = _cue_in(wir_r, a, x, t_r);

        w = u2_bc(wir_r, u2_rx(wir_r, u2_t(u)),
                         u2_rx(wir_r, u2_t(v)));

        y = j2_mbc(Pt1, add)(wir_r, u2_h(u), u2_h(v));

        p = j2_mbc(Pt1, add)(wir_r, _2, y);
        q = u2_cs_save(wir_r, t_r, 0, b, w);

        u2_rz(wir_r, u); u2_rz(wir_r, v); u2_rz(wir_r, x); u2_rz(wir_r, y);
      }
      else {
        u2_noun d = j2_mby(Pt5, rub)(wir_r, c, a);
        u2_weak x = u2_cs_find(wir_r, t_r, 0, u2_t(d));

        p = j2_mbc(Pt1, add)(wir_r, _2, u2_h(d));

        if ( u2_none == x ) {
          return u2_bl_bail(wir_r, c3__fail);
        }
        q = u2_rx(wir_r, x);

        u2_rz(wir_r, d);
      }
      u2_rz(wir_r, l);
      u2_rz(wir_r, c);
    }
    return u2_bc(wir_r, p, q);
  }

  u2_noun                                                         //  transfer
  _cue_internal(u2_wire wir_r, 
                u2_atom a)                                        //  retain
  {
    u2_ray  t_r = u2_cs_make(wir_r);
    u2_noun x   = _cue_in(wir_r, a, _0, t_r);
    u2_noun y   = u2_rx(wir_r, u2_t(x));

    u2_rz(wir_r, x);
    u2_cs_free(wir_r, t_r);

    return y;
  }

u2_noun
u2_cke_cue(u2_atom a)
{
  u2_noun b = _cue_internal(u2_Wire, a);

  u2_cz(a);
  return b;
}

/* u2_cke_jam(): pack noun as atom.
*/
u2_atom
u2_cke_jam(u2_noun a)
{
  u2_atom b = _coal_jam(u2_Wire, a);

  u2_cz(a);
  return b;
}

/* u2_cke_trip(): atom to tape.
*/
u2_atom
u2_cke_trip(u2_noun a)
{
  u2_atom b = _coal_trip(u2_Wire, a);

  u2_cz(a);
  return b;
}
