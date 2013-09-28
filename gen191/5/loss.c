/* j/5/loss.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* functions
*/
  typedef struct _u2_loss {                 //  loss problem
    u2_noun hel;                            //  a as a list
    c3_w lel_w;                             //  length of a 
    c3_w lev_w;                             //  length of b
    u2_noun* hev;                           //  b as an array
    u2_noun sev;                            //  b as a set of lists
    c3_w kct_w;                             //  candidate count
    u2_noun* kad;                           //  candidate array
  } u2_loss;

  //  free loss object
  //
  static void 
  _flem(u2_loss* loc_u)
  {
    u2z(loc_u->sev);
    {
      c3_w i_w;

      for ( i_w = 0; i_w < loc_u->kct_w; i_w++ ) {
        u2z(loc_u->kad[i_w]);
      }
    }
    free(loc_u->hev);
    free(loc_u->kad);
  }

  //  extract lcs  -  XX don't use the stack like this
  //
  static u2_noun
  _lext(u2_loss* loc_u, u2_noun kad)
  {
    if ( u2_nul == kad ) {
      return u2_nul;
    } else {
      return u2nc(u2k(loc_u->hev[u2_cr_word(0, u2h(kad))]), 
                  _lext(loc_u, u2t(kad)));
    }
  }

  //  extract lcs 
  //
  static u2_noun
  _lexs(u2_loss* loc_u)
  {
    if ( 0 == loc_u->kct_w ) {
      return u2_nul;
    } else return u2_ckb_flop(_lext(loc_u, loc_u->kad[loc_u->kct_w - 1]));
  }

  //  initialize loss object
  //
  static void
  _lemp(u2_loss* loc_u, 
        u2_noun  hel,                                             //  retain 
        u2_noun  hev)                                             //  retain
  {
    loc_u->hel = hel;
    loc_u->lel_w = u2_ckb_lent(u2k(hel));

    //  Read hev into array.
    {
      c3_w i_w;

      loc_u->hev = malloc(u2_ckb_lent(u2k(hev)) * sizeof(u2_noun));

      for ( i_w = 0; u2_nul != hev; i_w++ ) {
        loc_u->hev[i_w] = u2h(hev);
        hev = u2t(hev);
      }
      loc_u->lev_w = i_w;
    }
    loc_u->kct_w = 0;
    loc_u->kad = malloc(
                              (1 + c3_min(loc_u->lev_w, loc_u->lel_w)) *
                              sizeof(u2_noun));

    //  Compute equivalence classes.
    //
    loc_u->sev = u2_nul;
    {
      c3_w i_w;

      for ( i_w = 0; i_w < loc_u->lev_w; i_w++ ) {
        u2_noun how = loc_u->hev[i_w];
        u2_weak hav = u2_ckd_by_get(u2k(loc_u->sev), u2k(how));
        u2_noun teg = u2nc(u2_ci_words(1, &i_w), 
                          (hav == u2_none) ? u2_nul : hav);

        loc_u->sev = u2_ckd_by_put(loc_u->sev, u2k(how), teg);
      }
    }
  }

  //  apply
  //
  static void
  _lune(u2_loss* loc_u,
        c3_w     inx_w,
        c3_w     goy_w)
  {
    u2_noun kad;

    kad = u2nc(u2_ci_words(1, &goy_w),
               (inx_w == 0) ? u2_nul
                            : u2k(loc_u->kad[inx_w - 1]));
    if ( loc_u->kct_w == inx_w ) {
      c3_assert(loc_u->kct_w < (1 << 31));
      loc_u->kct_w++;
    } else {
      u2z(loc_u->kad[inx_w]);
    }
    loc_u->kad[inx_w] = kad;
  } 

  //  extend fits top
  //
  static u2_bean
  _hink(u2_loss* loc_u,
        c3_w     inx_w,
        c3_w     goy_w)
  {
    return u2_say
         ( (loc_u->kct_w == inx_w) ||
           (u2_cr_word(0, u2h(loc_u->kad[inx_w])) > goy_w) );
  }

  //  extend fits bottom
  //
  static u2_bean
  _lonk(u2_loss* loc_u,
        c3_w     inx_w,
        c3_w     goy_w)
  {
    return u2_say
      ( (0 == inx_w) ||
        (u2_cr_word(0, u2h(loc_u->kad[inx_w - 1])) < goy_w) );
  }

#if 0
  //  search for first index >= inx_w and <= max_w that fits
  //  the hink and lonk criteria.
  //
  static u2_bean
  _binka(u2_loss* loc_u,
         c3_w*    inx_w,
         c3_w     max_w,
         c3_w     goy_w)
  {
    while ( *inx_w <= max_w ) {
      if ( u2_no == _lonk(loc_u, *inx_w, goy_w) ) {
        return u2_no;
      }
      if ( u2_yes == _hink(loc_u, *inx_w, goy_w) ) {
        return u2_yes;
      }
      else ++*inx_w;
    }
    return u2_no;
  }
#endif

  //  search for lowest index >= inx_w and <= max_w for which
  //  both hink(inx_w) and lonk(inx_w) are true.  lonk is false
  //  if inx_w is too high, hink is false if it is too low.
  //
  static u2_bean
  _bink(u2_loss* loc_u,
        c3_w*    inx_w,
        c3_w     max_w,
        c3_w     goy_w)
  {
    c3_assert(max_w >= *inx_w);

    if ( max_w == *inx_w ) {
      if ( u2_no == _lonk(loc_u, *inx_w, goy_w) ) {
        return u2_no;
      }
      if ( u2_yes == _hink(loc_u, *inx_w, goy_w) ) {
        return u2_yes;
      }
      else {
        ++*inx_w;
        return u2_no;
      }
    } 
    else {
      c3_w mid_w = *inx_w + ((max_w - *inx_w) / 2);

      if ( (u2_no == _lonk(loc_u, mid_w, goy_w)) ||
           (u2_yes == _hink(loc_u, mid_w, goy_w)) ) 
      {
        return _bink(loc_u, inx_w, mid_w, goy_w);
      } else {
        *inx_w = mid_w + 1;
        return _bink(loc_u, inx_w, max_w, goy_w);
      }
    }
  }


  static void
  _merg(u2_loss* loc_u,
        c3_w     inx_w,
        u2_noun  gay)
  {
    if ( (u2_nul == gay) || (inx_w > loc_u->kct_w) ) {
      return;
    }
    else {
      u2_noun i_gay = u2h(gay); 
      c3_w    goy_w = u2_cr_word(0, i_gay);
      u2_noun bik;

      bik = _bink(loc_u, &inx_w, loc_u->kct_w, goy_w);
      
      if ( u2_yes == bik ) {
        _merg(loc_u, inx_w + 1, u2t(gay));
        _lune(loc_u, inx_w, goy_w);
      }
      else {
        _merg(loc_u, inx_w, u2t(gay));
      }
    }
  }

  //  compute lcs
  //
  static void
  _loss(u2_loss* loc_u)
  {
    while ( u2_nul != loc_u->hel ) {
      u2_noun i_hel = u2h(loc_u->hel);
      u2_weak guy   = u2_ckd_by_get(u2k(loc_u->sev), u2k(i_hel));

      if ( u2_none != guy ) {
        u2_noun gay = u2_ckb_flop(u2k(guy));
         
        _merg(loc_u, 0, gay);
        u2z(gay);
      }

      loc_u->hel = u2t(loc_u->hel);
    }
  }

  u2_noun                                                         //  produce
  j2_mbc(Pt5, loss)(u2_wire wir_r, 
                    u2_noun hel,                                  //  retain
                    u2_noun hev)                                  //  retain
  {
    u2_loss loc_u;
    u2_noun lcs;
 
    _lemp(&loc_u, hel, hev);
    _loss(&loc_u);
    lcs = _lexs(&loc_u);

    _flem(&loc_u);
    return lcs;
  }

  static u2_bean                                                
  _listp(u2_noun lix)                                             //  retain
  {
    while ( 1 ) { 
      if ( u2_nul == lix ) return u2_yes;
      if ( u2_no == u2du(lix) ) return u2_no;
      lix = u2t(lix);
    }
  }

  u2_weak                                                         //  produce
  j2_mb(Pt5, loss)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun hel, hev;

    if ( (u2_none == (hel = u2_frag(u2_cv_sam_2, cor))) ||
         (u2_none == (hev = u2_frag(u2_cv_sam_3, cor))) ||
         (u2_no == _listp(hel)) ||
         (u2_no == _listp(hev)) ) 
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mbc(Pt5, loss)(wir_r, hel, hev);
    }
  }


/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt5, loss)[] = { 
    { ".2", c3__lite, j2_mb(Pt5, loss), Tier5, u2_none, u2_none },
    { }
  };
