/* f/dash.c
**
** This file is in the public domain.
*/
#include "all.h"

/* _ds_mate(): u2_yes iff `xip` binds to `cor`.
*/
static u2_bean
_ds_mate(u2_noun xip,                                             //  retain
         u2_noun cor)                                             //  retain
{
  u2_noun dac, bat, pet;

  u2_as_trel(xip, &dac, &bat, &pet);

  //  Very important for performance that we mate top-down.
  //  Reason: duplicates are much more common in shallower
  //  batteries, and duplicate comparison is always slow.
  //
  if ( u2_nul != pet ) {
    u2_noun axe = u2_h(pet);
    u2_noun led = u2_t(pet);
    u2_noun ruc = u2_frag(axe, cor);
 
    if ( (u2_none == ruc) || (u2_no == _ds_mate(led, ruc)) ) {
      return u2_no;
    }
  } 

  return u2_sing(bat, u2_h(cor));
}

/* _ds_scan(): linear search for matching chip.
*/
static u2_noun                                                    //  discover
_ds_scan(u2_noun pug,                                             //  retain
         u2_noun cor)                                             //  retain
{
  while ( u2_nul != pug ) {
    u2_noun i_pug = u2_h(pug);

    if ( u2_yes == _ds_mate(i_pug, cor) ) {
      return i_pug;
    }
    pug = u2_t(pug);
  }
  return u2_none;
}

/* u2_ds_find(): find chip by core, or none.
*/
u2_weak                                                           //  senior
u2_ds_find(u2_wire wir_r,
           u2_noun cor)                                           //  retain
{
  if ( u2_no == u2_dust(cor) ) {
    return u2_none;
  } else {
    u2_rail bas_r = u2_wire_bas_r(wir_r);
    u2_noun pug = u2_cs_find(bas_r, u2_wire_des_r(wir_r), 0, u2_h(cor));
    u2_noun out;

    if ( u2_none == pug ) {
      out = u2_none;
    }
    else out = _ds_scan(pug, cor);

    if ( (u2_none == out) && (u2_none != pug) ) {
      fprintf(stderr, "half match\r\n");
    }
    return out;
  }
}

/* _ds_good_cop()::
*/
static u2_bean
_ds_good_cop(u2_noun cop)
{
  c3_w i_w = 0;

  while ( i_w < 4 ) {
    if ( u2_yes == u2_stud(cop) ) {
      return u2_yes;
    } 
    if ( u2_no == u2_stud(u2_h(cop)) ) {
      return u2_no;
    }
    cop = u2_t(cop);
    i_w++;
  }
  return u2_no;
}

/* _ds_good_bud()::
*/
static u2_bean
_ds_good_bud(u2_noun bud)
{
  u2_noun p_bud, q_bud;

  if ( _0 == bud ) {
    return u2_yes;
  }
  else if ( (u2_no == u2_as_cell(bud, &p_bud, &q_bud)) ) {
    return u2_no;
  }
  if ( (u2_nock_bone == p_bud) && (_0 == q_bud) ) {
    return u2_yes;
  }
  if ( (u2_nock_frag == p_bud) && (u2_yes == u2_stud(q_bud)) ) {
    return u2_yes;
  }
  if ( u2_nock_hint == p_bud ) {
    return u2_yes == u2_dust(u2_t(bud)) ? _ds_good_bud(u2_t(u2_t(bud))) : u2_no;
  }
  else return u2_no;
}

/* _ds_good_pic()::
*/
static u2_bean
_ds_good_pic(u2_noun pic)
{
  if ( u2_nul == pic ) {
    return u2_yes;
  } else {
    u2_noun i_pic, t_pic;
    u2_noun pi_pic, qi_pic;

    if ( (u2_no == u2_as_cell(pic, &i_pic, &t_pic)) ||
         (u2_no == u2_as_cell(i_pic, &pi_pic, &qi_pic)) ||
         (u2_no == u2_stud(pi_pic)) )
    {
      return u2_no;
    }
    else return u2_yes;
  }
}

extern void u2_lo_show(c3_c* cap_c, u2_noun nun);

/* _ds_chip(): fabricate chip from clue and core.
*/
static u2_weak                                                    //  senior
_ds_chip(u2_wire wir_r,
         u2_noun clu,                                             //  retain
         u2_noun cor)                                             //  retain
{
  u2_rail bas_r = u2_wire_bas_r(wir_r);
  u2_noun bud_clu, cop_clu, pic_clu;

  if ( (u2_no == u2_as_trel(clu, &bud_clu, &cop_clu, &pic_clu)) ||
       (u2_no == _ds_good_bud(bud_clu)) ||
       (u2_no == _ds_good_cop(cop_clu)) ||
       (u2_no == _ds_good_pic(pic_clu)) )
  {
    return u2_none;
  }
  else {
    u2_noun dac, bat, pet;
 
    /* disc: dac
    */
    {
      if ( u2_none == (dac = u2_rx(bas_r, u2_t(clu))) ) {
        u2_ho_warn_here();
        return u2_none;
      }
    }
#if 1
    /* battery: bat
    */
    {
      if ( u2_none == (bat = u2_rx(bas_r, u2_h(cor))) ) {
        u2_ho_warn_here();
        u2_rz(bas_r, dac); return u2_none;
      }
    }
#endif
#if 0
    /* bat: battery
    */
    {
      // Important to reuse existing battery even if it does not match
      // the whole chip - since battery is a comparison key, we don't
      // want duplicates, which compare slowly.
      //
      if ( u2_nul == pug ) {
        bat = u2_rx(bas_r, u2_h(cor));
      }
      else {
        u2_noun i_pug = u2_h(pug);
        bat = u2_rx(bas_r, u2_h(u2_t(i_pug)));
      }

      if ( u2_none == bat ) {
        u2_ho_warn_here();
        u2_rz(bas_r, dac); return u2_none;
      }
    }
#endif
    /* trunk: pet
    */
    {
      if ( _0 == bud_clu ) {
        pet = u2_nul;
      } 
      else {
        while ( _10 == u2_h(bud_clu) ) {
          bud_clu = u2_t(u2_t(bud_clu));
        }

        if ( _1 == u2_h(bud_clu) ) {
          pet = u2_nul;
        }
        else {
          u2_atom axe = u2_t(bud_clu);
          u2_noun ruc = u2_frag(axe, cor);
          u2_noun led;

          if ( u2_none == ruc ) {
            // u2_err(wir_r, "clu", clu);
            u2_ho_warn_here();
            u2_rz(bas_r, dac); u2_rz(bas_r, bat); return u2_none;
          } else {
            if ( u2_none == (led = u2_ds_find(wir_r, ruc)) ) {
              u2_lo_show("clu", clu);
              u2_ho_warn_here();
              c3_assert(0);
              u2_rz(bas_r, dac); u2_rz(bas_r, bat); return u2_none;
            }
            pet = u2_rc(bas_r, u2_rx(bas_r, axe), u2_rx(bas_r, led));
          }
        }
      }
    }
    return u2_rt(bas_r, dac, bat, pet);
  }
}

/* u2_ds_mine(): 
**
**   Register and/or save core.
*/
u2_noun                                                           //  transfer
u2_ds_mine(u2_wire wir_r,
           u2_noun clu,                                           //  retain
           u2_noun cor)                                           //  transfer
{
  u2_noun bas_r = u2_wire_bas_r(wir_r);

  if ( u2_no == u2_dust(cor) ) {
    return cor;
  } else {
    u2_noun pay = u2_t(cor);
    u2_noun bat = u2_h(cor);
    u2_noun pug = u2_cs_find(bas_r, u2_wire_des_r(wir_r), 0, bat);
    u2_noun xip, bat_xip;
    u2_noun gop;

    if ( u2_none == pug ) {
      pug = u2_nul;
    }
    if ( u2_none == (xip = _ds_scan(pug, cor)) ) {
      gop = u2_rc(bas_r, (xip = _ds_chip(wir_r, clu, cor)), u2_rx(bas_r, pug));

      if ( u2_none == gop ) {
        return cor;
      } else {
        bat_xip = u2_h(u2_t(xip));

#if 0
        {  
          c3_c* xip_c = u2_ho_cstring(xip);

          fprintf(stderr, "!%s - lent %d\r\n", xip_c, u2_ckb_lent(gop));
          free(xip_c);
        }
#endif
        gop = u2_cs_save(bas_r, u2_wire_des_r(wir_r), 0, bat_xip, gop);
        {
          u2_noun poo = u2_cs_find(bas_r, u2_wire_des_r(wir_r), 0, bat_xip);
         
          {
            _ds_scan(poo, cor);
          }
        }
        u2_rz(bas_r, gop);

      }
    }
    else {
      bat_xip = u2_h(u2_t(xip));
    }

    if ( bat_xip != bat ) {
      u2_noun cyr = u2_rc(wir_r, bat_xip, u2_rx(wir_r, pay));

      if ( u2_none == cyr ) {
        return cor;
      }
      else {
        u2_rz(wir_r, cor);
        return cyr;
      }
    }
    else return cor;
  }
}

/* _ds_leap(): formula from name and chip.
*/
static u2_weak                                                    //  senior
_ds_leap(u2_wire     wir_r,
         u2_noun     xip,                                         //  retain
         const c3_c* tam_c)                                       //  retain
{
  u2_noun dac = u2_h(xip);
  u2_noun pic = u2_t(dac);

  while ( u2_nul != pic ) {
    u2_noun i_pic = u2_h(pic);
    u2_noun t_pic = u2_t(pic);

    if ( u2_yes == u2_sing_c(tam_c, u2_h(i_pic)) ) {
      return u2_t(i_pic);
    }
    else pic = t_pic;
  }
  return u2_none;
}

/* u2_ds_look():
**
**   Produce hook formula from core, or u2_none.
*/
u2_weak                                                           //  produce
u2_ds_look(u2_wire     wir_r,
           u2_noun     cor,                                       //  retain
           const c3_c* tam_c)                                     //  retain
{
  u2_noun xip = u2_ds_find(wir_r, cor);

  if ( u2_none == xip ) {
    return u2_none;
  } 
  else {
    c3_l axe_l = _1;

    while ( 1 ) {
      u2_noun fol = _ds_leap(wir_r, xip, tam_c);

      if ( u2_none == fol ) {
        u2_noun pet = u2_t(u2_t(xip));

        if ( _0 == pet ) {
          // printf("no joy - %s\n", tam_c);
          return u2_none;
        }
        else {
          u2_axis pax = u2_h(pet);

          c3_assert(u2_fly_is_cat(pax));
          c3_assert((u2_ax_dep(axe_l) + u2_ax_dep(pax)) <= 30);

          axe_l = u2_ax_peg(axe_l, pax);
          xip = u2_t(pet);
          continue;
        }
      }
      else {
        if ( _1 != axe_l ) {
          return u2_rt(wir_r, u2_nock_flac,
                              u2_rc(wir_r, u2_nock_frag, axe_l),
                              fol);
        }
        else return fol;
      }
    }
  }
}

/* u2_ds_fire():
**
**   Fire formula from core.
*/
u2_weak                                                           //  produce
u2_ds_fire(u2_wire     wir_r,
           u2_noun     cor,                                       //  retain
           const c3_c* tam_c)                                     //  retain
{
  u2_noun fol = u2_ds_look(wir_r, cor, tam_c);

  if ( u2_none == fol ) {
    return u2_none;
  }
  else {
    u2_noun pro = u2_nk_nock(wir_r, u2_rx(wir_r, cor), fol);

    u2_rz(wir_r, fol);
    return pro;
  }
}
