/* f/shed.c
**
** This file is in the public domain.
*/
#include "all.h"

/* _sh_good_sil()::
*/
static u2_flag
_sh_good_sil(u2_noun sil)
{
  // XX: verify seal.
  //
  return u2_yes;
}

/* _sh_good_bud()::
*/
static u2_flag
_sh_good_bud(u2_noun bud)
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
  else return u2_no;
}

/* _sh_good_nut()::
*/
static u2_flag
_sh_good_nut(u2_noun nut)
{
  if ( u2_nul == nut ) {
    return u2_yes;
  } else {
    u2_noun i_nut, t_nut;
    u2_noun pi_nut, qi_nut;

    if ( (u2_no == u2_as_cell(nut, &i_nut, &t_nut)) ||
         (u2_no == u2_as_cell(i_nut, &pi_nut, &qi_nut)) ||
         (u2_no == u2_stud(pi_nut)) )
    {
      return u2_no;
    }
    else return u2_yes;
  }
}

/* u2_sh_mine(): substitute active, annotated battery.
*/
u2_weak                                                           //  transfer
u2_sh_mine(u2_ray  wir_r,
           u2_clue clu,                                           //  retain
           u2_noun cor)                                           //  transfer
{
  u2_ray bas_r;
  u2_noun pay, bat;
  u2_noun sil, bud, nut;
  u2_chip xip;

  if ( 0 == (bas_r = u2_wire_bas_r(wir_r)) ) {
    return cor;
  }
  else if ( (u2_none == u2_as_cell(cor, &pay, &bat)) ||
            (u2_no == u2_dust(bat)) )
  {
    return cor;
  }
  else if ( u2_none != (xip = u2_ch_find(u2_bask_hag_r(bas_r), bat)) ) {
    u2_noun cyr;

    if ( u2_none == (cyr = u2_rc(wir_r, u2_rx(wir_r, pay), u2_h(u2_t(xip)))) ) {
      return cor;
    }
    else {
      u2_rl_lose(wir_r, cor);

      return cyr;
    }
  }
  else if ( (u2_no == u2_as_trel(clu, &bud, &sil, &nut)) ||
            (u2_no == _sh_good_bud(bud)) ||
            (u2_no == _sh_good_sil(sil)) ||
            (u2_no == _sh_good_nut(nut)) )
  {
    return cor;
  }
  else {
    u2_noun dac, bot, pet, xop, cyr;

    dac = bot = pet = xop = cyr = u2_none;
    while ( 1 ) {
      /* disc: dac
      */
      {
        if ( u2_none == (dac = u2_rl_take(bas_r, u2_t(clu))) ) {
          break;
        }
      }

      /* battery: bot
      */
      {
        if ( u2_no == u2_rl_junior(bas_r, bat) ) {
          /* We need the jet battery to be in the shed, so that we
          ** have a fast algorithm for distinguishing jet batteries
          ** by ray address.
          */
          bot = u2_rc(bas_r, u2_h(bat), u2_t(bat));
#if 0
        printf("battery: in basket: %d.%x\n",
                u2_ray_a(u2_dog_a(bot)),
                u2_ray_b(u2_dog_a(bot)));
#endif
        }
        else {
          bot = u2_rl_take(bas_r, bat);
#if 0
          printf("battery: in shed! %d.%x\n",
                  u2_ray_a(u2_dog_a(bot)),
                  u2_ray_b(u2_dog_a(bot)));

#endif
        }
        if ( u2_none == bot ) {
          break;
        }
      }

      /* trunk: pet
      */
      {
        if ( _0 == bud ) {
          pet = u2_nul;
        }
        else {
          u2_atom p_bud = u2_t(bud);
          u2_noun car   = u2_frag(p_bud, cor);

          if ( (u2_none == car) || (u2_no == u2_dust(car)) ) {
            break;
          } else {
            u2_noun but = u2_t(car);
            u2_noun xup, axe;

            if ( u2_none == (xup = u2_ch_find(u2_bask_hag_r(bas_r), but)) ) {
              printf("no base!\n");
              u2_err(wir_r, "clu", clu);
              break;
            }
            else u2_rl_gain(bas_r, xup);

            if ( u2_none == (axe = u2_rl_take(bas_r, p_bud)) ) {
              u2_rl_lose(bas_r, xup);
            }

            if ( u2_none == (pet = u2_rc(bas_r, p_bud, xup)) ) {
              u2_rl_lose(bas_r, axe);
              u2_rl_lose(bas_r, xup);
              break;
            }
          }
        }
      }

      /* xop: new chip.
      */
      {
        if ( u2_none == (xop = u2_rt(bas_r, dac, bot, pet)) ) {
          break;
        }
        if ( u2_none == (u2_ch_save(bas_r, u2_bask_hag_r(bas_r), bot, xop)) ) {
          break;
        }
        u2_rl_lose(bas_r, xop);
      }

      /* cyr: new core.
      */
      {
        u2_noun cyr;

        if ( u2_none == (cyr = u2_rc(wir_r, u2_rx(wir_r, pay), bot)) ) {
          break;
        }
        else {
          u2_rl_lose(wir_r, cor);

          return cyr;
        }
      }
    }
    u2_ho_warn_here();
#if 0
    //  XXX: an unknown bug is triggered here;
    //  but basket needs a minor rewrite.
    //
    if ( dac != u2_none ) u2_rl_lose(bas_r, dac);
    if ( bot != u2_none ) u2_rl_lose(bas_r, bot);
    if ( pet != u2_none ) u2_rl_lose(bas_r, pet);
    if ( xop != u2_none ) u2_rl_lose(bas_r, xop);
#endif
    return cor;
  }
}

/* _sh_good(): verify payload integrity in core.
*/
static u2_flag
_sh_good(u2_noun cor,
         u2_noun xip)
{
#if 1
  return u2_yes;    //  Good!  Ja!  Everything good!
#else
  while ( 1 ) {
    u2_noun bat = u2_h(u2_t(xip));
    u2_noun pet = u2_t(u2_t(xip));

    if ( u2_no == u2_sing(bat, u2_t(cor)) ) {
      {
        char *cos_c = u2_ho_cstring(xip);

        printf("sh: cos_c: %s\n", cos_c);
      }
      c3_assert(0);
      u2_ho_warn_here();
      return u2_no;
    }
    else {
      if ( _0 == pet ) {
        return u2_yes;
      } else {
        u2_atom axe = u2_h(pet);
        u2_noun nub = u2_frag(axe, cor);

        if ( u2_none == nub ) {
          return u2_no;
        } else {
          cor = nub;
          xip = u2_t(pet);
          continue;
        }
      }
    }
  }
#endif
}

/* u2_sh_find(): find chip by core.
*/
u2_weak
u2_sh_find(u2_ray  wir_r,
           u2_noun cor)
{
  u2_ray bas_r;

  if ( 0 == (bas_r = u2_wire_bas_r(wir_r)) ) {
    return u2_none;
  }
  if ( u2_no == u2_dust(cor) ) {
    return u2_none;
  } else {
    u2_noun bat = u2_t(cor);

    if ( !u2_fly_is_dog(bat) ) {
      return u2_none;
    } else {
      u2_ray bat_r = u2_dog_a(bat);
      u2_ray rut_r = u2_rail_rut_r(bas_r);
      u2_ray hat_r = u2_rail_hat_r(bas_r);

      if ( (bat_r < rut_r) || (bat_r >= hat_r) ) {
        return u2_none;
      } else {
        u2_chip xip = u2_ch_find(u2_bask_hag_r(bas_r), bat);

        if ( u2_none == xip ) {
#if 0
          printf("bat_r %d.%x; hat_r %d.%x; rut_r %d.%x\n",
                  u2_ray_a(bat_r), u2_ray_b(bat_r),
                  u2_ray_a(hat_r), u2_ray_b(hat_r),
                  u2_ray_a(rut_r), u2_ray_b(rut_r));
          u2_ho_warn_here();
#endif
          return u2_none;
        } else {
          if ( u2_yes == _sh_good(cor, xip) ) {
            return xip;
          }
          else {
            u2_ho_warn_here();

            return u2_none;
          }
        }
      }
    }
  }
}

/* u2_sh_cook():
**
**   Produce hook formula from chip, or u2_none.
*/
u2_weak
u2_sh_cook(u2_wire     wir_r,
           u2_noun     xip,
           const c3_c* tam_c)
{
  u2_noun dac = u2_h(xip);
  u2_noun nut = u2_t(dac);

  while ( u2_nul != nut ) {
    u2_noun i_nut = u2_h(nut);
    u2_noun t_nut = u2_t(nut);

    if ( u2_yes == u2_sing_c(tam_c, u2_h(i_nut)) ) {
      return u2_t(i_nut);
    }
    else nut = t_nut;
  }
  return u2_none;
}

/* u2_sh_look():
**
**   Produce hook formula from core, or u2_none.
*/
u2_weak
u2_sh_look(u2_wire     wir_r,
           u2_noun     cor,
           const c3_c* tam_c)
{
  u2_ray  bas_r = u2_wire_bas_r(wir_r);
  u2_noun bat   = u2_t(cor);
  u2_noun fol;

  if ( u2_none != (fol = u2_ch_find_mixt(u2_bask_hag_r(bas_r), tam_c, bat)) ) {
    return fol;
  } else {
    u2_noun xip = u2_sh_find(wir_r, cor);

    if ( u2_none == xip ) {
      return u2_none;
    }
    else {
      u2_axis axe_w = _1;

      while ( 1 ) {
        fol = u2_sh_cook(wir_r, xip, tam_c);

        if ( u2_none == fol ) {
          u2_noun pet = u2_t(u2_t(xip));

          if ( _0 == pet ) {
            return u2_none;
          }
          else {
            u2_axis pax = u2_h(pet);

            c3_assert(u2_fly_is_cat(pax));
            c3_assert((u2_ax_dep(axe_w) + u2_ax_dep(pax)) <= 30);

            axe_w = u2_ax_peg(axe_w, pax);
            xip = u2_t(pet);
            continue;
          }
        }
        else {
          fol = u2_rl_take(bas_r, fol);

          if ( _1 != axe_w ) {
            /* XX: suboptimal; use comb:lily.
            */
            fol = u2_rt(bas_r, u2_nock_flac,
                               u2_rc(bas_r, u2_nock_frag, axe_w),
                               fol);
          }
          fol = u2_ch_save_mixt(bas_r, u2_bask_hag_r(bas_r), tam_c, bat, fol);

          return fol;
        }
      }
    }
  }
}
