/* watt/iris.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* burn:iris:rose:plow
*/
  static u4_type
  _iris_burn_sint(u4_plow, u4_type, u4_rail, u4_axis, u4_tack);

  static u4_type
  _iris_burn_dext(u4_plow p,
                  u4_type sut,
                  u4_rail bar,
                  u4_axis axe,
                  u4_tack tac)
  {
    u4_lane lan = p->lan;
    u4_noun p_tac, q_tac;
    u4_noun p_sut, q_sut, qrs_sut;

    if ( u4_n_zero(tac) ) {
      return sut;
    }
    else if ( u4_b_p(tac, u4_atom_leaf, &p_tac) ) {
      return _iris_snap(p, sut, bar, axe, p_tac);
    }
    else if ( u4_b_pq(tac, u4_atom_bran, &p_tac, &q_tac) ) {
      if ( u4_b_pq(sut, u4_atom_face, &p_sut, &q_sut) ) {
        if ( u4_n_eq(p_sut, p_tac) ) {
          return u4_kt
            (lan, 
             u4_atom_face, 
             p_tac, 
             _iris_burn_dext(p, q_sut, bar, axe, q_tac));
        }
        else {
          return u4_kt
            (lan,
             u4_atom_face,
             p_tac,
             _iris_burn_dext(p, sut, bar, axe, q_tac));
        }
      }
      else if ( u4_b_pq(sut, u4_atom_fork, &p_sut, &q_sut) ||
                u4_b_pq(sut, u4_atom_fuse, &p_sut, &q_sut) ||
                u4_b_pq(sut, u4_atom_hold, &p_sut, &q_sut) )
      {
        return _iris_burn_sint(p, sut, bar, axe, tac);
      }
      else {
        return u4_kt
          (lan,
           u4_atom_face,
           p_tac,
           _iris_burn_dext(p, sut, bar, axe, q_tac));
      }
    }
    else if ( u4_b_pq(tac, u4_atom_pair, &p_tac, &q_tac) ) {
      if ( u4_b_pq(sut, u4_atom_cell, &p_sut, &q_sut) ) {
        u4_noun l_sut, l_bar, l_axe;
        u4_noun r_sut, r_bar, r_axe;

        l_sut = sut; l_bar = bar; l_axe = axe;
        _iris_slip(p, &l_sut, &l_bar, &l_axe, u4_axis_2);

        r_sut = sut; r_bar = bar; r_axe = axe;
        _iris_slip(p, &r_sut, &r_bar, &r_axe, u4_axis_3);

        return u4_kt
          (lan,
           u4_atom_cell,
           _iris_burn_dext(p, l_sut, l_bar, l_axe, p_tac),
           _iris_burn_dext(p, r_sut, r_bar, r_axe, q_tac));
      }
      else if ( u4_b_pq(sut, u4_atom_core, &p_sut, &qrs_sut) ) {
        u4_noun l_sut, l_bar, l_axe;
        u4_type ham;

        l_sut = sut; l_bar = bar; l_axe = axe;
        _iris_slip(p, &l_sut, &l_bar, &l_axe, u4_axis_2);

        ham = _iris_burn_dext(p, l_sut, l_bar, l_axe, p_tac);

        if ( u4_n_zero(q_tac) ) {
          return u4_k_trel
            (lan, u4_atom_core, ham, qrs_sut);
        } else {
          return u4_k_trel
            (lan, u4_atom_cell, ham, u4_atom_blur);
        }
      }
      else if ( u4_b_pq(sut, u4_atom_face, &p_sut, &q_sut) ||
                u4_b_pq(sut, u4_atom_fork, &p_sut, &q_sut) ||
                u4_b_pq(sut, u4_atom_fuse, &p_sut, &q_sut) ||
                u4_b_pq(sut, u4_atom_hold, &p_sut, &q_sut) )
      {
        return _iris_burn_sint(p, sut, bar, axe, tac);
      }
      else {
        u4_noun poq = u4_kt
          (lan,
           u4_atom_cell,
           _iris_half(p, sut, bar, axe, u4_axis_2),
           _iris_half(p, sut, bar, axe, u4_axis_3));

        return _iris_burn_dext(p, poq, bar, axe, tac);
      }
    }
    else return u4_trip;
  }
  static u4_unit
  _iris_burn_swim(u4_plow p,
                  u4_type sut,
                  u4_rail bar,
                  u4_axis axe,
                  u4_tack tac)
  {
    u4_lane lan = p->lan;
    u4_noun p_sut, q_sut;

    if ( u4_b_pq(sut, u4_atom_fork, &p_sut, &q_sut) ) {
      u4_unit yop = _iris_burn_swim(p, p_sut, bar, axe, tac);
      u4_unit paf = _iris_burn_swim(p, q_sut, bar, axe, tac);

      if ( u4_n_zero(yop) ) {
        return paf;
      } else if ( u4_n_zero(paf) ) {
        return yop;
      }
      else {
        return u4_kc
          (lan,
           u4_nul,
           _rose_eith(p, u4_ct(yop), u4_ct(paf)));
      }
    }
    else {
      if ( u4_so(_iris_cull(p, sut, bar, axe)) ) {
        return u4_nul;
      }
      else {
        return u4_kc
          (lan, u4_nul, _iris_burn_dext(p, sut, bar, axe, tac));
      }
    }
  }
  static u4_type
  _iris_burn_sint(u4_plow p,
                  u4_type sut,
                  u4_rail bar,
                  u4_axis axe,
                  u4_tack tac)
  {
    u4_lane lan = p->lan;
    u4_noun p_sut, q_sut;

    if ( u4_b_pq(sut, u4_atom_face, &p_sut, &q_sut) ) {
      return u4_kt
        (lan, 
         u4_atom_face,
         p_sut,
         _iris_burn_dext(p, q_sut, bar, axe, tac));
    }
    else if ( u4_b_pq(sut, u4_atom_fork, &p_sut, &q_sut) ) {
      return u4_ct(_iris_burn_swim(p, sut, bar, axe, tac));
    }
    else if ( u4_b_pq(sut, u4_atom_fuse, &p_sut, &q_sut) ) {
      return _rose_both
        (p, 
         _iris_burn_dext(p, q_sut, u4_kc(lan, p_sut, bar), axe, tac),
         _iris_burn_dext(p, p_sut, bar, axe, tac));
    }
    else if ( u4_b_pq(sut, u4_atom_hold, &p_sut, &q_sut) ) {
      return _iris_burn_dext
        (p, _rose_repo(p, p_sut, q_sut), bar, axe, tac); 
    }
    else return u4_trip;
  }
u4_type
_iris_burn(u4_plow p,
           u4_type sut,
           u4_rail bar,
           u4_axis axe,
           u4_tack tac)
{
  return _iris_burn_dext(p, sut, bar, axe, tac);
}

/* cull:iris:rose:plow
*/
  static u4_flag
  _iris_cull_a(u4_plow p,
               u4_type sut,
               u4_rail bar,
               u4_axis axe)
  {
    if ( u4_n_zero(bar) ) {
      return u4_no;
    }
    else if ( u4_so(_rose_orth(p, sut, u4_ch(bar))) ) {
      return u4_yes;
    }
    else return _iris_cull_a(p, sut, u4_ct(bar), axe);
  }
u4_flag
_iris_cull(u4_plow p,
           u4_type sut,
           u4_rail bar,
           u4_axis axe)
{
  if ( u4_so(_rose_null(p, sut)) ) {
    return u4_yes;
  }
  else {
    return _iris_cull_a(p, sut, bar, axe);
  }
}

/* find:iris:rose:plow
*/
  static u4_unit _iris_find_main
    (u4_plow, u4_type, u4_rail, u4_axis, u4_pool, u4_term);

  static u4_unit
  _iris_find_half(u4_plow p,
                  u4_type sut,
                  u4_rail bar,
                  u4_axis axe,
                  u4_pool gil,
                  u4_term cog,
                  u4_axis cap)
  {
    _iris_slip(p, &sut, &bar, &axe, cap);

    return _iris_find_main(p, sut, bar, axe, gil, cog);
  }
  static u4_unit
  _iris_find_swim(u4_plow p,
                  u4_type sut,
                  u4_rail bar,
                  u4_axis axe,
                  u4_pool gil,
                  u4_term cog)
  {
    u4_lane lan = p->lan;
    u4_noun p_sut, q_sut;

    if ( u4_b_pq(sut, u4_atom_fork, &p_sut, &q_sut) ) {
      u4_unit yop = _iris_find_swim(p, p_sut, bar, axe, gil, cog);
      u4_unit paf = _iris_find_swim(p, q_sut, bar, axe, gil, cog);

      if ( u4_n_zero(yop) ) {
        return paf;
      }
      else if ( u4_n_zero(paf) ) {
        return yop;
      }
      else {
        u4_unit lep = u4_ct(yop);
        u4_unit gam = u4_ct(paf);
        
        if ( u4_n_zero(lep) ) {
          if ( u4_n_zero(gam) ) {
            return u4_nul;
          }
          else {
            u4_burp(lan, "cog", u4_prep_textual(lan, cog));
            return _plow_fail(p, "fork conflict a");
          }
        }
        else if ( u4_n_zero(gam) ) {
          return _plow_fail(p, "fork conflict b");
        }
        else {
          u4_plan u_lep = u4_ct(lep);
          u4_plan u_gam = u4_ct(gam);

          if ( u4_n_eq(u4_ch(u_lep), u4_ch(u_gam)) &&
               u4_n_eq(u4_ch(u4_ct(u_lep)), u4_ch(u4_ct(u_gam))) )
          {
            return u4_kt
              (lan,
               u4_nul,
               u4_nul,
               u4_kt
                (lan, 
                 u4_ch(u_lep),
                 u4_ch(u4_ct(u_lep)),
                 _rose_eith(p, u4_ct(u4_ct(u_lep)), u4_ct(u4_ct(u_gam)))));
          }
          else {
            u4_burp(lan, "cog", u4_prep_textual(lan, cog));
            return _plow_fail(p, "fork conflict c");
          }
        }
      }
    }
    else {
      if ( u4_so(_iris_cull(p, sut, bar, axe)) ) {
        return u4_nul;
      }
      else if ( u4_b_pq(sut, u4_atom_hold, &p_sut, &q_sut) &&
                u4_bag_in(u4_kc(lan, bar, sut), gil) )
      {
        return u4_nul;
      }
      else {
        return u4_kc
          (lan, u4_nul, _iris_find_main(p, sut, bar, axe, gil, cog));
      }
    }
  }
  static u4_unit
  _iris_find_main_a(u4_plow p,
                    u4_type sut,
                    u4_rail bar,
                    u4_axis axe,
                    u4_pool gil,
                    u4_term cog)
  {
    u4_lane lan = p->lan;
    u4_noun p_sut, q_sut, r_sut, s_sut;

    if ( u4_n_eq(u4_atom_atom, sut) ||
         u4_n_eq(u4_atom_blur, sut) ||
         u4_n_eq(u4_atom_blot, sut) ||
         u4_b_p(sut, u4_atom_cube, &p_sut) )
    {
      return u4_nul;
    }
    else if ( u4_b_pq(sut, u4_atom_cell, &p_sut, &q_sut) ) {
      u4_unit taf = _iris_find_half(p, sut, bar, axe, gil, cog, u4_axis_2);
      u4_unit bov = _iris_find_half(p, sut, bar, axe, gil, cog, u4_axis_3);

      if ( u4_n_zero(taf) ) {
        return bov;
      }
      else if ( u4_n_zero(bov) ) {
        return taf;
      }
      else {
#if 0
        u4_burp(lan, "cog", u4_prep_textual(lan, cog));
        return _plow_fail(p, "cell conflict");
#else
        return taf;
#endif
      }
    }
    else if ( u4_b_pqrs(sut, u4_atom_core, &p_sut, &q_sut, &r_sut, &s_sut) ) {
      u4_spec dab = s_sut;
      u4_unit zem;

      zem = _gull_look(p, dab, cog);
      if ( u4_n_zero(zem) ) {
        return _iris_find_half(p, sut, bar, axe, gil, cog, u4_axis_2);
      }
      else return 
        u4_kc
          (lan, 
           u4_nul, 
           u4_kt
            (lan, 
             axe, 
             u4_kq
              (lan, 
               u4_nul, 
               u4_op_peg(lan, u4_axis_3, u4_ch(u4_ct(zem))),
               sut,
               u4_ct(u4_ct(zem))),
             sut));
    }
    else if ( u4_b_pq(sut, u4_atom_face, &p_sut, &q_sut) ) {
      if ( u4_n_eq(cog, p_sut) ) {
        return u4_kq
          (lan, u4_nul, axe, u4_nul, q_sut);
      }
      else return u4_nul;
    }
    else if ( u4_b_pq(sut, u4_atom_fork, &p_sut, &q_sut) ) {
      u4_unit foz = _iris_find_swim(p, sut, bar, axe, gil, cog);

      if ( u4_n_zero(foz) ) {
        return u4_nul; 
      }
      return u4_ct(foz);
    }
    else if ( u4_b_pq(sut, u4_atom_fuse, &p_sut, &q_sut) ) {
      u4_unit hax = _iris_find_main
        (p, p_sut, bar, axe, gil, cog);
      u4_unit yor = _iris_find_main
        (p, q_sut, u4_kc(lan, p_sut, bar), axe, gil, cog);

      if ( u4_n_zero(yor) ) {
        if ( u4_n_zero(hax) ) {
          return u4_nul;
        }
        else {
          u4_noun u_hax = u4_ct(hax);
          u4_noun pu_hax = u4_ch(u_hax);
          u4_noun qu_hax = u4_ch(u4_ct(u_hax));
          u4_noun ru_hax = u4_ct(u4_ct(u_hax));

          return u4_kc
            (lan, 
             u4_nul,
             u4_kt
              (lan,
               pu_hax,
               qu_hax,
               _rose_both
                (p,
                 _iris_peek
                  (p, q_sut, u4_kc(lan, p_sut, bar), axe, pu_hax),
                 ru_hax)));
        }
      }
      else {
        u4_noun u_yor = u4_ct(yor);
        u4_noun pu_yor = u4_ch(u_yor);
        u4_noun qu_yor = u4_ch(u4_ct(u_yor));
        u4_noun ru_yor = u4_ct(u4_ct(u_yor));

        if ( u4_n_zero(hax) ) {
          return u4_kc
            (lan, 
             u4_nul,
             u4_kt
              (lan,
               pu_yor,
               qu_yor,
               _rose_both
                (p,
                 ru_yor,
                 _iris_peek(p, p_sut, bar, axe, pu_yor))));
        }
        else {
          u4_noun u_hax = u4_ct(hax);
          u4_noun pu_hax = u4_ch(u_hax);
          u4_noun qu_hax = u4_ch(u4_ct(u_hax));
          u4_noun ru_hax = u4_ct(u4_ct(u_hax));

          if ( !(u4_n_eq(pu_hax, pu_yor) && u4_n_eq(qu_hax, qu_yor)) ) {
            ru_hax = _iris_peek(p, p_sut, bar, axe, pu_yor);
          }
          return u4_kc
            (lan, 
             u4_nul,
             u4_kt
              (lan,
               pu_yor,
               qu_yor,
               _rose_both(p, ru_yor, ru_hax)));
        }
      }
    }
    else if ( u4_b_pq(sut, u4_atom_hold, &p_sut, &q_sut) ) {
      u4_noun dit = u4_kc(lan, bar, sut);

      if ( u4_bag_in(dit, gil) ) {
        return u4_nul;
      } 
      else {
        return _iris_find_main
          (p,
           _rose_repo(p, p_sut, q_sut), 
           bar,
           axe,
           u4_bag_add(lan, dit, gil), 
           cog);
      }
    }
    else {
      return u4_trip;
    }
  }
  static u4_unit
  _iris_find_main(u4_plow p,
                  u4_type sut,
                  u4_rail bar,
                  u4_axis axe,
                  u4_pool gil,
                  u4_term cog)
  {
    u4_lane lan = p->lan;
    u4_noun mum  = u4_k_quil(lan, sut, bar, axe, gil, cog);
    u4_nopt zod  = u4_tab_get(mum, p->fin);

    if ( zod != u4_bull ) {
      return zod;
    }
    else {
      u4_unit gur = _iris_find_main_a
        (p, sut, bar, axe, gil, cog);

      p->fin = u4_tab_add(lan, mum, gur, p->fin);
      return gur;
    }
  }
u4_plan
_iris_find(u4_plow p,
           u4_type sut,
           u4_rail bar,
           u4_axis axe,
           u4_term cog)
{
  u4_unit fyg = _iris_find_main(p, sut, bar, axe, u4_nul, cog);

  if ( u4_n_zero(fyg) ) {
    u4_burp(p->lan, "name", u4_prep_textual(p->lan, cog));
    u4_burp(p->lan, "sut", _dump_type(p, sut));
    return _plow_fail(p, "not found");
  }
  return u4_ct(fyg);
}

/* fish:iris:rose:plow
*/
  static u4_tool
  _iris_fish_main(u4_plow, u4_type, u4_rail, u4_axis, u4_pool);

  static u4_tool
  _iris_fish_slip(u4_plow p,
                  u4_type sut,
                  u4_rail bar,
                  u4_axis axe,
                  u4_pool vit,
                  u4_axis had)
  {
    _iris_slip(p, &sut, &bar, &axe, had);
    return _iris_fish_main(p, sut, bar, axe, vit);
  }
  static u4_unit
  _iris_fish_swim(u4_plow p,
                  u4_type sut,
                  u4_rail bar,
                  u4_axis axe,
                  u4_pool vit)
  {
    u4_lane lan = p->lan;
    u4_noun p_sut, q_sut;

    if ( u4_b_pq(sut, u4_atom_fork, &p_sut, &q_sut) ) {
      u4_unit doz = _iris_fish_swim(p, p_sut, bar, axe, vit);
      u4_unit ryg = _iris_fish_swim(p, q_sut, bar, axe, vit);
    
      if ( u4_n_zero(doz) ) return ryg;
      else if ( u4_n_zero(ryg) ) return doz;
      else return u4_kc
        (lan, u4_nul, 
              _lily_flor(p, u4_ct(doz), u4_ct(ryg)));
    }
    else {
      if ( u4_so(_iris_cull(p, sut, bar, axe)) ) {
        return u4_nul;
      }
      else {
        return u4_kc
          (lan, u4_nul, _iris_fish_main(p, sut, bar, axe, vit));
      }
    }
  }
  static u4_tool
  _iris_fish_main(u4_plow p,
                  u4_type sut,
                  u4_rail bar,
                  u4_axis axe,
                  u4_pool vit)
  {
    u4_lane lan = p->lan;
    u4_noun p_sut, q_sut;

    if ( u4_n_eq(u4_atom_atom, sut) ) {
      return _lily_flop
        (p, u4_kt(lan, u4_nock_dust, u4_nock_frag, axe));
    }
    else if ( u4_n_eq(u4_atom_blur, sut) ) {
      return u4_kc(lan, u4_nock_bone, u4_yes);
    }
    else if ( u4_n_eq(u4_atom_blot, sut) ) {
      return u4_kc(lan, u4_nock_bone, u4_no);
    }
    else if ( u4_b_pq(sut, u4_atom_core, &p_sut, &q_sut) ) {
      return u4_kc(lan, u4_nock_frag, u4_noun_0);
    }
    else if ( u4_b_p(sut, u4_atom_cube, &p_sut) ) {
      return u4_kt
        (lan, u4_nock_sing, 
              u4_kc(lan, u4_nock_bone, p_sut),
              u4_kc(lan, u4_nock_frag, axe));
    }
    else if ( u4_b_pq(sut, u4_atom_cell, &p_sut, &q_sut) ) {
      return _lily_flan
        (p,
         u4_kt(lan, u4_nock_dust, u4_nock_frag, axe),
         _lily_flan
          (p, _iris_fish_slip(p, sut, bar, axe, vit, u4_axis_2),
              _iris_fish_slip(p, sut, bar, axe, vit, u4_axis_3)));
    }
    else if ( u4_b_pq(sut, u4_atom_face, &p_sut, &q_sut) ) {
      return _iris_fish_main(p, q_sut, bar, axe, vit);
    }
    else if ( u4_b_pq(sut, u4_atom_fork, &p_sut, &q_sut) ) {
      return u4_ct(_iris_fish_swim(p, sut, bar, axe, vit));
    }
    else if ( u4_b_pq(sut, u4_atom_fuse, &p_sut, &q_sut) ) {
      return _lily_flan
        (p, 
         _iris_fish_main(p, p_sut, bar, axe, vit),
         _iris_fish_main(p, q_sut, u4_kc(lan, p_sut, bar), axe, vit));
    }
    else if ( u4_b_pq(sut, u4_atom_hold, &p_sut, &q_sut) ) {
      u4_noun duq = u4_kc(lan, sut, bar);

      if ( u4_bag_in(duq, vit) ) {
        return _plow_fail(p, "fish recursion");
      } 
      else {
        return _iris_fish_main
          (p, _rose_repo(p, p_sut, q_sut),
              bar,
              axe,
              u4_bag_add(lan, duq, vit));
      }
    }
    else {
      return u4_trip;
    }
  }
u4_tool
_iris_fish(u4_plow p,
           u4_type sut,
           u4_rail bar,
           u4_axis axe)
{
  return _iris_fish_main(p, sut, bar, axe, u4_nul);
}

/* half:iris:rose:plow
*/
  static u4_unit
  _iris_half_swim(u4_plow p,
                  u4_type sut,
                  u4_rail bar,
                  u4_axis axe,
                  u4_axis had)
  {
    u4_lane lan = p->lan;
    u4_noun p_sut, q_sut;

    if ( u4_b_pq(sut, u4_atom_fork, &p_sut, &q_sut) ) {
      u4_unit doz = _iris_half_swim(p, p_sut, bar, axe, had);
      u4_unit ryg = _iris_half_swim(p, q_sut, bar, axe, had);

      if ( u4_n_zero(doz) ) return ryg;
      else if ( u4_n_zero(ryg) ) return doz;
      else return u4_kc
        (lan, u4_nul, _rose_eith(p, u4_ct(doz), u4_ct(ryg)));
    }
    else {
      if ( u4_so(_iris_cull(p, sut, bar, axe)) ) {
        return u4_nul;
      }
      else {
        return u4_kc
          (lan, u4_nul, _iris_half(p, sut, bar, axe, had));
      }
    }
  }
  u4_type
  _iris_half_main(u4_plow p,
                  u4_type sut,
                  u4_rail bar,
                  u4_axis axe,
                  u4_axis had)
  {
    u4_lane lan = p->lan;
    u4_noun p_sut, q_sut;

    if ( u4_n_eq(u4_atom_atom, sut) ) {
      return u4_atom_blot;
    }
    else if ( u4_n_eq(u4_atom_blot, sut) ) {
      return u4_atom_blot;
    }
    else if ( u4_n_eq(u4_atom_blur, sut) ) {
      return u4_atom_blur;
    }
    else if ( u4_b_pq(sut, u4_atom_cell, &p_sut, &q_sut) ) {
      if ( u4_n_eq(u4_axis_2, had) ) {
        return p_sut;
      } else return q_sut;
    }
    else if ( u4_b_pq(sut, u4_atom_core, &p_sut, &q_sut) ) {
      if ( u4_n_eq(u4_axis_2, had) ) {
        return p_sut;
      } else return u4_atom_blur;
    }
    else if ( u4_b_p(sut, u4_atom_cube, &p_sut) )  {
      if ( u4_n_atom(p_sut) ) {
        return u4_atom_blot;
      }
      else if ( u4_n_eq(u4_axis_2, had) ) {
        return u4_kc(lan, u4_atom_cube, u4_ch(p_sut));
      } else return u4_kc(lan, u4_atom_cube, u4_ct(p_sut));
    }
    else if ( u4_b_pq(sut, u4_atom_face, &p_sut, &q_sut) ) {
      return _iris_half(p, q_sut, bar, axe, had);
    }
    else if ( u4_b_pq(sut, u4_atom_fork, &p_sut, &q_sut) ) {
      u4_unit pez = _iris_half_swim(p, sut, bar, axe, had);

      if ( u4_n_zero(pez) ) {
        return u4_atom_blot;
      }
      return u4_ct(pez);
    }
    else if ( u4_b_pq(sut, u4_atom_fuse, &p_sut, &q_sut) ) {
      return _rose_both
        (p, 
         _iris_half(p, q_sut, u4_kc(lan, p_sut, bar), axe, had),
         _iris_half(p, p_sut, bar, axe, had));
    }
    else if ( u4_b_pq(sut, u4_atom_hold, &p_sut, &q_sut) ) {
      return _iris_half(p, _rose_repo(p, p_sut, q_sut), bar, axe, had);
    }
    else return u4_trip;
  }

u4_type
_iris_half(u4_plow p,
           u4_type sut,
           u4_rail bar,
           u4_axis axe,
           u4_axis had)
{
  u4_lane lan = p->lan;
  u4_noun mum = u4_kt(lan, sut, bar, had);  /* axe is not used */
  u4_nopt zod = u4_tab_get(mum, p->huf);

  if ( zod != u4_bull ) {
    return zod;
  }
  else {
    u4_type gur = _iris_half_main(p, sut, bar, axe, had);

    p->huf = u4_tab_add(lan, mum, gur, p->huf);
    return gur;
  }
}

/* nest:iris:rose:plow
*/
  static u4_flag
  _iris_nest_sint
    (u4_plow, u4_type, u4_rail, u4_axis, u4_pool, u4_type, u4_rail, u4_axis); 
  static u4_flag
  _iris_nest_dext
    (u4_plow, u4_type, u4_rail, u4_axis, u4_pool, u4_type, u4_rail, u4_axis); 

  static u4_flag
  _iris_nest_dext_slip(u4_plow p,
                       u4_type sut,
                       u4_rail bar,
                       u4_axis axe,
                       u4_pool gil,
                       u4_type bon,
                       u4_rail nef,
                       u4_axis ful,
                       u4_axis had)
  {
    _iris_slip(p, &sut, &bar, &axe, had);
    _iris_slip(p, &bon, &nef, &ful, had);

    return _iris_nest_dext(p, sut, bar, axe, gil, bon, nef, ful);
  }
  static u4_flag
  _iris_nest_dext_swim(u4_plow p,
                       u4_type sut,
                       u4_rail bar,
                       u4_axis axe,
                       u4_pool gil,
                       u4_type bon,
                       u4_rail nef,
                       u4_axis ful)
  {
    u4_noun p_sut, q_sut;

    if ( u4_b_pq(sut, u4_atom_fork, &p_sut, &q_sut) ) {
      return u4_or
        (_iris_nest_dext_swim(p, p_sut, bar, axe, gil, bon, nef, ful),
         _iris_nest_dext_swim(p, q_sut, bar, axe, gil, bon, nef, ful));
    }
    else {
      if ( u4_so(_iris_cull(p, sut, bar, axe)) ) {
        return u4_no;
      }
      else return _iris_nest_dext
        (p, sut, bar, axe, gil, bon, nef, ful);
    }
  }

  static u4_flag
  _iris_nest_dext_main(u4_plow p,
                       u4_type sut,
                       u4_rail bar,
                       u4_axis axe,
                       u4_pool gil,
                       u4_type bon,
                       u4_rail nef,
                       u4_axis ful)
  {
    u4_lane lan = p->lan;
    u4_noun p_sut, q_sut, qrs_sut;
    u4_noun p_bon, q_bon, qrs_bon;

    if ( u4_n_eq(sut, u4_atom_atom) ) {
      if ( u4_n_eq(bon, u4_atom_atom) ) {
        return u4_yes;
      }
      else if ( u4_b_p(bon, u4_atom_cube, &p_bon) ) {
        return u4_say(u4_n_atom(p_bon));
      }
      else return _iris_nest_sint(p, sut, bar, axe, gil, bon, nef, ful);
    }
    else if ( u4_n_eq(sut, u4_atom_blur) ) {
      return u4_yes;
    }
    else if ( u4_b_pq(sut, u4_atom_core, &p_sut, &qrs_sut) ) {
      if ( u4_b_pq(bon, u4_atom_core, &p_bon, &qrs_bon) ) {
        return u4_and
          (u4_say(u4_n_eq(qrs_sut, qrs_bon)),
           _iris_nest_dext_slip
              (p, sut, bar, axe, gil, bon, nef, ful, u4_axis_2));
      }
      else return _iris_nest_sint(p, sut, bar, axe, gil, bon, nef, ful);
    }
    else if ( u4_b_pq(sut, u4_atom_cell, &p_sut, &q_sut) ) {
      if ( u4_b_pq(bon, u4_atom_cell, &p_bon, &q_bon) ) {
        return u4_and
          (_iris_nest_dext_slip
              (p, sut, bar, axe, gil, bon, nef, ful, u4_axis_2),
           _iris_nest_dext_slip
              (p, sut, bar, axe, gil, bon, nef, ful, u4_axis_3));
      }
      else {
        return _iris_nest_sint(p, sut, bar, axe, gil, bon, nef, ful);
      }
    }
    else if ( u4_b_p(sut, u4_atom_cube, &p_sut) ) {
      if ( u4_b_p(bon, u4_atom_cube, &p_bon) ) {
        return u4_n_eq(p_sut, p_bon) ? u4_yes : u4_no;
      }
      else return _iris_nest_sint(p, sut, bar, axe, gil, bon, nef, ful);
    }
    else if ( u4_b_pq(sut, u4_atom_face, &p_sut, &q_sut) ) {
      return _iris_nest_dext(p, q_sut, bar, axe, gil, bon, nef, ful);
    }
    else if ( u4_b_pq(sut, u4_atom_fork, &p_sut, &q_sut) ) {
      if ( u4_n_atom(bon) ||
           u4_b_pq(bon, u4_atom_cell, &p_bon, &q_bon) ||
           u4_b_pq(bon, u4_atom_core, &p_bon, &q_bon) ||
           u4_b_p(bon, u4_atom_cube, &p_bon) ) 
      {
        return _iris_nest_dext_swim
          (p, sut, bar, axe, gil, bon, nef, ful);
      }
      else {
        return _iris_nest_sint(p, sut, bar, axe, gil, bon, nef, ful);
      }
    }
    else if ( u4_b_pq(sut, u4_atom_fuse, &p_sut, &q_sut) ) {
      u4_rail buz = u4_kc(lan, p_sut, bar);

      return u4_and
        (_iris_nest_dext(p, p_sut, bar, axe, gil, bon, nef, ful),
         _iris_nest_dext(p, q_sut, buz, axe, gil, bon, nef, ful));
    }
    else if ( u4_b_pq(sut, u4_atom_hold, &p_sut, &q_sut) ) {
      u4_noun duq = u4_kq(lan, sut, bar, bon, nef);

      if ( u4_bag_in(duq, gil) ) {
        return u4_yes;
      } else {
        return _iris_nest_dext
          (p, _rose_repo(p, p_sut, q_sut),
              bar,
              axe,
              u4_bag_add(lan, duq, gil),
              bon, 
              nef, 
              ful);
      }
    }
    else {
      return u4_trip;
    }
  }
  static u4_flag
  _iris_nest_dext(u4_plow p,
                  u4_type sut,
                  u4_rail bar,
                  u4_axis axe,
                  u4_pool gil,
                  u4_type bon,
                  u4_rail nef,
                  u4_axis ful)
  {
    u4_lane lan = p->lan;
  
    if ( u4_n_eq(sut, bon) ) {
      return u4_yes;
    }
    else {
      u4_noun mum = u4_kq(lan, bar, sut, nef, bon);
      u4_nopt zod = u4_tab_get(mum, p->vus);

      if ( zod != u4_bull ) {
        return zod;
      }
      else {
        u4_flag gur = _iris_nest_dext_main
          (p, sut, bar, axe, gil, bon, nef, ful);

        p->vus = u4_tab_add(lan, mum, gur, p->vus);
        return gur;
      }
    }
  }
  static u4_flag
  _iris_nest_sint_swim(u4_plow p,
                       u4_type sut,
                       u4_rail bar,
                       u4_axis axe,
                       u4_pool gil,
                       u4_type bon,
                       u4_rail nef,
                       u4_axis ful)
  {
    u4_noun p_bon, q_bon;

    if ( u4_b_pq(bon, u4_atom_fork, &p_bon, &q_bon) ) {
      return u4_and
        (_iris_nest_sint_swim(p, sut, bar, axe, gil, p_bon, nef, ful),
         _iris_nest_sint_swim(p, sut, bar, axe, gil, q_bon, nef, ful));
    }
    else {
      if ( u4_so(_iris_cull(p, bon, nef, ful)) ) {
        return u4_yes;
      }
      else return _iris_nest_dext
        (p, sut, bar, axe, gil, bon, nef, ful);
    }
  }
  static u4_flag
  _iris_nest_sint(u4_plow p,
                  u4_type sut,
                  u4_rail bar,
                  u4_axis axe,
                  u4_pool gil,
                  u4_type bon,
                  u4_rail nef,
                  u4_axis ful)
  {
    u4_lane lan = p->lan;
    u4_noun p_bon, q_bon;

    if ( u4_n_eq(bon, u4_atom_atom) || u4_n_eq(bon, u4_atom_blur) ) {
      return u4_no;
    }
    else if ( u4_b_pq(bon, u4_atom_cell, &p_bon, &q_bon) ) {
      return u4_no;
    }
    else if ( u4_b_pq(bon, u4_atom_core, &p_bon, &q_bon) ) {
      bon = u4_kt(lan, u4_atom_cell, p_bon, u4_atom_blur);

      return _iris_nest_dext(p, sut, bar, axe, gil, bon, nef, ful);
    }
    else if ( u4_b_p(bon, u4_atom_cube, &p_bon) ) {
      if ( u4_n_atom(p_bon) ) {
        return u4_no;
      }
      else {
        bon = u4_kt
          (lan, u4_atom_cell,
                u4_kc(lan, u4_atom_cube, u4_ch(p_bon)),
                u4_kc(lan, u4_atom_cube, u4_ct(p_bon)));

        return _iris_nest_dext(p, sut, bar, axe, gil, bon, nef, ful);
      }
    }
    else if ( u4_b_pq(bon, u4_atom_face, &p_bon, &q_bon) ) {
      return _iris_nest_dext(p, sut, bar, axe, gil, q_bon, nef, ful);
    }
    else if ( u4_b_pq(bon, u4_atom_fork, &p_bon, &q_bon) ) {
      return _iris_nest_sint_swim(p, sut, bar, axe, gil, bon, nef, ful);
    }
    else if ( u4_b_pq(bon, u4_atom_fuse, &p_bon, &q_bon) ) {
      u4_rail gok = u4_kc(lan, p_bon, nef);

      return u4_or
        (_iris_nest_dext(p, sut, bar, axe, gil, p_bon, nef, ful),
         _iris_nest_dext(p, sut, bar, axe, gil, q_bon, gok, ful));
    }
    else if ( u4_b_pq(bon, u4_atom_hold, &p_bon, &q_bon) ) {
      u4_noun duq = u4_kq(lan, sut, bar, bon, nef);

      if ( u4_bag_in(duq, gil) ) {
        return u4_yes;
      } else {
        return _iris_nest_dext
          (p, sut,
              bar,
              axe,
              u4_bag_add(lan, duq, gil),
              _rose_repo(p, p_bon, q_bon),
              nef, 
              ful);
      }
    }
    else return u4_trip;
  }
u4_flag
_iris_nest(u4_plow p,
           u4_type sut,
           u4_rail bar,
           u4_axis axe,
           u4_type bon)
{
  if ( u4_so(_rose_null(p, bon)) ) {
    return u4_yes;
  }
  else if ( u4_so(_rose_null(p, sut)) ) {
    return u4_no;
  }
  else {
    u4_pool gil = u4_nul;
    u4_rail nef = u4_nul;
    u4_axis ful = u4_axis_1;

    return _iris_nest_dext(p, sut, bar, axe, gil, bon, nef, ful);
  }
}

/* peek:iris:rose:plow
*/
u4_type
_iris_peek(u4_plow p,
           u4_type sut,
           u4_rail bar,
           u4_axis axe,
           u4_axis bow)
{
  u4_lane lan = p->lan;

  if ( u4_n_eq(u4_axis_1, bow) ) {
    return sut;
  } 
  else {
    _iris_slip(p, &sut, &bar, &axe, u4_op_tip(bow));

    return _iris_peek(p, sut, bar, axe, u4_op_tap(lan, bow));
  }
}

/* slip:iris:rose:plow
*/
  static u4_rail
  _iris_slip_a(u4_plow p,
               u4_rail bar,
               u4_axis had)
  {
    if ( u4_n_zero(bar) ) {
      return u4_nul;
    }
    else {
      return u4_kc
        (p->lan, _iris_half(p, u4_ch(bar), u4_nul, u4_axis_1, had),
                 _iris_slip_a(p, u4_ct(bar), had));
    } 
  }
void
_iris_slip(u4_plow p,
           u4_type *sut,
           u4_rail *bar,
           u4_axis *axe,
           u4_axis had)
{
  *sut = _iris_half(p, *sut, *bar, *axe, had);
  *bar = _iris_slip_a(p, *bar,  had);
  *axe = u4_op_peg(p->lan, *axe, had);
}

/* snap:iris:rose:plow
*/
  static u4_type
  _iris_snap_dext(u4_plow, u4_type, u4_rail, u4_axis, u4_type);
  static u4_type
  _iris_snap_sint(u4_plow, u4_type, u4_rail, u4_axis, u4_type);

  static u4_type
  _iris_snap_slip(u4_plow p,
                  u4_type sut,
                  u4_rail bar,
                  u4_axis axe,
                  u4_type bon,
                  u4_axis had)
  {
    _iris_slip(p, &sut, &bar, &axe, had);
    return _iris_snap(p, sut, bar, axe, bon);
  }
  static u4_flag
  _iris_snap_cull(u4_plow p,
                  u4_type sut,
                  u4_rail bar,
                  u4_axis axe,
                  u4_type bon)
  {
    return u4_or
      (_iris_cull(p, sut, bar, axe),
       _rose_orth(p, sut, bon));
  }
  static u4_type
  _iris_snap_dext(u4_plow p,
                  u4_type sut,
                  u4_rail bar,
                  u4_axis axe,
                  u4_type bon)
  {
    u4_lane lan = p->lan;
    u4_noun p_sut, q_sut;
    u4_noun p_bon, q_bon;

    if ( u4_n_eq(sut, u4_atom_atom) ||
         u4_n_eq(sut, u4_atom_blot) ||
         u4_n_eq(sut, u4_atom_blur) ||
         u4_b_p(sut, u4_atom_cube, &p_sut) ||
         u4_b_pq(sut, u4_atom_core, &p_sut, &q_sut) )
    {
      return bon;
    }
    else if ( u4_b_pq(sut, u4_atom_cell, &p_sut, &q_sut) ) {
      if ( u4_b_pq(bon, u4_atom_cell, &p_bon, &q_bon) ) {
        return u4_kt
          (lan,
           u4_atom_cell,
           _iris_snap_slip(p, sut, bar, axe, p_bon, u4_axis_2),
           _iris_snap_slip(p, sut, bar, axe, q_bon, u4_noun_3));
      }
      else {
        return _iris_snap_sint(p, sut, bar, axe, bon);
      }
    }
    else if ( u4_b_pq(sut, u4_atom_face, &p_sut, &q_sut) ) {
      if ( u4_b_pq(bon, u4_atom_face, &p_bon, &q_bon) ) {
        return u4_kt
          (lan, u4_atom_face,
                p_sut,
                _iris_snap_dext(p, q_sut, bar, axe, q_bon));
      }
      else {
        return u4_kt
          (lan, u4_atom_face,
                p_sut,
                _iris_snap_dext(p, q_sut, bar, axe, bon));
      }
    }
    else if ( u4_b_pq(sut, u4_atom_fork, &p_sut, &q_sut) ) {
      if ( u4_so(_iris_snap_cull(p, p_sut, bar, axe, bon)) ) {
        if ( u4_so(_iris_snap_cull(p, q_sut, bar, axe, bon)) ) {
          return bon;
        }
        else return _iris_snap_dext(p, q_sut, bar, axe, bon);
      }
      else {
        if ( u4_so(_iris_snap_cull(p, q_sut, bar, axe, bon)) ) {
          return _iris_snap_dext(p, p_sut, bar, axe, bon);
        }
        else return bon;
      }
    }
    else if ( u4_b_pq(sut, u4_atom_fuse, &p_sut, &q_sut) ) {
      return _iris_snap_dext
        (p, q_sut, u4_kc(lan, p_sut, bar), axe, bon);
    }
    else if ( u4_b_pq(sut, u4_atom_hold, &p_sut, &q_sut) ) {
      return _iris_snap_dext
        (p, _rose_repo(p, p_sut, q_sut), bar, axe, bon);
    }
    else return u4_trip;
  }
  static u4_type
  _iris_snap_sint(u4_plow p,
                  u4_type sut,
                  u4_rail bar,
                  u4_axis axe,
                  u4_type bon)
  {
    u4_noun p_bon, q_bon;

    if ( u4_b_pq(bon, u4_atom_fork, &p_bon, &q_bon) ) {
      return _rose_eith
        (p, 
         _iris_snap_dext(p, sut, bar, axe, p_bon), 
         _iris_snap_dext(p, sut, bar, axe, q_bon));
    }
    else if ( u4_b_pq(bon, u4_atom_fuse, &p_bon, &q_bon) ) {
      return _rose_both
        (p,
         _iris_snap_dext(p, sut, bar, axe, q_bon),
         _iris_snap_dext(p, sut, bar, axe, p_bon));
    }
    else return bon;
  }
u4_type
_iris_snap(u4_plow p,
           u4_type sut,
           u4_rail bar,
           u4_axis axe,
           u4_type bon)
{
  u4_type sap;

  sap = _iris_snap_dext(p, sut, bar, axe, bon);

  return sap;
}
