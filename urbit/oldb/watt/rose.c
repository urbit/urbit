/* watt/rose.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* both:rose:plow
*/
u4_type
_rose_both(u4_plow p,
           u4_type sut,
           u4_type hoc)
{
  u4_lane lan = p->lan;

#if 1
  if ( u4_n_eq(u4_atom_blur, sut) ) {
    return hoc;
  }
  else if ( u4_n_eq(u4_atom_blur, hoc) ) {
    return sut;
  }
  else {
    return u4_kt(lan, u4_atom_fuse, hoc, sut);
  }
#else 
  if ( u4_so(_iris_nest(p, hoc, u4_nul, u4_axis_1, sut)) ) {
    /* every sut is a hoc */
    return sut;
  }
  else {
    /* not every sut is a hoc */
    if ( u4_so(_iris_nest(p, sut, u4_nul, u4_axis_1, hoc)) ) {
      /* every hoc is a sut */
      return hoc;
    }
    else {
      /* not every sut is a hoc, not every hoc is a sut */
      return u4_kt(lan, u4_atom_fuse, hoc, sut);
    }
  }
#endif
}

/* eith:rose:plow
*/
u4_type
_rose_eith(u4_plow p,
           u4_type sut,
           u4_type hoc)
{
  u4_lane lan = p->lan;

  if ( u4_n_eq(sut, hoc) ) {
    return sut;
  }
  else if ( u4_n_eq(u4_atom_blot, hoc) ) {
    return sut;
  }
  else if ( u4_n_eq(u4_atom_blot, sut) ) {
    return hoc;
  }
  else return u4_kt(lan, u4_atom_fork, sut, hoc);
}

/* etch:rose:plow
*/
u4_type
_rose_etch(u4_plow p,
           u4_type sut)
{
  u4_noun p_sut, q_sut;

  if ( u4_b_pq(sut, u4_atom_fuse, &p_sut, &q_sut) ) {
    return q_sut;
  }
  else return _plow_fail(p, "bad etch");
}

/* edit:rose:plow
*/
u4_type
_rose_edit(u4_plow p,
           u4_type sut,
           u4_list mut)
{
  u4_tack tac = _lark_feed(p, sut, u4_nul, mut);
  u4_type fuz = _iris_burn(p, sut, u4_nul, u4_axis_1, tac);

  return fuz;
}

/* gain:rose:plow
*/
  static u4_rack
  _rose_gain_hunt(u4_plow, u4_type, u4_gene);

  static u4_rack
  _rose_gain_hunt_a(u4_plow p,
                    u4_type sut,
                    u4_bank vof)
  {
    if ( u4_n_zero(vof) ) {
      return u4_nul;
    }
    else return u4_log_cat(p->lan, _rose_gain_hunt(p, sut, u4_ch(vof)),
                                   _rose_gain_hunt_a(p, sut, u4_ct(vof)));
  }
  static u4_plot
  _rose_gain_hunt(u4_plow p,
                  u4_type sut,
                  u4_gene gen)
  {
    u4_lane lan = p->lan;
    u4_noun p_gen, q_gen;

    if ( u4_b_pq(gen, u4_atom_plin, &p_gen, &q_gen) ) {
      return u4_k_cell
        (lan, 
         u4_k_cell(lan, q_gen, _rose_play(p, sut, p_gen)),
         u4_nul);
    }
    else if ( u4_b_pq(gen, u4_atom_zemp, &p_gen, &q_gen) ) {
      return _rose_gain_hunt(p, sut, q_gen);
    }
    else if ( u4_b_pq(gen, u4_atom_zush, &p_gen, &q_gen) ) {
      return _rose_gain_hunt(p, sut, q_gen);
    }
    else if ( u4_b_p(gen, u4_atom_chan, &p_gen) ) {
      return _rose_gain_hunt_a(p, sut, p_gen);
    }
    else return u4_nul;
  }
  static u4_type
  _rose_gain_gild(u4_plow p,
                  u4_type sut,
                  u4_plot mut)
  {
    u4_tack tac = _lark_feed(p, sut, u4_nul, mut);
    u4_type hoc = _iris_burn(p, u4_atom_blur, u4_nul, u4_axis_1, tac);

    return _rose_both(p, sut, hoc);
  }
u4_type
_rose_gain(u4_plow p,
           u4_type sut,
           u4_gene gen)
{
  u4_plot zet = _rose_gain_hunt(p, sut, gen);

  return _rose_gain_gild(p, sut, zet);
}

/* make:rose:plow
*/
  u4_list
  _rose_make_boil_a(u4_plow p,
                    u4_type sut,
                    u4_list rem)
  {
    u4_lane lan = p->lan;

    if ( u4_n_zero(rem) ) {
      return u4_nul;
    }
    else {
      u4_noun i_rem  = u4_ch(rem);
      u4_noun t_rem  = u4_ct(rem);
      u4_gene pi_rem = u4_ch(i_rem);
      u4_tool qi_rem = u4_ct(i_rem);
      u4_plan giz    = _rose_seek(p, sut, _plow_rake(p, pi_rem));
      u4_axis p_giz  = u4_ch(giz);

      return u4_k_cell(lan, u4_k_cell(lan, p_giz, qi_rem),
                            _rose_make_boil_a(p, sut, t_rem));
    }
  }
  u4_tool
  _rose_make_boil(u4_plow p,
                  u4_type sut,
                  u4_axis axe,
                  u4_list rem)
  {
    return _lily_hike(p, axe, _rose_make_boil_a(p, sut, rem));
  }
  u4_noun
  _rose_make_bake(u4_plow p,
                  u4_type sut,
                  u4_book dab)
  {
    u4_lane lan = p->lan;

    if ( u4_n_zero(dab) ) {
      return u4_noun_0;
    }
    else {
      u4_noun n_dab, l_dab, r_dab, qn_dab;

      u4_c_trel(dab, &n_dab, &l_dab, &r_dab);
      qn_dab = u4_ct(n_dab);
      
      if ( u4_n_zero(l_dab) && u4_n_zero(r_dab) ) {
        return _rose_make(p, sut, qn_dab);
      }
      else if ( u4_n_zero(l_dab) ) {
        return u4_kc
          (lan, _rose_make(p, sut, qn_dab),
                _rose_make_bake(p, sut, r_dab));
      }
      else if ( u4_n_zero(r_dab) ) {
        return u4_kc
          (lan, _rose_make(p, sut, qn_dab),
                _rose_make_bake(p, sut, l_dab));
      }
      else {
        return u4_kt
          (lan, _rose_make(p, sut, qn_dab),
                _rose_make_bake(p, sut, l_dab),
                _rose_make_bake(p, sut, r_dab));
      }
    }
  }
  u4_noun
  _rose_make_a(u4_plow p,
               u4_type sut,
               u4_rack dus)
  {
    u4_lane lan = p->lan;

    if ( u4_n_zero(dus) ) {
      return u4_nul;
    }
    else {
      u4_noun i_dus = u4_ch(dus);
      u4_noun t_dus = u4_ct(dus);
      u4_gene pi_dus = u4_ch(i_dus);
      u4_gene qi_dus = u4_ct(i_dus);
      u4_tool pog    = _rose_make(p, sut, qi_dus);

      return u4_k_cell
        (lan,
         u4_k_cell(lan, pi_dus, pog),
         _rose_make_a(p, sut, t_dus));
    }
  }
  u4_noun
  _rose_make_prop(u4_plow p,
                  u4_type sut,
                  u4_prop pup)
  {
    u4_lane lan = p->lan;
    
    if ( u4_n_zero(pup) ) {
      return pup;
    } else {
      u4_plan lar = _rose_seek(p, sut, u4_ch(pup));

      return u4_kc(lan, u4_ch(lar), u4_ct(pup));
    }
  }

  static u4_tool
  _rose_make_main(u4_plow p,
                  u4_type sut,
                  u4_gene gen)
  {
    u4_lane lan = p->lan;
    u4_noun p_gen, q_gen, r_gen;

    if ( u4_b_p(gen, u4_atom_zike, &p_gen) ) {
      return u4_k_cell(lan, u4_nock_frag, u4_axis_0);
    }
    else if ( u4_b_p(gen, u4_atom_zoot, &p_gen) ) {
      return u4_tank;
    }
    else if ( u4_b_p(gen, u4_atom_vint, &p_gen) ) {
      return u4_k_cell(lan, u4_nock_vint, _rose_make(p, sut, p_gen));
    }
    else if ( u4_b_pq(gen, u4_atom_cast, &p_gen, &q_gen) ) {
      return _rose_make(p, sut, q_gen);
    }
    else if ( u4_b_pq(gen, u4_atom_twix, &p_gen, &q_gen) ) {
      return _lily_cons
        (p, _rose_make(p, sut, p_gen), _rose_make(p, sut, q_gen));
    }
    else if ( u4_b_p(gen, u4_atom_wost, &p_gen) ) {
      return _rose_make(p, sut, p_gen);
    }
    else if ( u4_b_p(gen, u4_atom_zush, &p_gen) ) {
      u4_tool guz;
      u4_atom bug;

      bug = p->bug;
      p->bug = u4_op_inc(lan, p->bug);

      guz = _rose_make(p, sut, p_gen);
      
      p->bug = bug;
      return guz;
    }
    else if ( u4_b_p(gen, u4_atom_dust, &p_gen) ) {
      return u4_k_cell(lan, u4_nock_dust, _rose_make(p, sut, p_gen));
    }
    else if ( u4_b_pq(gen, u4_atom_germ, &p_gen, &q_gen) ) {
      return u4_kt(lan, 
                   u4_atom_germ,
                   _rose_make(p, sut, p_gen),
                   _rose_make(p, sut, q_gen));
    } 
    else if ( u4_b_pq(gen, u4_atom_hint, &p_gen, &q_gen) ) {
      return u4_kt(lan, 
                   u4_nock_hint,
                   _rose_make(p, sut, p_gen),
                   _rose_make(p, sut, q_gen));
    }
    else if ( u4_b_p(gen, u4_atom_grit, &p_gen) ) {
      return _rose_make(p, sut, p_gen);
    }
    else if ( u4_b_pq(gen, u4_atom_tash, &p_gen, &q_gen) ) {
      u4_book sec = _gull_fill(p, u4_nul, q_gen);
      u4_noun heq = _rose_make_prop(p, sut, p_gen);
      u4_noun baf = _rose_make_bake(p, _rose_play(p, sut, gen), sec);

#if 1
      return u4_kt
        (lan, u4_nock_coat, heq, baf);
#else
      return u4_kc
        (lan,
         u4_kc(lan, u4_nock_frag, u4_axis_1), 
         u4_kc(lan, u4_nock_bone, baf));
#endif
    }
    else if ( u4_b_pq(gen, u4_atom_plin, &p_gen, &q_gen) ) {
      u4_plan lar = _rose_seek(p, sut, _plow_rake(p, q_gen));
      u4_axis p_lar = u4_ch(lar);

      if ( !u4_n_zero(u4_ch(u4_ct(lar))) ) {
        return _plow_fail(p, "bad like");
      }
      else {
        return _iris_fish(p, _rose_play(p, sut, p_gen), u4_nul, p_lar);
      }
    }
    else if ( u4_b_pq(gen, u4_atom_flac, &p_gen, &q_gen) ) {
      return _lily_comb
        (p,
         _rose_make(p, sut, p_gen),
         _rose_make(p, _rose_play(p, sut, p_gen), q_gen));
    }
    else if ( u4_b_pq(gen, u4_atom_pank, &p_gen, &q_gen) ) {
      u4_book sec = _gull_fill(p, u4_nul, q_gen);
      u4_noun heq = _rose_make_prop(p, sut, p_gen);
      u4_noun baf = _rose_make_bake(p, _rose_play(p, sut, gen), sec);

#if 1
      return u4_kt
        (lan, u4_nock_coat, heq, baf);
#else
      return u4_kc
        (lan,
         u4_kc(lan, u4_nock_frag, u4_axis_1), 
         u4_kc(lan, u4_nock_bone, baf));
#endif
    }
    else if ( u4_b_p(gen, u4_atom_zalt, &p_gen) ) {
      u4_type yoz = _rose_play(p, sut, p_gen);
 
      return u4_kc(lan, u4_nock_bone, yoz);
    }
    else if ( u4_b_p(gen, u4_atom_zond, &p_gen) ) {
      u4_type yoz = _rose_make(p, sut, p_gen);
 
      return u4_kc(lan, u4_nock_bone, yoz);
    }
    else if ( u4_b_pq(gen, u4_atom_bran, &p_gen, &q_gen) ) {
      return _rose_make(p, sut, q_gen);
    }
    else if ( u4_b_pq(gen, u4_atom_sail, &p_gen, &q_gen) ) {
      return u4_k_trel(lan, u4_nock_sail,
                            _rose_make(p, sut, p_gen),
                            _rose_make(p, sut, q_gen));
    }
    else if ( u4_b_p(gen, u4_atom_bone, &p_gen) ) {
      return u4_k_cell(lan, u4_nock_bone, p_gen);
    }
    else if ( u4_b_pq(gen, u4_atom_zemp, &p_gen, &q_gen) ) {
      u4_noun qip;

      p->meb = u4_kc(lan, u4_kc(lan, u4_nul, p_gen), p->meb);
      qip = _rose_make(p, sut, q_gen);
      p->meb = u4_ct(p->meb);

      return qip;
    }
    else if ( u4_b_pq(gen, u4_atom_stil, &p_gen, &q_gen) ) {
      return u4_kc(lan, u4_nock_frag, u4_axis_1);
    }
    else if ( u4_b_pq(gen, u4_atom_mack, &p_gen, &q_gen) ) {
      u4_plan lar = _rose_seek(p, sut, p_gen);
      u4_noun rem = _rose_make_a(p, sut, q_gen);
      u4_axis p_lar = u4_ch(lar);
      u4_noun q_lar = u4_ch(u4_ct(lar));
      u4_type r_lar = u4_ct(u4_ct(lar));

#if 0
      if ( !u4_n_zero(q_gen) ) {
        u4_err(lan, "make: take", gen);
        u4_err(lan, "make: q", q_lar);
      }
#endif
      if ( u4_n_zero(q_lar) ) {
        return _rose_make_boil(p, r_lar, p_lar, rem);
      }
      else {
        u4_door uq_lar  = u4_ct(q_lar);
        u4_axis puq_lar = u4_ch(uq_lar);
        u4_type quq_lar = u4_ch(u4_ct(uq_lar));
        u4_tool ruz = _rose_make_boil(p, quq_lar, p_lar, rem);
        u4_tool gak;

        gak = u4_k_trel
          (lan,
           u4_nock_sail,
           ruz,
           u4_k_cell(lan, u4_nock_frag, u4_op_peg(lan, p_lar, puq_lar)));

        return gak;
      }
    }
    else if ( u4_b_pqr(gen, u4_atom_trol, &p_gen, &q_gen, &r_gen) ) {
      return u4_k_qual
        (lan,
         u4_nock_trol,
         _rose_make(p, sut, p_gen),
         _rose_make(p, _rose_gain(p, sut, p_gen), q_gen),
         _rose_make(p, sut, r_gen));
    }
    else if ( u4_b_pq(gen, u4_atom_sing, &p_gen, &q_gen) ) {
      return u4_k_trel
        (lan,
         u4_nock_sing,
         _rose_make(p, sut, p_gen),
         _rose_make(p, sut, q_gen));
    }
    else {
      u4_noun fog = _plow_open(p, gen);

      if ( u4_n_eq(fog, gen) ) {
        u4_err(lan, "dup: gen", gen);
        return u4_trip;
      }
      return _rose_make(p, sut, _plow_open(p, gen)); 
    }
  }
u4_tool
_rose_make(u4_plow p,
           u4_type sut,
           u4_gene gen)
{
  u4_lane lan = p->lan;
  u4_noun mum = u4_kc(lan, sut, gen);
  u4_nopt zod = u4_tab_get(mum, p->niq);

  if ( zod != u4_bull ) {
    return zod;
  }
  else {
    u4_tool gur = _rose_make_main(p, sut, gen);

    p->niq = u4_tab_add(lan, mum, gur, p->niq);
    return gur;
  }
}

/* null:rose:plow
*/
  static u4_flag
  _rose_null_a(u4_plow p, u4_type sut, u4_pool hem);

  static u4_flag
  _rose_null_b(u4_plow p,
               u4_type sut,
               u4_pool hem)
  {
    u4_lane lan = p->lan;
    u4_noun p_sut, q_sut;

    if ( u4_n_eq(sut, u4_atom_atom) ) {
      return u4_no;
    }
    else if ( u4_n_eq(sut, u4_atom_blur) ) {
      return u4_no;
    }
    else if ( u4_n_eq(sut, u4_atom_blot) ) {
      return u4_yes;
    }
    else if ( u4_b_pq(sut, u4_atom_cell, &p_sut, &q_sut) ) {
      return u4_or(_rose_null_a(p, p_sut, hem), _rose_null_a(p, q_sut, hem));
    }
    else if ( u4_b_pq(sut, u4_atom_core, &p_sut, &q_sut) ) {
      return _rose_null_a(p, p_sut, hem);
    }
    else if ( u4_b_p(sut, u4_atom_cube, &p_sut) ) {
      return u4_no;
    }
    else if ( u4_b_pq(sut, u4_atom_face, &p_sut, &q_sut) ) {
      return _rose_null_a(p, q_sut, hem);
    }
    else if ( u4_b_pq(sut, u4_atom_fork, &p_sut, &q_sut) ) {
      return u4_and(_rose_null_a(p, p_sut, hem), _rose_null_a(p, q_sut, hem));
    }
    else if ( u4_b_pq(sut, u4_atom_fuse, &p_sut, &q_sut) ) {
      return _rose_orth(p, p_sut, q_sut);
    }
    else if ( u4_b_pq(sut, u4_atom_hold, &p_sut, &q_sut) ) {
      if ( u4_bag_in(sut, hem) ) {
        return u4_yes;
      } else {
        return _rose_null_a
          (p, _rose_repo(p, p_sut, q_sut), u4_bag_add(lan, sut, hem));
      }
    } 
    else return u4_trip;
  }
  static u4_flag
  _rose_null_a(u4_plow p,
               u4_type sut,
               u4_pool hem)
  {
    u4_lane lan = p->lan;
    u4_noun mum = u4_kc(lan, sut, hem);
    u4_nopt zod = u4_tab_get(mum, p->tyc);

    if ( zod != u4_bull ) {
      return zod;
    }
    else {
      u4_flag gur = _rose_null_b(p, sut, hem);

      p->tyc = u4_tab_add(lan, mum, gur, p->tyc);
      return gur;
    }
  }
u4_flag
_rose_null(u4_plow p,
           u4_type sut)
{
  return _rose_null_a(p, sut, u4_nul);
}

/* orth:rose:plow
*/
  static u4_flag
  _rose_orth_a(u4_plow p, u4_type sut, u4_type ref, u4_pool bol);

  static u4_flag
  _rose_orth_b(u4_plow p,
               u4_type sut,
               u4_type ref,
               u4_pool bol)
  {
    u4_lane lan = p->lan;
    u4_noun p_sut, q_sut;
    u4_noun p_ref, q_ref;

    if ( u4_n_eq(sut, u4_atom_atom) ) {
      if ( u4_n_eq(ref, u4_atom_atom) ) {
        return u4_no;
      }
      else if ( u4_b_pq(ref, u4_atom_cell, &p_sut, &q_sut) ) {
        return u4_yes;
      }
      else return _rose_orth_a(p, ref, sut, bol);
    }
    else if ( u4_n_eq(sut, u4_atom_blot) ) {
      return u4_yes;
    }
    else if ( u4_n_eq(sut, u4_atom_blur) ) {
      return u4_no;
    }
    else if ( u4_b_pq(sut, u4_atom_cell, &p_sut, &q_sut) ) {
      if ( u4_b_pq(ref, u4_atom_cell, &p_ref, &q_ref) ) {
        return u4_or(_rose_orth_a(p, p_sut, p_ref, bol),
                     _rose_orth_a(p, q_sut, q_ref, bol));
      }
      else {
        return _rose_orth_a(p, ref, sut, bol);
      }
    }
    else if ( u4_b_pq(sut, u4_atom_core, &p_sut, &q_sut) ) {
      return _rose_orth_a
        (p, u4_k_trel(lan, u4_atom_cell, p_sut, u4_atom_blur), ref, bol);
    }
    else if ( u4_b_p(sut, u4_atom_cube, &p_sut) ) {
      if ( u4_n_eq(u4_atom_atom, ref) ) {
        return u4_n_atom(p_sut) ? u4_no : u4_yes;
      }
      else if ( u4_b_p(ref, u4_atom_cube, &p_ref) ) {
        return u4_say(!u4_n_eq(p_sut, p_ref));
      }
      else if ( u4_b_pq(ref, u4_atom_cell, &p_ref, &q_ref) ) {
        if ( u4_n_atom(p_sut) ) {
          return u4_yes;
        } else {
          u4_type hed = u4_k_cell(lan, u4_atom_cube, u4_ch(p_sut));
          u4_type tal = u4_k_cell(lan, u4_atom_cube, u4_ct(p_sut));

          return u4_or(_rose_orth_a(p, hed, p_ref, bol),
                       _rose_orth_a(p, tal, q_ref, bol));
        }
      }
      else {
        return _rose_orth_a(p, ref, sut, bol);
      }
    }
    else if ( u4_b_pq(sut, u4_atom_face, &p_sut, &q_sut) ) {
      return _rose_orth_a(p, q_sut, ref, bol);
    }
    else if ( u4_b_pq(sut, u4_atom_fork, &p_sut, &q_sut) ) {
      return u4_and(_rose_orth_a(p, p_sut, ref, bol),
                    _rose_orth_a(p, q_sut, ref, bol));
    }
    else if ( u4_b_pq(sut, u4_atom_fuse, &p_sut, &q_sut) ) {
      return u4_or(_rose_orth_a(p, p_sut, ref, bol),
                   _rose_orth_a(p, q_sut, ref, bol));
    }
    else if ( u4_b_pq(sut, u4_atom_hold, &p_sut, &q_sut) ) {
      u4_noun gob = u4_k_cell(lan, sut, ref);

      if ( u4_bag_in(gob, bol) ) {
        return u4_yes;
      } else {
        return _rose_orth_a
          (p, _rose_repo(p, p_sut, q_sut), ref, u4_bag_add(lan, gob, bol));
      }
    } 
    else {
      u4_err(lan, "sut", sut);

      return u4_trip;
    }
  }
  static u4_flag
  _rose_orth_a(u4_plow p,
               u4_type sut,
               u4_type ref,
               u4_pool bol)
  {
    u4_lane lan = p->lan;
    u4_noun mum = u4_kt(lan, sut, ref, bol);
    u4_nopt zod = u4_tab_get(mum, p->gam);

    if ( zod != u4_bull ) {
      return zod;
    }
    else {
      u4_flag gur = _rose_orth_b(p, sut, ref, bol);

      p->gam = u4_tab_add(lan, mum, gur, p->gam);
      return gur;
    }
  }
u4_flag
_rose_orth(u4_plow p,
           u4_type sut,
           u4_type ref)
{
  return _rose_orth_a(p, sut, ref, u4_nul);
}

/* play:rose:plow
*/
  static u4_plot
  _rose_play_a(u4_plow p,
               u4_type sut,
               u4_rack dus)
  {
    u4_lane lan = p->lan;

    if ( u4_n_zero(dus) ) {
      return u4_nul;
    }
    else {
      u4_noun i_dus = u4_ch(dus);
      u4_noun t_dus = u4_ct(dus);
      u4_gene pi_dus = u4_ch(i_dus);
      u4_gene qi_dus = u4_ct(i_dus);
      u4_type feg    = _rose_play(p, sut, qi_dus);

      return u4_k_cell
        (lan,
         u4_k_cell(lan, pi_dus, feg),
         _rose_play_a(p, sut, t_dus));
    }
  }
  static u4_type
  _rose_play_main(u4_plow p,
                  u4_type sut,
                  u4_gene gen)
  {
    u4_lane lan = p->lan;
    u4_noun p_gen, q_gen, r_gen;

    if ( u4_b_p(gen, u4_atom_zike, &p_gen) ) {
      return u4_atom_blot;
    }
    else if ( u4_b_p(gen, u4_atom_zoot, &p_gen) ) {
      return u4_tank;
    }
    else if ( u4_b_p(gen, u4_atom_vint, &p_gen) ) {
      return u4_atom_atom;
    }
    else if ( u4_b_pq(gen, u4_atom_cast, &p_gen, &q_gen) ) {
      return _rose_play(p, sut, p_gen);
    }
    else if ( u4_b_pq(gen, u4_atom_twix, &p_gen, &q_gen) ) {
      return u4_k_trel
        (lan, u4_atom_cell,
              _rose_play(p, sut, p_gen),
              _rose_play(p, sut, q_gen));
    }
    else if ( u4_b_p(gen, u4_atom_wost, &p_gen) ) {
      return _rose_play(p, sut, p_gen);
    }
    else if ( u4_b_p(gen, u4_atom_zush, &p_gen) ) {
      u4_type soq;
      u4_atom bug;

      bug = p->bug;
      p->bug = u4_op_inc(lan, p->bug);

      soq = _rose_play(p, sut, p_gen);
     
      u4_brut(p, "soq", soq);

      p->bug = bug;
      return soq;
    }
    else if ( u4_b_p(gen, u4_atom_dust, &p_gen) ) {
      return u4_k_trel
        (lan,
         u4_atom_fork,
         u4_k_cell(lan, u4_atom_cube, u4_yes),
         u4_k_cell(lan, u4_atom_cube, u4_no));
    }
    else if ( u4_b_pq(gen, u4_atom_germ, &p_gen, &q_gen) ) {
      return _rose_play(p, sut, q_gen);
    } 
    else if ( u4_b_pq(gen, u4_atom_hint, &p_gen, &q_gen) ) {
      return _rose_play(p, sut, q_gen);
    }
    else if ( u4_b_p(gen, u4_atom_grit, &p_gen) ) {
      return _rose_etch(p, _rose_play(p, sut, p_gen));
    }
    else if ( u4_b_pq(gen, u4_atom_tash, &p_gen, &q_gen) ) {
      return u4_k_quil
        (lan, u4_atom_core, 
              sut, 
              u4_atom_soft,
              sut,
              _gull_fill(p, u4_nul, q_gen));
    }
    else if ( u4_b_pq(gen, u4_atom_plin, &p_gen, &q_gen) ) {
      return u4_k_trel
        (lan,
         u4_atom_fork,
         u4_k_cell(lan, u4_atom_cube, u4_yes),
         u4_k_cell(lan, u4_atom_cube, u4_no));
    }
    else if ( u4_b_pq(gen, u4_atom_flac, &p_gen, &q_gen) ) {
      return _rose_play(p, _rose_play(p, sut, p_gen), q_gen);
    }
    else if ( u4_b_pq(gen, u4_atom_pank, &p_gen, &q_gen) ) {
      return u4_k_quil
        (lan, u4_atom_core, 
              sut, 
              u4_atom_hard,
              sut,
              _gull_fill(p, u4_nul, q_gen));
    }
    else if ( u4_b_p(gen, u4_atom_zalt, &p_gen) ) {
      return u4_k_cell(lan, u4_atom_cube, _rose_play(p, sut, p_gen));
    }
    else if ( u4_b_p(gen, u4_atom_zond, &p_gen) ) {
      return u4_k_cell(lan, u4_atom_cube, _rose_make(p, sut, p_gen));
    }
    else if ( u4_b_pq(gen, u4_atom_bran, &p_gen, &q_gen) ) {
      return u4_k_trel(lan, u4_atom_face, p_gen, _rose_play(p, sut, q_gen));
    }
    else if ( u4_b_pq(gen, u4_atom_sail, &p_gen, &q_gen) ) {
      return u4_atom_blur;
    }
    else if ( u4_b_p(gen, u4_atom_bone, &p_gen) ) {
      return u4_k_cell(lan, u4_atom_cube, p_gen); 
    }
    else if ( u4_b_pq(gen, u4_atom_zemp, &p_gen, &q_gen) ) {
      u4_noun qip;

      p->meb = u4_kc(lan, u4_kc(lan, u4_nul, p_gen), p->meb);
      qip = _rose_play(p, sut, q_gen);
      p->meb = u4_ct(p->meb);

      return qip;
    }
    else if ( u4_b_pq(gen, u4_atom_stil, &p_gen, &q_gen) ) {
      return sut;
    }
    else if ( u4_b_pq(gen, u4_atom_mack, &p_gen, &q_gen) ) {
      u4_plan lar = _rose_seek(p, sut, p_gen);
      u4_noun huz = _rose_play_a(p, sut, q_gen);
      u4_noun q_lar = u4_ch(u4_ct(lar));
      u4_type r_lar = u4_ct(u4_ct(lar));

      if ( u4_n_zero(q_lar) ) {
        return _rose_edit(p, r_lar, huz);
      }
      else {
        u4_door uq_lar  = u4_ct(q_lar);
        u4_type quq_lar = u4_ch(u4_ct(uq_lar));
        u4_gene ruq_lar = u4_ct(u4_ct(uq_lar));
        u4_gene old     = gen;
        u4_noun p_sut, q_sut, r_sut, s_sut;

        sut = _rose_edit(p, quq_lar, huz);
        gen = ruq_lar;

        if ( u4_b_pqrs(sut, u4_atom_core, &p_sut, &q_sut, &r_sut, &s_sut) ) {
          if ( u4_n_eq(u4_atom_hard, q_sut) ) {
            sut = u4_k_quil(lan, u4_atom_core, r_sut, q_sut, r_sut, s_sut);
            if ( !u4_n_zero(p->bug) ) {
              u4_err(lan, "hard: gen", old);
              u4_brut(p, "hard: sut", sut);
              u4_brut(p, "hard: mack", u4_kt(lan, u4_atom_hold, sut, gen));
              printf("\n");
            }
          } 
          else if ( u4_n_eq(u4_atom_soft, q_sut) ) {
            sut = sut;
            if ( !u4_n_zero(p->bug) ) {
              u4_err(lan, "soft: gen", old);
              u4_brut(p, "soft: sut", sut);
              u4_brut(p, "soft: mack", u4_kt(lan, u4_atom_hold, sut, gen));
              printf("\n");
            }
          }
          else return u4_trip;
        }
        else return u4_trip;

        return u4_k_trel(lan, u4_atom_hold, sut, gen);
      }
    }
    else if ( u4_b_pqr(gen, u4_atom_trol, &p_gen, &q_gen, &r_gen) ) {
      return _rose_eith
        (p,
         _rose_play(p, _rose_gain(p, sut, p_gen), q_gen),
         _rose_play(p, sut, r_gen));
    }
    else if ( u4_b_pq(gen, u4_atom_sing, &p_gen, &q_gen) ) {
      return u4_k_trel
        (lan,
         u4_atom_fork,
         u4_k_cell(lan, u4_atom_cube, u4_yes),
         u4_k_cell(lan, u4_atom_cube, u4_no));
    }
    else {
      return _rose_play(p, sut, _plow_open(p, gen)); 
    }
  }
u4_type
_rose_play(u4_plow p,
           u4_type sut,
           u4_gene gen)
{
  u4_lane lan = p->lan;
  u4_noun mum = u4_kc(lan, sut, gen);
  u4_nopt zod = u4_tab_get(mum, p->zor);

  if ( zod != u4_bull ) {
    return zod;
  }
  else {
    u4_type gur = _rose_play_main(p, sut, gen);

    p->zor = u4_tab_add(lan, mum, gur, p->zor);
    return gur;
  }
}

/* repo:rose:plow
*/
u4_type
_rose_repo(u4_plow p,
           u4_type sut,
           u4_gene gen)
{
  u4_lane lan = p->lan;
  u4_noun vax = u4_k_cell(lan, sut, gen);

  if ( u4_bag_in(vax, p->fan) ) {
    return _plow_fail(p, "inference recursion");
  }
  else {
    u4_noun fan;
    u4_type gex;

    fan = p->fan;
    p->fan = u4_bag_add(lan, vax, p->fan);

    gex = _rose_play(p, sut, gen);
    p->fan = fan;
    return gex;
  }
}

/* seek:rose:plow
*/
  static u4_plan
  _rose_seek_a(u4_plow p,
               u4_type sut,
               u4_rope rop,
               u4_axis axe,
               u4_unit act)
  {
    u4_lane lan = p->lan;

    if ( u4_n_zero(rop) ) {
      return u4_k_trel(lan, axe, act, sut);
    }
    else {
      u4_noun i_rop = u4_ch(rop);
      u4_noun t_rop = u4_ct(rop);
      u4_noun pi_rop;

      if ( u4_n_atom(i_rop) ) {
        return _rose_seek_a
          (p, sut, 
              u4_k_cell(lan, u4_k_cell(lan, u4_atom_pane, i_rop), t_rop),
              axe,
              act);
      }
      else if ( u4_b_p(i_rop, u4_atom_frag, &pi_rop) ) {
        return _rose_seek_a
          (p, _iris_peek(p, sut, u4_nul, u4_axis_1, pi_rop), 
              t_rop,
              u4_op_peg(lan, axe, pi_rop),
              u4_nul);
      }
      else if ( u4_b_p(i_rop, u4_atom_pane, &pi_rop) ) {
        u4_plan sap = _iris_find(p, sut, u4_nul, u4_axis_1, pi_rop);
        u4_axis p_sap = u4_ch(sap);
        u4_unit q_sap = u4_ch(u4_ct(sap));
        u4_type r_sap = u4_ct(u4_ct(sap));
#if 0
        if ( u4_n_zero(pi_rop) ) {
          u4_err(lan, "blip: p", p_sap);
          u4_err(lan, "blip: q", q_sap);
          u4_err(lan, "blip: r", r_sap);
        }
#endif
        return _rose_seek_a
          (p, r_sap, t_rop, u4_op_peg(lan, axe, p_sap), q_sap);
      }
      else return u4_trip;
    }
  }
u4_plan
_rose_seek(u4_plow p,
           u4_type sut,
           u4_rope rop)
{
  return _rose_seek_a(p, sut, rop, u4_axis_1, u4_nul);
}

/* show:rose:plow
*/
  u4_flag
  _rose_show_nest(u4_plow p,
                  u4_type sut,
                  u4_type bon)
  {
    if ( !u4_so(_iris_nest(p, sut, u4_nul, u4_axis_1, bon)) ) {
      u4_brut(p, "sut", sut);
      u4_brut(p, "bon", bon);
      return _plow_fail(p, "nest: show");
    }
    else {
      return u4_yes;
    }
  }
  static u4_plot
  _rose_show_a(u4_plow p,
               u4_type sut,
               u4_rack dus)
  {
    u4_lane lan = p->lan;

    if ( u4_n_zero(dus) ) {
      return u4_nul;
    }
    else {
      u4_noun i_dus = u4_ch(dus);
      u4_noun t_dus = u4_ct(dus);
      u4_gene pi_dus = u4_ch(i_dus);
      u4_gene qi_dus = u4_ct(i_dus);

      if ( !u4_so(_rose_show(p, sut, qi_dus)) ) {
        return _plow_fail(p, "show: take");
      }
      else {
        u4_type feg    = _rose_play(p, sut, qi_dus);

        return u4_k_cell
          (lan,
           u4_k_cell(lan, pi_dus, feg),
           _rose_play_a(p, sut, t_dus));
      }
    }
  }
  u4_flag
  _rose_show_main(u4_plow p,
                  u4_type sut,
                  u4_gene gen)
  {
    u4_lane lan = p->lan;
    u4_noun p_gen, q_gen, r_gen;

    if ( u4_b_p(gen, u4_atom_zike, &p_gen) ) {
      return u4_yes;
    }
    else if ( u4_b_p(gen, u4_atom_zoot, &p_gen) ) {
      return u4_yes;
    }
    else if ( u4_b_p(gen, u4_atom_vint, &p_gen) ) {
      return u4_and(_rose_show(p, sut, p_gen),
                    _rose_show_nest
                        (p, u4_atom_atom, _rose_play(p, sut, p_gen)));
    }
    else if ( u4_b_pq(gen, u4_atom_cast, &p_gen, &q_gen) ) {
      return u4_and
        (_rose_show(p, sut, p_gen),
         u4_and
          (_rose_show(p, sut, q_gen),
           _rose_show_nest(p, _rose_play(p, sut, p_gen),
                              _rose_play(p, sut, q_gen))));
    }
    else if ( u4_b_pq(gen, u4_atom_twix, &p_gen, &q_gen) ) {
      return u4_and(_rose_show(p, sut, p_gen), _rose_show(p, sut, q_gen));
    }
    else if ( u4_b_p(gen, u4_atom_wost, &p_gen) ) {
      return _rose_show(p, sut, p_gen);
    }
    else if ( u4_b_p(gen, u4_atom_zush, &p_gen) ) {
      u4_flag zic;
      u4_atom bug;

      bug = p->bug;
      p->bug = u4_op_inc(lan, p->bug);

      zic = _rose_show(p, sut, p_gen);
      
      p->bug = bug;
      return zic;
    }
    else if ( u4_b_p(gen, u4_atom_dust, &p_gen) ) {
      return _rose_show(p, sut, p_gen);
    }
    else if ( u4_b_pq(gen, u4_atom_germ, &p_gen, &q_gen) ) {
      return u4_and(_rose_show(p, sut, p_gen),
                    _rose_show(p, sut, q_gen));
    }
    else if ( u4_b_pq(gen, u4_atom_hint, &p_gen, &q_gen) ) {
      return u4_and(_rose_show(p, sut, p_gen),
                    _rose_show(p, sut, q_gen));
    }
    else if ( u4_b_p(gen, u4_atom_grit, &p_gen) ) {
      return _rose_show(p, sut, p_gen);
    }
    else if ( u4_b_pq(gen, u4_atom_tash, &p_gen, &q_gen) ) {
      return u4_yes;
    }
    else if ( u4_b_pq(gen, u4_atom_plin, &p_gen, &q_gen) ) {
      return u4_and(_rose_show(p, sut, p_gen), 
                    _rose_show(p, sut, q_gen));
    }
    else if ( u4_b_pq(gen, u4_atom_flac, &p_gen, &q_gen) ) {
      return u4_and(_rose_show(p, sut, p_gen),
                    _rose_show(p, _rose_play(p, sut, p_gen), q_gen));
    }
    else if ( u4_b_pq(gen, u4_atom_pank, &p_gen, &q_gen) ) {
      sut = _rose_play(p, sut, gen);

      while ( !u4_n_zero(q_gen) ) {
        u4_noun iq_gen = u4_ch(q_gen);
        u4_noun tq_gen = u4_ct(q_gen);
        u4_noun qiq_gen = u4_ct(iq_gen);

        if ( !u4_so(_rose_show(p, sut, qiq_gen)) ) {
          return _plow_fail(p, "menu fail");
        }
        q_gen = tq_gen;
      }
      return u4_yes;
    }
    else if ( u4_b_p(gen, u4_atom_zalt, &p_gen) ) {
      return _rose_show(p, sut, p_gen);
    }
    else if ( u4_b_p(gen, u4_atom_zond, &p_gen) ) {
      return _rose_show(p, sut, p_gen);
    }
    else if ( u4_b_pq(gen, u4_atom_bran, &p_gen, &q_gen) ) {
      return _rose_show(p, sut, q_gen);
    }
    else if ( u4_b_pq(gen, u4_atom_sail, &p_gen, &q_gen) ) {
      return u4_and(_rose_show(p, sut, p_gen),
                    _rose_show(p, sut, q_gen));
    }
    else if ( u4_b_p(gen, u4_atom_bone, &p_gen) ) {
      return u4_yes;
    }
    else if ( u4_b_pq(gen, u4_atom_zemp, &p_gen, &q_gen) ) {
      u4_noun qip;

      p->meb = u4_kc(lan, u4_kc(lan, u4_nul, p_gen), p->meb);
      qip = _rose_show(p, sut, q_gen);
      p->meb = u4_ct(p->meb);

      return qip;
    }
    else if ( u4_b_pq(gen, u4_atom_stil, &p_gen, &q_gen) ) {
      return u4_and
        (_rose_show(p, sut, p_gen),
         u4_and
          (_rose_show(p, sut, q_gen),
           _rose_show_nest
            (p, _rose_play(p, sut, p_gen), _rose_play(p, sut, q_gen))));
    }
    else if ( u4_b_pq(gen, u4_atom_mack, &p_gen, &q_gen) ) {
      u4_plan lar = _rose_seek(p, sut, p_gen);
      u4_noun huz = _rose_show_a(p, sut, q_gen);
      u4_noun q_lar = u4_ch(u4_ct(lar));

      if ( u4_n_zero(q_lar) ) {
        return u4_yes;
      }
      else {
        u4_door uq_lar  = u4_ct(q_lar);
        u4_type quq_lar = u4_ch(u4_ct(uq_lar));
        u4_gene ruq_lar = u4_ct(u4_ct(uq_lar));
        u4_noun p_sut, q_sut, r_sut, s_sut;

        sut = _rose_edit(p, quq_lar, huz);
        gen = ruq_lar;

        if ( u4_b_pqrs(sut, u4_atom_core, &p_sut, &q_sut, &r_sut, &s_sut) ) {
          if ( u4_n_eq(u4_atom_hard, q_sut) ) {
            return _rose_show_nest(p, r_sut, p_sut);
          }
          else if ( u4_n_eq(u4_atom_soft, q_sut) ) {
            u4_noun fuy = u4_k_cell(lan, sut, gen);

            if ( u4_bag_in(fuy, p->ver) ) {
              return u4_yes;
            } else {
              u4_type  gim = u4_k_quil
                  (lan, u4_atom_core, r_sut, q_sut, r_sut, s_sut);
              u4_bag   rev = p->ver;
              u4_flag  goh;

              p->ver = u4_bag_add(lan, fuy, p->ver);
              goh = u4_and
                (_rose_show(p, sut, gen),
                 u4_say(u4_n_eq(_rose_make(p, sut, gen), 
                                _rose_make(p, gim, gen))));

              p->ver = rev;
              if ( !u4_so(goh) ) {
                u4_err(lan, "gen", gen);
                u4_err(lan, "mka", _rose_make(p, sut, gen));
                u4_err(lan, "mkb", _rose_make(p, gim, gen));
                return _plow_fail(p, "soft mack");
              }
              return goh;
            }
          } 
          else return u4_trip;
        }
        else return u4_trip;
      }
    }
    else if ( u4_b_pqr(gen, u4_atom_trol, &p_gen, &q_gen, &r_gen) ) {
      return u4_and
        (_rose_show(p, sut, p_gen),
         u4_and
         (_rose_show(p, _rose_gain(p, sut, p_gen), q_gen),
          _rose_show(p, sut, r_gen)));
    }
    else if ( u4_b_pq(gen, u4_atom_sing, &p_gen, &q_gen) ) {
      return u4_and
        (_rose_show(p, sut, p_gen),
         _rose_show(p, sut, q_gen));
    }
    else {
      u4_gene vul = _plow_open(p, gen);

      if ( u4_n_eq(vul, gen) ) {
        u4_err(lan, "gen", gen);
        u4_err(lan, "vul", vul);
        return u4_trip;
      }

      return _rose_show(p, sut, vul);
    }
  }
u4_flag
_rose_show(u4_plow p,
           u4_type sut,
           u4_gene gen)
{
#if 0
  return u4_yes;
#else
  u4_lane lan = p->lan;
  u4_noun mum = u4_kc(lan, sut, gen);
  u4_nopt zod = u4_tab_get(mum, p->hos);

  if ( zod != u4_bull ) {
    return zod;
  }
  else {
    u4_flag gur = _rose_show_main(p, sut, gen);

    p->hos = u4_tab_add(lan, mum, gur, p->hos);
    return gur;
  }
#endif
}
