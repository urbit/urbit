/* mill/safe.c
**
** This file is in the public domain.
*/
#include "u4/all.h"

/* _flag()::
*/
static u4_type
_flag(u4_lane lane)
{
  return u4_k_trel
    (lane, u4_atom_fork, u4_k_cell(lane, u4_atom_cube, u4_noun_0),
                         u4_k_cell(lane, u4_atom_cube, u4_noun_1));
}

/* _main_load()::
*/
static u4_t
_main_load(u4_milr m, u4_bag gil, u4_type roz, u4_noun q_dug)
{
  if ( u4_n_cell(u4_ch(q_dug)) ) {
    return _main_load(m, gil, roz, u4_ch(q_dug)) &&
           _main_load(m, gil, roz, u4_ct(q_dug));
  }
  else {
    return _mill_safe_in(m, gil, roz, u4_ct(q_dug));
  }
}

/* _safe_loop(): check type, inside memoization.
*/
static u4_t
_safe_loop(u4_milr m,
           u4_bag  gil,
           u4_type naf,
           u4_gene dug)
{
  u4_lane lane = m->lane;

  /* Recursion control.
  */
  {
    u4_noun mel = u4_k_cell(lane, naf, dug);

    if ( u4_bag_in(mel, gil) ) {
      /* Conservative search.
      */
      return 1;
    }
    else {
      gil = u4_bag_add(lane, mel, gil);
    }
  }

  /* Core semantics.
  */
  {
    u4_noun p_dug, q_dug, r_dug;

    if ( u4_b_fork(dug, &p_dug, &q_dug) ) {
      return _mill_safe_in(m, gil, naf, p_dug) && 
             _mill_safe_in(m, gil, naf, q_dug);
    }

    else if ( u4_b_pq(dug, u4_atom_bend, &p_dug, &q_dug) ) {
      return _mill_suss(m, gil, naf, p_dug, q_dug);
    }

    else if ( u4_b_pq(dug, u4_atom_cast, &p_dug, &q_dug) ) {
      if ( !_mill_safe_in(m, gil, naf, p_dug) || 
           !_mill_safe_in(m, gil, naf, q_dug) )
      {
        return 0;
      }
      else {
        u4_type lup = _mill_play(m, naf, p_dug);
        u4_type tog = _mill_play(m, naf, q_dug);

        if ( !_mill_cong(m, lup, tog) ) {
          // u4_burp(lane, "cast: lup", _mill_dump(m, lup));
          // u4_burp(lane, "cast: tog", _mill_dump(m, tog));

          _mill_fail(m, "cast infraction");
          return 0;
        }
        return 1;
      }
    }
  
    else if ( u4_b_pq(dug, u4_atom_coat, &p_dug, &q_dug) ) {
      return _mill_safe_in(m, gil, naf, q_dug);
    }

    else if ( u4_b_p(dug, u4_atom_dbug, &p_dug) ) {
      u4_t gaf;

      m->rux = u4_op_inc(lane, m->rux);
      gaf = _mill_safe_in(m, gil, naf, p_dug);
      m->rux = u4_op_dec(lane, m->rux);

      return gaf;
    }

    else if ( u4_b_pqr(dug, u4_atom_if, &p_dug, &q_dug, &r_dug) ) {
      if ( !_mill_safe_in(m, gil, naf, p_dug) ) {
        return 0;
      }
      else {
        u4_type lag = _mill_play(m, naf, p_dug);

        if ( !_mill_cong(m, _flag(lane), lag) ) {
          return _mill_fail(m, "if abuse");
        }
        else {
          u4_loaf ruf = _mill_hunt(m, naf, p_dug);
          u4_type sec = u4_ch(ruf);
          u4_form nak = u4_ct(ruf);
    
          if ( u4_n_eq(u4_noun_1, u4_ch(nak)) ) {
            if ( u4_n_eq(u4_noun_0, u4_ct(nak)) ) {
              return _mill_safe_in(m, gil, naf, q_dug);
            } else {
              return _mill_safe_in(m, gil, naf, r_dug);
            }
          }
          else {
            return _mill_safe_in(m, gil, _mill_both(m, sec, naf), q_dug) && 
                   _mill_safe_in(m, gil, naf, r_dug);
          }
        }
      }
    }
  
    else if ( u4_b_pq(dug, u4_atom_like, &p_dug, &q_dug) ) {
      if ( !_mill_safe_in(m, gil, naf, p_dug) || 
           !_mill_safe_in(m, gil, naf, q_dug) )
      {
        return 0;
      }
      else {
#if 0
        u4_type fez = _mill_play(m, naf, p_dug);
        u4_type gar = _mill_play(m, naf, q_dug);

        if ( !_mill_cong(m, fez, gar) && !_mill_orth(m, fez, gar) ) {
          _mill_fail(m, "like confusion");
          return 0;
        }
#endif
        return 1;
      }
    }

    else if ( u4_b_p(dug, u4_atom_limb, &p_dug) ) {
      return 1;
    }

    else if ( u4_b_pq(dug, u4_atom_link, &p_dug, &q_dug)) {
      if ( !_mill_safe_in(m, gil, naf, p_dug) ) {
        return 0;
      }
      else return _mill_safe_in(m, gil, _mill_play(m, naf, p_dug), q_dug);
    }

    else if ( u4_b_p(dug, u4_atom_load, &p_dug) ) {
      u4_type roz = u4_k_trel(lane, u4_atom_cone, naf, p_dug);

      return _main_load(m, gil, roz, p_dug);
    }
  
    else if (u4_b_pq(dug, u4_atom_look, &p_dug, &q_dug) ) {
      return _mill_peek(m, gil, naf, p_dug, q_dug);
    }

    else if ( u4_b_pq(dug, u4_atom_raw, &p_dug, &q_dug) ) {
      if ( !_mill_safe_in(m, gil, naf, q_dug) ) {
        return 0;
      }
      else {
        if ( u4_n_eq(u4_noun_3, p_dug) || u4_n_eq(u4_noun_4, p_dug) ) {
          return 1;
        }
        else if ( u4_n_eq(u4_noun_5, p_dug) ) {
          u4_type poz = _mill_play(m, naf, q_dug);

          if ( !_mill_cong(m, u4_atom_atom, poz) ) {
            return _mill_fail(m, "raw bump");
          }
          else return 1;
        }
        else if ( u4_n_eq(u4_noun_6, p_dug) ) {
          u4_type lem = u4_k_trel
            (lane, u4_atom_pair, u4_atom_blur, u4_atom_blur);
          u4_type poz = _mill_play(m, naf, q_dug);

          if ( !_mill_cong(m, lem, poz) ) {
            return _mill_fail(m, "raw same");
          }
          else return 1;
        }
        else return u4_trip;
      }
    }
  
    else if ( u4_b_p(dug, u4_atom_rock, &p_dug) ) {
      return 1;
    }

    else if ( u4_b_pq(dug, u4_atom_site, &p_dug, &q_dug) ) {
      u4_noun fob = m->zud;
      u4_t    gaf;

      m->zud = p_dug;
      gaf = _mill_safe_in(m, gil, naf, q_dug);
      m->zud = fob;

      return gaf;
    }

    else if ( u4_b_pq(dug, u4_atom_spot, &p_dug, &q_dug) ) {
      u4_noun fob = m->nix;
      u4_t    gaf;

      m->nix = p_dug;
      gaf = _mill_safe_in(m, gil, naf, q_dug);
      m->nix = fob;

      return gaf;
    }
  
    else if ( u4_b_pqr(dug, u4_atom_sure, &p_dug, &q_dug, &r_dug) ) {
      if ( !_mill_safe_in(m, gil, naf, p_dug) || 
           !_mill_safe_in(m, gil, naf, q_dug) )
      {
        return 0;
      }
      else {
        u4_type lop = _mill_play(m, naf, p_dug);
        u4_type huq = _mill_play(m, naf, q_dug);

        if ( !_mill_cong(m, huq, lop) ) {
          return _mill_fail(m, "sure violation");
        }
        else {
          return _mill_safe_in(m, gil, naf, r_dug);
        }
      }
    }
  
    else {
      return _mill_safe_in(m, gil, naf, _mill_open(m, dug));
    }
  }
}

/* _mill_safe_in(): check type, with recursion control.
*/
u4_t
_mill_safe_in(u4_milr m,
              u4_bag  gil,
              u4_type naf,
              u4_gene dug)
{
  u4_lane lane = m->lane;
  u4_noun fid  = u4_k_cell(lane, naf, dug);

  /* Memoization.  Can we do this without gil?  Yes.
  */
  {
    if ( u4_bag_in(fid, m->tyx) ) {
      return 1;
    }
    else {
      u4_t t = _safe_loop(m, gil, naf, dug);

      u4_assert(t);
      m->tyx = u4_bag_add(lane, fid, m->tyx);
      return 1;
    }
  }
}

/* _mill_safe(): check type.
*/
u4_t
_mill_safe(u4_milr m,
           u4_type naf, 
           u4_gene dug)
{
  return _mill_safe_in(m, u4_noun_0, naf, dug);
}
