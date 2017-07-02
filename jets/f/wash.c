
/* j/f/wash.c
**
*/
#include "all.h"

/* functions
*/

  u3_noun _re_ram(u3_noun tac);
  u3_noun _re_win_buc(u3_noun tac, u3_noun tab, u3_noun edg, u3_noun lug);

  // stem/bulb -> tem/bub
  u3_noun
  _re_ram_leaf(u3_noun bub)
  {
    return u3k(bub);
  }

  // p and r as in p.p.tac, q.p.tac
  // res as in rest (q.tac)
  u3_noun
  _re_ram_rose_in(u3_noun p,
                  u3_noun r,
                  u3_noun res)
  {
    if ( u3_nul == res ) {
      return u3k(r);
    }
    else if ( c3n == u3du(res) ) {
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun voz = _re_ram_rose_in(p, r, u3t(res)),
              dex = _re_ram(u3h(res)),
              sin = ( u3_nul == u3t(res) ) ? voz : u3kb_weld(u3k(p), voz);

      return u3kb_weld(dex, sin);
    }
  }

  u3_noun
  _re_ram_rose(u3_noun bub)
  {
    u3_noun tre, res, p, q, r;
    u3x_cell(bub, &tre, &res);
    u3x_trel(tre, &p, &q, &r);
    return u3kb_weld(u3k(q), _re_ram_rose_in(p, r, res));
  }

  u3_noun
  _re_ram_palm(u3_noun bub)
  {
    u3_noun p, q, r, s, qua, res, pur, rob, pro;

    u3x_cell(bub, &qua, &res);
    u3x_qual(qua, &p, &q, &r, &s);
    pur = u3nt(u3k(p), u3qb_weld(q, r), u3k(s));
    rob = u3nc(pur, u3k(res));
    pro = _re_ram_rose(rob);
    u3z(rob);

    return pro;
  }

  u3_noun
  _re_ram(u3_noun tac)
  {
    u3_noun tem, bub;

    u3x_cell(tac, &tem, &bub);
    switch ( tem ) {
      case c3__leaf:
        return _re_ram_leaf(bub);
      case c3__palm:
        return _re_ram_palm(bub);
      case c3__rose:
        return _re_ram_rose(bub);
      default:
        return u3m_bail(c3__exit);
    }
  }

  u3_noun
  _re_win_rig(u3_noun hom,
              u3_noun tab,
              u3_noun lug)
  {
    return u3nc(u3qe_runt(tab, ' ', hom), u3k(lug));
  }

  u3_noun
  _re_win_leaf(u3_noun tac,
               u3_noun tab,
               u3_noun edg,
               u3_noun lug)
  {
    return _re_win_rig(u3t(tac), tab, lug);
  }

  u3_noun
  _re_win_fit(u3_noun tac, u3_noun tab, u3_noun edg)
  {
    u3_noun ram = _re_ram(tac),
            len = u3kb_lent(ram),
            dif = u3qa_sub(edg, tab);

    return u3ka_lte(len, dif);
  }

  u3_noun
  _re_win_palm_qyr(u3_noun tab,
                   u3_noun edg,
                   u3_noun lyn,
                   u3_noun res,
                   u3_noun lug)
  {
    if ( u3_nul == res ) {
      return u3k(lug);
    }
    else if ( c3n == u3du(res) ) {
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun cat = u3h(res),
              sub = u3qa_sub(lyn, 2),
              bat = u3qa_add(tab, sub),
              gul = _re_win_palm_qyr(tab, edg, sub, u3t(res), lug),
              pro = _re_win_buc(cat, bat, edg, gul);
      u3z(sub);
      u3z(bat);
      u3z(gul);
      return pro;
    }
  }

  c3_y
  _re_win_wig_mir(u3_noun mir,
                  u3_noun wug)
  {
    if ( u3_nul == mir ) {
      return c3n;
    }
    else if ( c3n == u3du(mir) ) {
      return u3m_bail(c3__exit);
    }
    else if ( 0 == wug ) {
      return c3y;
    }
    else {
      if ( ' ' != u3h(mir) ) {
        return c3n;
      }
      else {
        u3_noun guw   = u3qa_dec(wug);
        c3_y    pro_y = _re_win_wig_mir(u3t(mir), guw);
        u3z(guw);
        return pro_y;
      }
    }
  }

  u3_noun
  _re_win_wig(u3_noun hom,
              u3_noun tab,
              u3_noun edg,
              u3_noun lug)
  {
    if ( u3_nul == lug ) {
      return _re_win_rig(hom, tab, lug);
    }
    else if ( c3n == u3du(lug) ) {
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun lin = u3qb_lent(hom),
              wug = u3ka_add(1, u3qa_add(tab, lin)),
              pro;
      if ( c3n == _re_win_wig_mir(u3h(lug), wug) ) {
        pro = _re_win_rig(hom, tab, lug);
      }
      else {
        u3_noun sin = u3nc(' ', u3qb_slag(wug, u3h(lug))),
                moh = u3kb_weld(u3k(hom), sin),
                dex = u3ke_runt(u3k(tab), ' ', moh);
        pro = u3nc(dex, u3k(u3t(lug)));
      }
      u3z(wug);
      u3z(lin);
      return pro;
    }
  }

  u3_noun
  _re_win_palm(u3_noun tac,
               u3_noun tab,
               u3_noun edg,
               u3_noun lug)
  {
    if ( c3y == _re_win_fit(tac, tab, edg) ) {
      u3_noun ram = _re_ram(tac),
              pro = _re_win_rig(ram, tab, lug);
      u3z(ram);
      return pro;
    }
    else {
      u3_noun tem, bub, p, q, r, s, res, qua;

      u3x_cell(tac, &tem, &bub);
      u3x_cell(bub, &qua, &res);
      u3x_qual(qua, &p, &q, &r, &s);

      if ( u3_nul == res ) {
        return _re_win_rig(q, tab, lug);
      }
      else if ( c3n == u3du(res) ) {
        return u3m_bail(c3__exit);
      }
      else if ( u3_nul == u3t(res) ) {
        u3_noun bat = u3qa_add(2, tab),
                gul = _re_win_buc(u3h(res), tab, edg, lug),
                pro = _re_win_rig(q, bat, gul);
        u3z(bat);
        u3z(gul);
        return pro;
      }
      else {
        u3_noun lyn = u3ka_mul(2, u3qb_lent(res)),
                qyr = _re_win_palm_qyr(tab, edg, lyn, res, lug),
                pro = _re_win_wig(q, tab, edg, qyr);
        u3z(lyn);
        u3z(qyr);
        return pro;
      }
    }
  }

  u3_noun
  _re_win_din(u3_noun tab,
              u3_noun edg)
  {
    return u3ka_mod(u3qa_add(2,tab), u3ka_mul(2, u3qa_div(edg, 3)));
  }

  u3_noun
  _re_win_rose_lug(u3_noun r,
                   u3_noun tab,
                   u3_noun edg,
                   u3_noun res,
                   u3_noun lug)
  {
    if ( u3_nul == res ) {
      if ( u3_nul == r ) {
        return u3k(lug);
      }
      else {
        return _re_win_rig(r, tab, lug);
      }
    }
    else if ( c3n == u3du(res) ) {
      return u3m_bail(c3__exit);
    }
    else {
      u3_noun cat = u3h(res),
              gul = _re_win_rose_lug(r, tab, edg, u3t(res), lug),
              bat = _re_win_din(tab, edg),
              pro = _re_win_buc(cat, bat, edg, gul);
      u3z(gul);
      u3z(bat);
      return pro;
    }
  }

  u3_noun
  _re_win_rose(u3_noun tac,
               u3_noun tab,
               u3_noun edg,
               u3_noun lug)
  {
    u3_noun tem, bub, tre, res, p, q, r;

    u3x_cell(tac, &tem, &bub);
    u3x_cell(bub, &tre, &res);
    u3x_trel(tre, &p, &q, &r);

    if ( c3y == _re_win_fit(tac, tab, edg) ) {
      u3_noun ram = _re_ram(tac),
              pro = _re_win_rig(ram, tab, lug);
      u3z(ram);
      return pro;
    }
    else {
      u3_noun gul = _re_win_rose_lug(r, tab, edg, res, lug);
      if ( u3_nul == q ) {
        return gul;
      }
      else {
        u3_noun pro = _re_win_wig(q, tab, edg, gul);
        u3z(gul);
        return pro;
      }
    }
  }

  u3_noun
  _re_win_buc(u3_noun tac,
              u3_noun tab,
              u3_noun edg,
              u3_noun lug)
  {
    if ( c3n == u3du(tac) ) {
      return u3m_bail(c3__exit);
    }

    switch ( u3h(tac) ) {
      case c3__leaf:
        return _re_win_leaf(tac, tab, edg, lug);
      case c3__palm:
        return _re_win_palm(tac, tab, edg, lug);
      case c3__rose:
        return _re_win_rose(tac, tab, edg, lug);
      default:
        return u3m_bail(c3__exit);
    }
  }

  u3_noun
  _re_win(u3_noun tac,
          u3_noun tab,
          u3_noun edg)
  {
    return _re_win_buc(tac, tab, edg, u3_nul);
  }

  u3_noun
  u3qf_wash(u3_noun tab,
            u3_noun edg,
            u3_noun tac)
  {
    return _re_win(tac, tab, edg);
  }

  u3_noun
  u3wf_wash(u3_noun cor)
  {
    u3_noun tab, edg, tac;

    if ( c3n == u3r_mean(cor,
          u3x_sam_4, &tab,
          u3x_sam_5, &edg,
          u3x_sam_3, &tac, 0) ) {
      return u3m_bail(c3__exit);
    }
    else {
      return u3qf_wash(tab, edg, tac);
    }
  }
