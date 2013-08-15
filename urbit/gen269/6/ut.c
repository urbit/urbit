/* j/6/ut.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"

/* debugging hacks
*/
  static void
  _dump_wall(u2_wire     wir_r,
             const c3_c* cap_c,
             u2_noun     wal)
  {
    if ( cap_c ) printf("%s:\n", cap_c);

    while ( u2_nul != wal ) {
      u2_noun h_wal = u2_h(wal);

      while ( u2_nul != h_wal ) {
        c3_assert(u2_h(h_wal) < 128);

        putchar(u2_h(h_wal));
        h_wal = u2_t(h_wal);
      }
      putchar(10);
      wal = u2_t(wal);
    }
  }
  void
  j2_mcy(Pit, ut, dupt)(u2_wire     wir_r,
                          u2_noun     van,
                          const c3_c* cap_c,
                          u2_noun     typ)
  {
    u2_noun vin;
    u2_noun pup, cul, col, fly, wal;

    vin = u2_bn_molt(wir_r, van, 4, typ, 0);
    pup = u2_bn_hook(wir_r, vin, "dump");
    cul = u2_bn_hook(wir_r, van, "to");
    col = u2_bn_molt(wir_r, cul, 4, pup, 0); 

    fly = u2_bn_hook(wir_r, col, "fly");

    wal = u2_bn_mung(wir_r, fly, 75);

    _dump_wall(wir_r, cap_c, wal);

    u2_rl_lose(wir_r, vin);
    u2_rl_lose(wir_r, pup);
    u2_rl_lose(wir_r, cul);
    u2_rl_lose(wir_r, col);
    u2_rl_lose(wir_r, fly);
    u2_rl_lose(wir_r, wal);
  }
  static void
  _type_in(u2_wire wir_r, 
           u2_noun typ)
  {
    u2_noun p_typ, q_typ, r_typ;

    if ( u2_no == u2_dust(typ) ) switch ( typ ) {
      default: goto fail;

      case c3__atom: break;
      case c3__noun: break;
      case c3__void: break;
    } 
    else switch ( u2_h(typ) ) {
      default: fail: u2_err(wir_r, "bum type", typ); u2_bl_bail(wir_r, c3__fail); break;

      case c3__cell: {
        if ( u2_no == u2_as_cell(u2_t(typ), &p_typ, &q_typ) ) {
          goto fail;
        } else {
          _type_in(wir_r, p_typ);
          _type_in(wir_r, q_typ);
          break;
        }
      }
      case c3__core: {
        if ( u2_no == u2_as_trel(u2_t(typ), &p_typ, &q_typ, &r_typ) ) {
          goto fail;
        } else {
          _type_in(wir_r, p_typ);

          if ( u2_yes == u2_dust(q_typ) ) {
            _type_in(wir_r, u2_t(q_typ));
          }
          break;
        }
      }
      case c3__cube: { 
        p_typ = u2_t(typ);
        {
          break;
        }
      case c3__face: {
        if ( u2_no == u2_as_cell(u2_t(typ), &p_typ, &q_typ) ) {
          goto fail;
        } else {
          if ( u2_no == u2_stud(p_typ) ) {
            goto fail;
          }
          _type_in(wir_r, q_typ);
          break;
        }
      }
      case c3__fork: {
        if ( u2_no == u2_as_cell(u2_t(typ), &p_typ, &q_typ) ) {
          goto fail;
        } else {
          _type_in(wir_r, p_typ);
          _type_in(wir_r, q_typ);
          break;
        }
      }
      case c3__hold:
        if ( u2_no == u2_as_cell(u2_t(typ), &p_typ, &q_typ) ) {
          goto fail;
        } else {
          _type_in(wir_r, p_typ);
          break;
        }
      }
    } 
  }
  void
  j2_mby(Pit, type)(u2_wire  wir_r,
                    u2_noun  typ)
  {
    u2_rl_ok(wir_r, typ);
    _type_in(wir_r, typ);
  }

/* declarations
*/
  u2_noun                                                         //  transfer
  j2_mc(Pit, ut, repo)(u2_wire wir_r, 
                       u2_noun cor);                              //  retain
  u2_noun                                                         //  transfer
  j2_mc(Pit, ut, burn)(u2_wire wir_r, 
                       u2_noun cor);                              //  retain

  extern u2_ho_jet j2_mcj(Pit, ut, bake)[];
  extern u2_ho_jet j2_mcj(Pit, ut, cull)[];
  extern u2_ho_jet j2_mcj(Pit, ut, edit)[];
  extern u2_ho_jet j2_mcj(Pit, ut, emit)[];
  extern u2_ho_jet j2_mcj(Pit, ut, find)[];
  extern u2_ho_jet j2_mcj(Pit, ut, fire)[];
  extern u2_ho_jet j2_mcj(Pit, ut, fish)[];
  extern u2_ho_jet j2_mcj(Pit, ut, fret)[];
  extern u2_ho_jet j2_mcj(Pit, ut, fuse)[];
  extern u2_ho_jet j2_mcj(Pit, ut, gain)[];
  extern u2_ho_jet j2_mcj(Pit, ut, heal)[];
  extern u2_ho_jet j2_mcj(Pit, ut, mint)[];
  extern u2_ho_jet j2_mcj(Pit, ut, nest)[];
  extern u2_ho_jet j2_mcj(Pit, ut, orth)[];
  extern u2_ho_jet j2_mcj(Pit, ut, park)[];
  extern u2_ho_jet j2_mcj(Pit, ut, peek)[];
  extern u2_ho_jet j2_mcj(Pit, ut, play)[];
  extern u2_ho_jet j2_mcj(Pit, ut, rest)[];
  extern u2_ho_jet j2_mcj(Pit, ut, seek)[];
  extern u2_ho_jet j2_mcj(Pit, ut, snap)[];
  extern u2_ho_jet j2_mcj(Pit, ut, tuck)[];
  extern u2_ho_jet j2_mcj(Pit, ut, tusk)[];

/* structures
*/
  u2_ho_driver 
  j2_mbd(Pit, ut)[] = {
    { j2_sc(Pit, ut, bake), j2_mcj(Pit, ut, bake), 0, 0, u2_none },
    { j2_sc(Pit, ut, cull), j2_mcj(Pit, ut, cull), 0, 0, u2_none },
    { j2_sc(Pit, ut, edit), j2_mcj(Pit, ut, edit), 0, 0, u2_none },
    { j2_sc(Pit, ut, emit), j2_mcj(Pit, ut, emit), 0, 0, u2_none },
    { j2_sc(Pit, ut, find), j2_mcj(Pit, ut, find), 0, 0, u2_none },
    { j2_sc(Pit, ut, fire), j2_mcj(Pit, ut, fire), 0, 0, u2_none },
    { j2_sc(Pit, ut, fish), j2_mcj(Pit, ut, fish), 0, 0, u2_none },
    { j2_sc(Pit, ut, fret), j2_mcj(Pit, ut, fret), 0, 0, u2_none },
    { j2_sc(Pit, ut, fuse), j2_mcj(Pit, ut, fuse), 0, 0, u2_none },
    { j2_sc(Pit, ut, gain), j2_mcj(Pit, ut, gain), 0, 0, u2_none },
    { j2_sc(Pit, ut, heal), j2_mcj(Pit, ut, heal), 0, 0, u2_none },
    { j2_sc(Pit, ut, mint), j2_mcj(Pit, ut, mint), 0, 0, u2_none },
    { j2_sc(Pit, ut, nest), j2_mcj(Pit, ut, nest), 0, 0, u2_none },
    { j2_sc(Pit, ut, orth), j2_mcj(Pit, ut, orth), 0, 0, u2_none },
    { j2_sc(Pit, ut, park), j2_mcj(Pit, ut, park), 0, 0, u2_none },
    { j2_sc(Pit, ut, peek), j2_mcj(Pit, ut, peek), 0, 0, u2_none },
    { j2_sc(Pit, ut, play), j2_mcj(Pit, ut, play), 0, 0, u2_none },
    { j2_sc(Pit, ut, rest), j2_mcj(Pit, ut, rest), 0, 0, u2_none },
    { j2_sc(Pit, ut, seek), j2_mcj(Pit, ut, seek), 0, 0, u2_none },
    { j2_sc(Pit, ut, snap), j2_mcj(Pit, ut, snap), 0, 0, u2_none },
    { j2_sc(Pit, ut, tuck), j2_mcj(Pit, ut, tuck), 0, 0, u2_none },
    { j2_sc(Pit, ut, tusk), j2_mcj(Pit, ut, tusk), 0, 0, u2_none },
    { }
  };

  u2_ho_jet 
  j2_mbj(Pit, ut)[] = {
    { "burn", c3__hevy, j2_mc(Pit, ut, burn), Tier6_b, u2_none, u2_none },
    { "repo", c3__hevy, j2_mc(Pit, ut, repo), Tier6_b, u2_none, u2_none },
    { }
  };

  u2_ho_driver
  j2_db(Pit, ut) = 
    { j2_sb(Pit, ut), j2_mbj(Pit, ut), j2_mbd(Pit, ut), 0, u2_none };
