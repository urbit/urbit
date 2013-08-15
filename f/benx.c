/* f/benx.c
**
** This file is in the public domain.
*/
#include "all.h"
 
/* u2_bx_boot(): reset the performance log.
*/
void
u2_bx_boot(u2_ray wir_r)
{
  u2_ray bex_r;

  if ( 0 == (bex_r = u2_wire_bex_r(wir_r)) ) {
    return;
  } else {
    u2_benx_at(bex_r, zat) = u2_nul;
    u2_benx_at(bex_r, zof) = u2_nul;

    u2_benx_be(bex_r, c3_d, sap_d) = 0;
    u2_benx_be(bex_r, c3_d, cop_d) = 0;
    u2_benx_be(bex_r, c3_d, det_d) = 0;
    u2_benx_be(bex_r, c3_d, jax_d) = 0;
    u2_benx_be(bex_r, c3_d, use_d) = 0;

    u2_benx_be(bex_r, c3_w, wac_w) = 0;
    u2_benx_be(bex_r, c3_w, wax_w) = 0;

    u2_benx_be(bex_r, c3_w, lif_w) = u2_soup_liv_w(u2_rail_rut_r(wir_r));
    u2_benx_be(bex_r, c3_w, bos_w) = 
      u2_soup_liv_w(u2_rail_rut_r(u2_wire_bas_r(wir_r)));

    {
      struct timeval tv;

      gettimeofday(&tv, 0);
      u2_benx_at(bex_r, sec_w) = tv.tv_sec;
      u2_benx_at(bex_r, usc_w) = tv.tv_usec;
    }
  }
}

/* u2_bx_post(): export and reset the performance log.
**
**  zat: source position stack (on shed)
**  zof: programer action stack (on shed)
**  sap: number of steps
**  cop: number of words copied
**  det: number of identical nouns compared
**  jax: number of jet activations
**  use: number of user counts
**  wax: maximum depth of C stack
**  viq: words in wire allocated
**  zor: words in basket allocated
**  ums: number of milliseconds consumed
*/
u2_bean
u2_bx_post(u2_ray   wir_r,
           u2_noun* zat,
           u2_noun* zof, 
           c3_d*    sap_d,
           c3_d*    cop_d,
           c3_d*    det_d,
           c3_d*    jax_d,
           c3_d*    use_d,
           c3_w*    wax_w,
           c3_ws*   viq_ws,
           c3_ws*   zor_ws,
           c3_w*    ums_w)
{
  u2_ray bex_r;

  if ( 0 == (bex_r = u2_wire_bex_r(wir_r)) ) {
    return u2_no;
  } else {
    c3_w   sec_w, usc_w;

    *zat = u2_benx_at(bex_r, zat);
    *zof = u2_benx_at(bex_r, zof);

    *sap_d = u2_benx_be(bex_r, c3_d, sap_d);
    *cop_d = u2_benx_be(bex_r, c3_d, cop_d);
    *det_d = u2_benx_be(bex_r, c3_d, det_d);
    *jax_d = u2_benx_be(bex_r, c3_d, jax_d);
    *use_d = u2_benx_be(bex_r, c3_d, use_d);

    *wax_w = u2_benx_at(bex_r, wax_w);

    *viq_ws = u2_soup_liv_w(u2_rail_rut_r(wir_r)) - 
              u2_benx_be(bex_r, c3_w, lif_w);

    *zor_ws = u2_soup_liv_w(u2_rail_rut_r(u2_wire_bas_r(wir_r))) - 
              u2_benx_be(bex_r, c3_w, bos_w);

    sec_w = u2_benx_at(bex_r, sec_w);
    usc_w = u2_benx_at(bex_r, usc_w);
    u2_bx_boot(wir_r);

    /* Measure and return time change.
    */
    {
      c3_d old_d, new_d;

      old_d = sec_w;
      old_d *= 1000000ULL;
      old_d += usc_w;

      new_d = u2_benx_at(bex_r, sec_w);
      new_d *= 1000000ULL;
      new_d += u2_benx_at(bex_r, usc_w);

      *ums_w = (c3_w) (((new_d - old_d) + 999ULL) / 1000ULL);
    }
    return u2_yes;
  }
}

/* u2_bx_step(): note interpreter step.
*/
void
u2_bx_step(u2_ray wir_r)
{
  u2_ray bex_r;

  if ( 0 == (bex_r = u2_wire_bex_r(wir_r)) ) {
    return;
  } else {
    u2_benx_be(bex_r, c3_d, sap_d) += (c3_d) 1;
  }
}

/* u2_bx_copy(): note `cop` copied words.
*/
void 
u2_bx_copy(u2_ray wir_r,
           c3_w   cop_w)
{
  u2_ray bex_r;

  if ( 0 == (bex_r = u2_wire_bex_r(wir_r)) ) {
    return;
  } else {
    u2_benx_be(bex_r, c3_d, cop_d) += (c3_d) cop_w;
  }
}

/* u2_bx_dent(): note `det` identical comparisons.
*/
void 
u2_bx_dent(u2_ray wir_r,
           c3_w   det_w)
{
  u2_ray bex_r;

  if ( 0 == (bex_r = u2_wire_bex_r(wir_r)) ) {
    return;
  } else {
    u2_benx_be(bex_r, c3_d, det_d) += (c3_d) det_w;
  }
}

/* u2_bx_sink(): go deeper (call) in the C stack.
*/
void
u2_bx_sink(u2_ray wir_r)
{
  u2_ray bex_r;

  if ( 0 == (bex_r = u2_wire_bex_r(wir_r)) ) {
    return;
  } else {
    u2_benx_at(bex_r, wac_w) += 1;

    if ( u2_benx_at(bex_r, wac_w) > u2_benx_at(bex_r, wax_w) ) {
      u2_benx_at(bex_r, wax_w) = u2_benx_at(bex_r, wac_w);
    }
  }
}

/* u2_bx_rise(): go shallower (return) in the C stack.
*/
void 
u2_bx_rise(u2_ray wir_r)
{
  u2_ray bex_r;

  if ( 0 == (bex_r = u2_wire_bex_r(wir_r)) ) {
    return;
  } else {
    u2_benx_at(bex_r, wac_w) -= 1;
  }
}

/* u2_bx_used(): report a user count.
*/
void
u2_bx_used(u2_ray wir_r)
{
  u2_ray bex_r;

  if ( 0 == (bex_r = u2_wire_bex_r(wir_r)) ) {
    return;
  } else {
    u2_benx_be(bex_r, c3_d, use_d) += (c3_d) 1;
  }
}

/* u2_bx_flew(): report a jet activation.
*/
void
u2_bx_flew(u2_ray wir_r)
{
  u2_ray bex_r;

  if ( 0 == (bex_r = u2_wire_bex_r(wir_r)) ) {
    return;
  } else {
    u2_benx_be(bex_r, c3_d, jax_d) += (c3_d) 1;
  }
}

/* u2_bx_spot(): declare source position.
*/
void
u2_bx_spot(u2_ray  wir_r,
           u2_noun hod)                                           //  transfer
{
  u2_ray bex_r, bas_r;

  if ( (0 == (bex_r = u2_wire_bex_r(wir_r))) ||
       (0 == (bas_r = u2_wire_bas_r(wir_r))) )
  {
    u2_rl_lose(wir_r, hod);
    return;
  } 
  else {
    u2_noun sud = u2_rl_take(bas_r, hod);

    u2_rl_lose(wir_r, hod);
    if ( u2_none == sud ) {
      return;
    } else {
      u2_rl_lose(wir_r, u2_benx_at(bex_r, zat));

      u2_benx_at(bex_r, zat) = sud;
    }
  }
}
void
u2_bx_spot_out(u2_ray wir_r)
{
  u2_ray bex_r, bas_r;

  if ( (0 == (bex_r = u2_wire_bex_r(wir_r))) ||
       (0 == (bas_r = u2_wire_bas_r(wir_r))) )
  {
    return;
  } 
  else {
    u2_noun zat = u2_benx_at(bex_r, zat);

    c3_assert(u2_nul != zat);

    u2_benx_at(bex_r, zat) = u2_t(zat);
    u2_rl_lose(wir_r, zat);
  }
}

/* u2_bx_bean_ent(), u2_bx_bean_out(): enter and exit source position.
*/
void
u2_bx_bean_ent(u2_ray  wir_r,
               u2_noun hod)                                       //  transfer
{
  u2_ray bex_r, bas_r;

  if ( (0 == (bex_r = u2_wire_bex_r(wir_r))) ||
       (0 == (bas_r = u2_wire_bas_r(wir_r))) )
  {
    u2_rl_lose(wir_r, hod);
    return;
  } 
  else {
    u2_noun naz = u2_rl_uniq(wir_r, hod);

    u2_rl_lose(wir_r, hod);
    if ( u2_none != naz ) { 
      u2_noun zof = u2_rc
        (bas_r, u2_rx(bas_r, naz), u2_rx(bas_r, u2_benx_at(bex_r, zof)));
      
      if ( u2_none != zof ) {
        u2_rl_lose(bas_r, u2_benx_at(bex_r, zof));
        u2_benx_at(bex_r, zof) = zof;
      }
    }
  }
}

void
u2_bx_bean_out(u2_ray wir_r)
{
  u2_ray bex_r, bas_r;

  if ( (0 == (bex_r = u2_wire_bex_r(wir_r))) ||
       (0 == (bas_r = u2_wire_bas_r(wir_r))) )
  {
    return;
  } 
  else {
    u2_noun zof = u2_benx_at(bex_r, zof);

    c3_assert(u2_nul != zof);

    u2_benx_at(bex_r, zof) = u2_rx(bas_r, u2_t(zof));
    u2_rl_lose(bas_r, zof);
  }
}

/* _print_tape(): print a byte tape.
*/
static void
_print_tape(u2_noun som,
            FILE*   fil_F)
{
  u2_noun h_som;

  while ( (u2_yes == u2_dust(som)) && ((h_som = u2_h(som)) < 128) ) {
    putc(h_som, fil_F);
    som = u2_t(som);
  }
}

/* _print_term(): print a terminal.
*/
static void
_print_term(u2_noun som,
            FILE*   fil_F)
{
  if ( u2_yes == u2_stud(som) ) {
    c3_w len_w = u2_met(3, som);
    c3_y *som_y = alloca(len_w) + 1;

    u2_bytes(0, len_w, som_y, som);
    som_y[len_w] = 0;
    fprintf(fil_F, "%s", (c3_c *)som_y);
  }
}

/* _print_space(): print `feq_w` spaces.
*/
static void
_print_space(c3_w  feq_w,
             FILE* fil_F)
{
  while ( feq_w-- ) {
    putc(' ', fil_F);
  }
}

/* _print_wall(): print debug wall.
*/
static void
_print_wall(u2_noun wal,
            FILE* fil_F)
{
  while ( u2_yes == u2_dust(wal) ) {
    _print_tape(u2_h(wal), fil_F);
    putc('\n', fil_F);
    wal = u2_t(wal);
  }
}
            
/* u2_bx_loaf(): print debug loaf.
*/
void
u2_bx_loaf(u2_ray  wir_r,
           u2_noun luf)                                           //  retain
{
  if ( u2_yes == u2_dust(luf) ) {
    _print_term(u2_h(luf), stdout);
    printf(":\n");
    _print_wall(u2_t(luf), stdout);
  }
}

/* u2_bx_bean_print(): print bean stack to FILE *.
*/
void
u2_bx_bean_print(u2_ray  wir_r,
                 FILE *  fil_F,
                 u2_noun zof)                                     //  retain
{
  while ( u2_yes == u2_dust(zof) ) {
    u2_noun i_zof = u2_h(zof);
    u2_noun t_zof = u2_t(zof);

    if ( u2_yes == u2_stud(i_zof) ) {
      _print_term(i_zof, fil_F);
      fprintf(fil_F, "\n");
    } else {
      u2_noun hi_zof = u2_h(i_zof);
      u2_noun ti_zof = u2_t(i_zof);
      u2_weak gol;

      gol = u2_nk_kick(wir_r, ti_zof);
      if ( u2_none == gol ) {
        _print_term(hi_zof, fil_F);
        fprintf(fil_F, ":!\n");
      }
      else {
        u2_noun gal = gol;

        if ( u2_nul == hi_zof ) {
          while ( u2_yes == u2_dust(gal) ) {
            _print_tape(u2_h(gal), fil_F);
            fprintf(fil_F, "\n");
            gal = u2_t(gal);
          }
        }
        else {
          c3_w feq_w = u2_met(3, hi_zof);

          _print_term(hi_zof, fil_F);
          printf(": ");

          while ( u2_yes == u2_dust(gal) ) {
            if ( gal != gol ) {
              _print_space(feq_w + 2, fil_F);
            }
            _print_tape(u2_h(gal), fil_F);
            fprintf(fil_F, "\n");
            gal = u2_t(gal);
          }
        }
        u2_rl_lose(wir_r, gol);
      }
    }
    zof = t_zof;
  }
}

static void
_bx_print_superdecimal_w(c3_w w)
{
  if ( w < 65536 ) {
    printf("%d", w);
  } else {
    printf("%d+%d", (w >> 16), (w & 65535));
  }
}

static void
_bx_print_superdecimal_ws(c3_ws ws)
{
  if ( ws < 0 ) {
    printf("-");
    _bx_print_superdecimal_w((c3_w) -(ws));
  } else {
    _bx_print_superdecimal_w((c3_w) ws);
  }
}

static void
_bx_print_superdecimal_d(c3_d d)
{
  if ( d > 0x100000000ULL ) {
    _bx_print_superdecimal_w((c3_w)(d >> 32ULL));
    printf(":");
    _bx_print_superdecimal_w((c3_w)(d & 0xffffffffULL));
  }
  else { 
    _bx_print_superdecimal_w((c3_w) d);
  }
}

/* u2_bx_show(): print benchmark report and clear structure.
*/
void
u2_bx_show(u2_ray wir_r)
{
  u2_noun zat, zof;
  c3_d sap_d, cop_d, det_d, jax_d, use_d;
  c3_w wax_w, ums_w;
  c3_ws viq_ws, zor_ws;

  if ( u2_no == u2_bx_post(wir_r, &zat,
                                  &zof,
                                  &sap_d, 
                                  &cop_d, 
                                  &det_d, 
                                  &jax_d, 
                                  &use_d, 
                                  &wax_w, 
                                  &viq_ws, 
                                  &zor_ws,
                                  &ums_w) )
  {
    return;
  } else {
    /* Dump and free trace information, if any.
    */
    {
      u2_ray bas_r = u2_wire_bas_r(wir_r);

      if ( u2_nul != zat ) {
        // u2_noun h_zat = u2_h(zat);
        u2_noun t_zat = u2_t(zat);

        printf("place: %d.%d:%d.%d\n", 
            u2_h(u2_h(t_zat)), u2_t(u2_h(t_zat)),
            u2_h(u2_t(t_zat)), u2_t(u2_t(t_zat)));
        u2_rl_lose(bas_r, zat);
      }

      if ( u2_nul != zof ) {
        printf("trace:\n");
        u2_bx_bean_print(wir_r, stdout, zof);
        u2_rl_lose(bas_r, zof);
      }
    }

    /* Dump performance log.
    */
    {
      printf("<");
      _bx_print_superdecimal_d(sap_d);
      printf(" hops");
      if ( cop_d ) {
        printf(", ");
        _bx_print_superdecimal_d(cop_d);
        printf(" dups");
      }
      if ( det_d ) {
        printf(", ");
        _bx_print_superdecimal_d(det_d);
        printf(" nods");
      }
      if ( use_d ) {
        printf(", ");
        _bx_print_superdecimal_d(use_d);
        printf(" pings");
      }
      printf(", ");
      _bx_print_superdecimal_w(wax_w);
      printf(" deep");

      if ( viq_ws ) {
        printf("; ");
        _bx_print_superdecimal_ws(viq_ws);
        printf(" kept");
      }
      if ( zor_ws ) {
        printf(", ");
        _bx_print_superdecimal_ws(zor_ws);
        printf(" held");
      }
      
      printf("; ");
      _bx_print_superdecimal_w(ums_w);
      printf(" ms>\n");
    }
  }
}
