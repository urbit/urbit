/* c/eyre.c
**
** This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <gmp.h>
#include <stdint.h>

#define U2_GLOBAL
#define C3_GLOBAL
#include "all.h"

#define GUNN
// #define PROBE    //  probe one stage ahead
// #define DPROBE   //  probe two stages ahead
// #define PERF
// #define PERF_REAM

#define EyreFirstKernel 225     //  counts down; max 264; > 259 needs nock7
u2_flag EyreSmoke;

  /**  Global kernel - used only for trace printing.
  **/
    u2_noun Ken   = u2_nul;
    c3_w    Kno_w = EyreFirstKernel;
  
    /* External drivers.
    */
      extern u2_ho_driver j2_da(k_223);
      extern u2_ho_driver j2_da(k_224);
      extern u2_ho_driver j2_da(k_225);

    /* Built-in battery drivers.   Null `cos` terminates. 
    */
      u2_ho_driver *HostDriverBase[] = {
        &j2_da(k_223),
        &j2_da(k_224),
        &j2_da(k_225),
        0
      };

  /**  Jet dependencies.  Minimize these.
  **/
#   define Pt5Y   k_225__a__b__c__d__e

    u2_noun
    j2_mby(Pt5Y, cue)(u2_wire, u2_noun a);

    u2_noun
    j2_mby(Pt5Y, jam)(u2_wire, u2_noun a);

#   define _eyre_cue  j2_mby(Pt5Y, cue)
#   define _eyre_jam  j2_mby(Pt5Y, jam)

  /**  Forward declarations.
  **/
    static void
    _eyre_print_trac(u2_wire, u2_noun, u2_noun);

/* _eyre_trac(): print trace, if any; produce nul.
*/
static u2_noun                                                    //  direct
_eyre_trac(u2_wire wir_r)
{
  u2_ray kit_r = u2_bl_open(wir_r);

  if ( u2_bl_set(wir_r) ) {
    u2_bl_done(wir_r, kit_r);
    fprintf(stderr, "  {trace failed!}\n");
    return u2_bl_bail(wir_r, c3__fail);
  }
  else {
    u2_noun tax;

    tax = u2_rx(wir_r, u2_wire_tax(wir_r));
    u2_wire_tax(wir_r) = u2_nul;

    if ( u2_nul == tax ) {
      fprintf(stderr, "{no trace}\n");
    } else {
      fprintf(stderr, "{trace}\n");
      if ( u2_nul == Ken ) {
        fprintf(stderr, "{trace: no kernel}\n");
      }
      else {
        _eyre_print_trac(wir_r, Ken, tax);
      }
      u2_rz(wir_r, tax);
    }
    u2_bl_done(wir_r, kit_r);
  }
  u2_bl_bail(wir_r, c3__fail);
  return u2_nul;
}

/* _eyre_nock(): control and trace wrapper for interpreter.
*/
static u2_noun                                                    //  produce
_eyre_nock(u2_wire wir_r,
           u2_flag rac,                                           //  direct
           u2_noun bus,                                           //  submit
           u2_noun fol)                                           //  retain
{
  u2_noun pro;

  pro = u2_nk_nock(wir_r, bus, fol);

  if ( u2_none != pro ) {
    return pro;
  }
  else if ( u2_yes == rac ) {
    return _eyre_trac(wir_r);
  }
  else return u2_bl_bail(wir_r, c3__fail);
}

/* _eyre_mong(): mong with trace.
*/
static u2_noun                                                    //  produce
_eyre_mong(u2_wire wir_r,
           u2_flag rac,                                           //  direct
           u2_noun gat,                                           //  retain
           u2_noun sam)                                           //  submit
{
  u2_noun pro;

  pro = u2_nk_mong(wir_r, gat, sam);

  if ( u2_none != pro ) {
    return pro;
  }
  else if ( u2_yes == rac ) {
    return _eyre_trac(wir_r);
  }
  else return u2_bl_bail(wir_r, c3__fail);
}

/* _eyre_hook(): hook with unitary sample.
*/
static u2_noun                                                    //  produce
_eyre_hook(u2_wire     wir_r,
           u2_noun     cor,                                       //  retain
           const c3_c* hoc_c,                                     //  retain
           u2_noun     sam)                                       //  submit
{
  u2_noun gat = u2_bn_hook(wir_r, cor, hoc_c);
  u2_noun pro;

  pro = _eyre_mong(wir_r, u2_yes, gat, sam);
  u2_rz(wir_r, gat);
  return pro;
}

/* _eyre_hook_cell(): hook with cell sample.
*/
static u2_noun                                                    //  produce
_eyre_hook_cell(u2_wire     wir_r,
                u2_noun     cor,                                  //  retain
                const c3_c* hoc_c,                                //  retain
                u2_noun     sam_2,                                //  submit
                u2_noun     sam_3)                                //  submit
{
  u2_noun gat = u2_bn_hook(wir_r, cor, hoc_c);
  u2_noun pro;

  pro = _eyre_mong(wir_r, u2_yes, gat, u2_bn_cell(wir_r, sam_2, sam_3));
  u2_rz(wir_r, gat);
  return pro;
}

#if 0
/* _eyre_hook_trel(): hook with trel sample.
*/
static u2_noun                                                    //  produce
_eyre_hook_trel(u2_wire     wir_r,
                u2_noun     cor,                                  //  retain
                const c3_c* hoc_c,                                //  retain
                u2_noun     sam_2,                                //  submit
                u2_noun     sam_6,                                //  submit
                u2_noun     sam_7)                                //  submit
{
  u2_noun gat = u2_bn_hook(wir_r, cor, hoc_c);
  u2_noun pro;

  pro = _eyre_mong
    (wir_r, u2_yes, gat, u2_bn_trel(wir_r, sam_2, sam_6, sam_7));
  u2_rz(wir_r, gat);
  return pro;
}

/* _eyre_hook_qual(): hook with quadruple sample.
*/
static u2_noun                                                    //  produce
_eyre_hook_qual(u2_wire     wir_r,
                u2_noun     cor,                                  //  retain
                const c3_c* hoc_c,                                //  retain
                u2_noun     sam_2,                                //  submit
                u2_noun     sam_6,                                //  submit
                u2_noun     sam_14,                               //  submit
                u2_noun     sam_15)                               //  submit
{
  u2_noun gat = u2_bn_hook(wir_r, cor, hoc_c);
  u2_noun pro;

  pro = _eyre_mong
    (wir_r, u2_yes, gat, u2_bn_qual(wir_r, sam_2, sam_6, sam_14, sam_15));
  u2_rz(wir_r, gat);
  return pro;
}
#endif

/* _eyre_path_int(): 
*/
static c3_c*                                                      //  produce
_eyre_path_int(c3_c* lid_c)                                       //  retain
{
  c3_c* pot_c = malloc(FILENAME_MAX + 1);

  snprintf(pot_c, FILENAME_MAX, "eyre/int/%s", lid_c);
  return pot_c;
}


/* _eyre_path_ken():
*/
static c3_c*                                                      //  produce 
_eyre_path_ken(c3_w kno_w)
{
  c3_c* pot_c = malloc(FILENAME_MAX + 1);

  snprintf(pot_c, FILENAME_MAX, "eyre/ken/%d", kno_w);
  return pot_c;
}

/* _eyre_ken_nuw():
**
**   u2_yes iff `ken` needs to be recompiled.
*/
static u2_flag
_eyre_ken_nuw(c3_w kno_w)
{
  c3_c*   pot_c = _eyre_path_ken(kno_w);
  u2_flag esh;

  esh = u2_ux_fresh(pot_c, "watt", "pile");
  free(pot_c);
  return u2_not(esh);
}

/* _eyre_ken_load_hard():
**
**   Load `ken` from the saved kernel binary.
*/
static u2_noun                                                    //  produce
_eyre_ken_load_hard(u2_wire wir_r,
                    c3_w    kno_w)
{
  c3_c* pot_c = _eyre_path_ken(kno_w);
  u2_noun paq, cun, ken;

  // u2_bx_boot(wir_r);
  paq = u2_ux_read(wir_r, pot_c, "pile");
  cun = _eyre_cue(wir_r, paq);
  printf("hard boot: %s: %x\n", pot_c, u2_mug(cun));

  u2_rz(wir_r, paq);
  free(pot_c);

  ken = u2_rl_take(u2_wire_bas_r(wir_r), cun);
  u2_rz(wir_r, cun);

  Ken = ken;
  Kno_w = kno_w;
  return ken;
}

/* _eyre_ken_load_soft():
**
**   Load `ken` from source, using previous `las`; save the binary.
*/
static u2_noun                                                    //  produce
_eyre_ken_load_soft(u2_wire wir_r,
                    u2_noun las,                                  //  retain
                    c3_w    kno_w)
{
  u2_noun cun, ken;

  if ( u2_no == u2_rl_leap(wir_r, c3__rock) ) {
    c3_assert(0);
  }
  u2_bx_boot(wir_r);
  {
    u2_ray  kit_r = u2_bl_open(wir_r);

    if ( u2_bl_set(wir_r) ) {
      u2_bl_done(wir_r, kit_r);
      u2_rl_fall(wir_r);
      fprintf(stderr, "{no boot, %d}\n", kno_w);
      exit(1);
    }
    else {
      c3_c* pot_c = _eyre_path_ken(kno_w);
      u2_noun src = u2_ux_read(wir_r, pot_c, "watt");

      cun = _eyre_nock(wir_r, u2_yes, src, las);
      // u2_rl_drain(wir_r);
#if 0
      if ( kno_w == 223 ) {
          printf("dump: a\n");
          u2_rl_dump(wir_r);

        u2_wr_gc(wir_r, cun, 0);

          printf("\ndump: b\n");
          u2_rl_dump(wir_r);

        u2_rl_drain(wir_r);

          printf("\ndump: c\n");
          u2_rl_dump(wir_r);

        u2_wr_gc(wir_r, cun, 0);

          printf("\ndump: d\n");
          u2_rl_dump(wir_r);

        u2_rz(wir_r, cun);

          printf("\ndump: e\n");
          u2_rl_dump(wir_r);
        
        u2_wr_gc(wir_r, 0);

          printf("\ndump: f\n");
          u2_rl_dump(wir_r);
        
        exit(1);
      }
#endif
      u2_bl_done(wir_r, kit_r);

      u2_bx_spot(wir_r, u2_nul);
      printf("{soft boot: %s: %x}\n", pot_c, u2_mug(cun));
      free(pot_c);
    }
  }
  ken = u2_rl_take(u2_wire_bas_r(wir_r), cun);
  u2_rl_fall(wir_r);

  {
    c3_c* pot_c = _eyre_path_ken(kno_w);
    u2_noun paq;

    paq = _eyre_jam(wir_r, ken);
    u2_ux_write(wir_r, paq, pot_c, "pile");
    printf("  {%d bits}\n", u2_met(0, paq));

#if 0
    {
      u2_noun foo;

      u2_bx_boot(wir_r);
      foo = _eyre_cue(wir_r, paq);
      u2_rz(wir_r, foo);
    }
#endif 
    u2_rz(wir_r, paq);
    free(pot_c);
  }
  Ken = ken;
  Kno_w = kno_w;

  return ken;
}

/* _eyre_ken(): load kernel by number.
*/
static u2_noun                                                    //  produce
_eyre_ken(u2_wire wir_r,
          c3_w    kno_w)
{
  c3_w    nec_w = EyreFirstKernel;
  u2_noun ken = u2_nul;
  u2_flag nuw = u2_no;

  while ( 1 ) {
    if ( u2_no == nuw ) {
      if ( u2_yes == _eyre_ken_nuw(nec_w) ) {
        nuw = u2_yes;

        if ( EyreFirstKernel == nec_w ) {
          fprintf(stderr, "eyre: %d is not fresh\n", EyreFirstKernel);
          exit(1);
        }
        else {
          u2_noun las = _eyre_ken_load_hard(wir_r, (nec_w + 1));
           
          ken = _eyre_ken_load_soft(wir_r, las, nec_w);
          u2_rz(wir_r, las);
        }
      }
    }
    else {
      u2_noun las = u2_rx(wir_r, ken);

      ken = _eyre_ken_load_soft(wir_r, las, nec_w);
      u2_rz(wir_r, las);
    }

    if ( nec_w == kno_w ) {
      if ( u2_no == nuw ) {
        return _eyre_ken_load_hard(wir_r, kno_w);
      }
      else return ken;
    } else {
      nec_w--;
    }
  }
}

/* _eyre_call_1(): call a text function, with argument `a`.
*/
static u2_noun                                                    //  produce
_eyre_call_1(u2_wire     wir_r,
             u2_flag     rac,                                     //  direct
             u2_noun     ken,                                     //  retain
             const c3_c* src_c,                                   //  retain
             u2_noun     a)                                       //  retain
{
  u2_noun src = u2_bn_string(wir_r, src_c);
  u2_noun noc = _eyre_nock(wir_r, rac, src, ken);
  u2_noun cor = _eyre_nock(wir_r, rac, 0, noc);
  u2_noun pro = _eyre_mong(wir_r, rac, cor, u2_rx(wir_r, a));

  u2_rz(wir_r, cor);
  u2_rz(wir_r, noc);

  return pro;
}

/* _eyre_call_2(): call a text function, with argument `[a b]`.
*/
static u2_noun                                                    //  produce
_eyre_call_2(u2_wire     wir_r,
             u2_flag     rac,                                     //  direct
             u2_noun     ken,                                     //  retain
             const c3_c* src_c,                                   //  retain
             u2_noun     a,                                       //  retain
             u2_noun     b)                                       //  retain
{
  u2_noun src = u2_bn_string(wir_r, src_c);
  u2_noun noc = _eyre_nock(wir_r, rac, src, ken);
  u2_noun cor = _eyre_nock(wir_r, rac, 0, noc);
  u2_noun pro = _eyre_mong(wir_r, rac, cor, u2_bc(wir_r, u2_rx(wir_r, a),
                                                         u2_rx(wir_r, b)));

  u2_rz(wir_r, cor);
  u2_rz(wir_r, noc);

  return pro;
}

/* _eyre_call_3(): call a text function, with argument `[a b c]`.
*/
static u2_noun                                                    //  produce
_eyre_call_3(u2_wire     wir_r,
             u2_flag     rac,                                     //  direct
             u2_noun     ken,                                     //  retain
             const c3_c* src_c,                                   //  retain
             u2_noun     a,                                       //  retain
             u2_noun     b,                                       //  retain
             u2_noun     c)                                       //  retain
{
  u2_noun src = u2_bn_string(wir_r, src_c);
  u2_noun noc = _eyre_nock(wir_r, rac, src, ken);
  u2_noun cor = _eyre_nock(wir_r, rac, 0, noc);
  u2_noun pro = _eyre_mong(wir_r, rac, cor, u2_bt(wir_r, u2_rx(wir_r, a),
                                                         u2_rx(wir_r, b),
                                                         u2_rx(wir_r, c)));

  u2_rz(wir_r, cor);
  u2_rz(wir_r, noc);

  return pro;
}

/* _eyre_columns(): return screen column width from OS.
*/
static c3_l
_eyre_columns(void)
{
  c3_s buf_s[4];
  
  ioctl(0, TIOCGWINSZ, &buf_s);

  return buf_s[1] - 1;
}

/* _eyre_tank_win(): tank to wall.
*/
static u2_noun
_eyre_tank_win(u2_wire wir_r,
               u2_noun ken,                                       //  retain
               c3_l    tab_l,
               u2_noun tec)                                       //  retain
{
  c3_l    edg_l = _eyre_columns();

  return _eyre_call_3
    (wir_r, u2_no,
            ken, 
            "|!([a=@ b=@ c=*tank] (~(win re c) [a b]))", 
            tab_l, edg_l, tec);
}

/* _eyre_bill(): bill to wall.
*/
static u2_noun
_eyre_bill(u2_wire wir_r,
           u2_noun ken,                                           //  retain
           u2_noun bil)                                           //  retain
{
  c3_l    edg_l = _eyre_columns();

  return _eyre_call_2
    (wir_r, u2_no, ken, "|!([a=@ b=*bill] (~(fly to b) a))", edg_l, bil);
}

/* _eyre_print_tape(): print a tape of txt to FIL_f.
*/
static void
_eyre_print_tape(u2_wire     wir_r,                               
                 u2_noun     tep)                                 //  retain
{
  while ( u2_nul != tep ) {
    c3_c car_c;

    if ( u2_h(tep) >= 127 ) {
      car_c = '?';
    } else car_c = u2_h(tep);

    putchar(car_c);
    tep = u2_t(tep);
  }
}

/* _eyre_print_wall(): print a wall of txt.
*/
static void
_eyre_print_wall(u2_wire     wir_r,                               
                 u2_noun     wal)                                 //  retain
{
  while ( u2_nul != wal ) {
    _eyre_print_tape(wir_r, u2_h(wal));
    putchar(10);

    wal = u2_t(wal);
  }
}

/* _eyre_gnaw(): dump a tank to a wall, with tab.
*/
static void
_eyre_gnaw(u2_wire wir_r,
           u2_noun ken,                                           //  retain
           c3_l    tab_l,
           u2_noun tec)                                           //  retain
{
  u2_noun wal;

  wal = _eyre_tank_win(wir_r, ken, tab_l, tec);
  _eyre_print_wall(wir_r, wal);

  u2_rz(wir_r, wal);
}

/* _eyre_dirt(): print an arbitrary pile as a wall.  Works <= 263.
*/
static void
_eyre_dirt(u2_wire wir_r, 
           u2_noun ken,                                           //  retain
           c3_l    tab_l,                                         //  retain
           u2_noun som)                                           //  retain
{
  u2_noun poq = u2_bc(wir_r, 'q', u2_rx(wir_r, som));
  u2_noun tec = _eyre_call_1(wir_r, u2_no, ken, "=>(!% show)", poq);

  _eyre_gnaw(wir_r, ken, tab_l, tec);

  u2_rz(wir_r, tec);
  u2_rz(wir_r, poq);
}
                  
/* _eyre_dump(): dump a bill to a wall.
*/
static void
_eyre_dump(u2_wire wir_r,
           u2_noun ken,                                           //  retain
           u2_noun bil)                                           //  retain
{
  u2_noun wal;

  wal = _eyre_bill(wir_r, ken, bil);
  _eyre_print_wall(wir_r, wal);

  u2_rz(wir_r, wal);
}

/* _eyre_print_spot(): print wrapper for file location, at tab of 2.
*/
static void
_eyre_print_spot(u2_wire wir_r,
                 u2_noun ken,                                     //  retain
                 u2_noun sot)                                     //  retain
{
  u2_noun p_sot, q_sot, r_sot, pq_sot, qq_sot, pr_sot, qr_sot;

  if ( (u2_yes == u2_as_trel(sot, &p_sot, &q_sot, &r_sot)) &&
       (u2_yes == u2_as_cell(q_sot, &pq_sot, &qq_sot)) &&
       (u2_yes == u2_as_cell(r_sot, &pr_sot, &qr_sot)) &&
       (u2_yes == u2_stud(pq_sot)) &&
       (u2_yes == u2_stud(qq_sot)) &&
       (u2_yes == u2_stud(pr_sot)) &&
       (u2_yes == u2_stud(qr_sot)) )
  {
    printf("{%d.%d:%d.%d}\n", pq_sot, qq_sot, pr_sot, qr_sot);
  }
  else {
    printf("  {spot!}\n");
    u2_err(wir_r, "{{invalid}}", sot);
  }
}

/* _eyre_print_bean(): print wrapper for flat meaning.
*/
static void
_eyre_print_bean(u2_wire wir_r,
                 u2_noun ken,                                     //  retain
                 u2_noun ben)                                     //  retain
{
  if ( Kno_w == 264 ) {
    _eyre_dump(wir_r, ken, ben);
  }
  else {
    _eyre_dirt(wir_r, ken, 2, ben);
  }
}

/* _eyre_print_mean(): print wrapper for meta-meaning.
*/
static void
_eyre_print_mean(u2_wire wir_r,
                 u2_noun ken,                                     //  retain
                 u2_noun mon)                                     //  retain
{
  if ( u2_yes == u2_dust(mon) ) {
    u2_noun som = u2_nk_nock(wir_r, u2_rx(wir_r, mon), u2_t(mon));

    if ( u2_none != som ) {
      _eyre_gnaw(wir_r, ken, 2, som);
      u2_rz(wir_r, som);
      return;
    }
    else printf("  {meaning failed!}\n");
  }
}

/* _eyre_print_tent(): print wrapper for trace entry.
*/
static void
_eyre_print_tent(u2_wire wir_r,
                 u2_noun ken,
                 u2_noun tax)                                     //  retain
{
  if ( u2_yes == u2_dust(tax) ) switch ( u2_h(tax) ) {
    case c3__spot: _eyre_print_spot(wir_r, ken, u2_t(tax)); return;
    case c3__bean: _eyre_print_bean(wir_r, ken, u2_t(tax)); return;
                   // u2_err(wir_r, "bean", u2_t(tax)); return;
    case c3__mean: _eyre_print_mean(wir_r, ken, u2_t(tax)); return;
  }
  // u2_err(wir_r, "htax", u2_h(tax));
  printf("  {tent!}\n");
}

/* _eyre_print_trac(): print wrapper for trace stack.
*/
static void
_eyre_print_trac(u2_wire wir_r,
                 u2_noun ken,
                 u2_noun tax)                                     //  retain
{
  while ( u2_nul != tax ) {
    _eyre_print_tent(wir_r, ken, u2_h(tax));
    tax = u2_t(tax);
  }
}

#if 0
/* _eyre_test2(): advanced test.
*/
static void
_eyre_test2(u2_wire wir_r, 
            u2_noun ken,
            c3_c*   lid_c)
{
  u2_ray kit_r = u2_bl_open(wir_r);

  if ( u2_bl_set(wir_r) ) {
    u2_bl_done(wir_r, kit_r);
    fprintf(stderr, "{exit}\n");
  } else {
    c3_c*   pot_c = _eyre_path_int(lid_c);
    u2_noun src   = u2_ux_read(wir_r, pot_c, "watt");
    u2_noun noc, cor, bum, goo;

    printf("compiling...\n");
    noc = _eyre_nock(wir_r, src, ken);
    printf("composing...\n");
    cor = _eyre_nock(wir_r, 0, noc);
    printf("ready...\n");

    bum = u2_bn_hook(wir_r, cor, "bump");
    printf("bumped...\n");

    goo = u2_bn_mong(wir_r, bum, 13);
    u2_err(wir_r, "goo", goo);

    u2_bl_done(wir_r, kit_r);
  }
}
#endif

  static u2_noun _gunn_fuel(u2_wire, u2_noun, u2_noun);
  static u2_noun _gunn_tool(u2_wire, u2_noun, u2_noun);

/* _eyre_gunn(): execute and print a line.  Produce new core.
*/
  /* _gunn_tape_len(): tape length.
  */
  static c3_w
  _gunn_tape_len(u2_noun tap)
  {
    return (u2_no == u2_dust(tap)) ? 0 : 1 + _gunn_tape_len(u2_t(tap));
  }
 
  /* _gunn_tape_nab(): tape copy.
  */
  static void
  _gunn_tape_nab(c3_c*   str_c,                                   //  retain
                 u2_noun tap)                                     //  retain
  {
    while ( u2_no != u2_dust(tap) ) {
      *str_c++ = u2_byte(0, u2_h(tap));
      tap = u2_t(tap);
    }
  }

  /* _gunn_unix_real(): yes iff file is real.
  */
  static u2_flag
  _gunn_unix_real(const c3_c* fil_c)                              //  retain
  {
    struct stat buf_s;

    return ( (0 == stat(fil_c, &buf_s)) ? u2_yes : u2_no );
  }

  /* _gunn_unix_rend(): render filename.
  */
  static c3_c*                                                    //  produce
  _gunn_unix_rend(u2_wire     wir_r,
                  const c3_c* bas_c,                              //  retain
                  const c3_c* ext_c,                              //  retain
                  u2_noun     tap,                                //  retain
                  u2_noun     med)                                //  retain
  {
    c3_w len = strlen(bas_c) + 
               strlen(ext_c) + 1 +
               _gunn_tape_len(tap) + 1 +
               u2_met(3, med);
    c3_c *str_c = malloc(len + 1);
    c3_c *rat_c;

    if ( 0 == str_c ) { u2_bl_bail(wir_r, c3__fail); return 0; }
    rat_c = str_c;

    strcpy(rat_c, bas_c);
    rat_c += strlen(bas_c);

    strcpy(rat_c, ext_c);
    rat_c += strlen(ext_c);
    *rat_c++ = '/';

    _gunn_tape_nab(rat_c, tap);
    rat_c += _gunn_tape_len(tap);

    if ( u2_nul != med ) {
      *rat_c++ = '.';

      u2_bytes(0, u2_met(3, med), (c3_y*)rat_c, med);
      rat_c += u2_met(3, med);
      *rat_c++ = 0;
    }
    return str_c;
  }

  /* _gunn_env_sysd(): return eyre system directory.
  */
  static c3_c*                                                    //  produce
  _gunn_env_sysd(void)
  {
    c3_c* env_c = getenv("EYRE_SYSDIR");

    if ( env_c ) { return strdup(env_c); }
    else return 0;
  }

  /* _gunn_env_myrd(): return eyre home directory.
  */
  static c3_c*                                                    //  produce
  _gunn_env_myrd(void)
  {
    c3_c* env_c = getenv("EYRE_HOMEDIR");

    if ( env_c ) { return strdup(env_c); }
    else {
      c3_c max_c[FILENAME_MAX+1];

      if ( !(env_c = getenv("HOME")) ) { return 0; }
      strcpy(max_c, env_c);
      strcat(max_c, "/eyre");

      return strdup(max_c);
    }
  }

  /* _gunn_env_name(): return named env variable.
  */
  static c3_c*                                                    //  produce
  _gunn_env_name(const c3_c *nam_c)
  {
    c3_c* env_c = getenv(nam_c);

    if ( env_c ) { return strdup(env_c); }
    else return 0;
  }

  /* _gunn_env_rund(): return eyre runtime directory.
  */
  static c3_c*                                                    //  produce
  _gunn_env_rund(void)
  {
    c3_assert(!"rund"); return 0;
  }

  /* _gunn_env_curd(): return current directory.
  */
  static c3_c*                                                    //  produce
  _gunn_env_curd(void)
  {
    c3_c max_c[FILENAME_MAX+1];

    getcwd(max_c, FILENAME_MAX);
    return strdup(max_c);
  }

  /* _gunn_unix_sys(): prep for system spot with extension.
  */
  static c3_c*                                                    //  produce
  _gunn_unix_sys(u2_wire wir_r,
                 u2_noun med,                                     //  retain
                 c3_c*   ext_c,                                   //  retain
                 u2_noun tap)                                     //  retain
  {
    c3_c* ret_c;
    c3_c* var_c;

    if ( 0 != (var_c = _gunn_env_sysd()) ) {
      ret_c = _gunn_unix_rend(wir_r, var_c, ext_c, tap, med);
      free(var_c);
      if ( u2_yes == _gunn_unix_real(ret_c) ) {
        return ret_c;
      } else free(ret_c);
    }
    if ( 0 != (var_c = _gunn_env_myrd()) ) {
      ret_c = _gunn_unix_rend(wir_r, var_c, ext_c, tap, med);
      free(var_c);
      if ( u2_yes == _gunn_unix_real(ret_c) ) {
        return ret_c;
      } else {
        perror(ret_c);
        free(ret_c);
        u2_bl_bail(wir_r, c3__fail); return 0;
      }
    }
    u2_bl_bail(wir_r, c3__fail); return 0;
  }

  /* _gunn_unix_run: prep for run spot with extension.
  */
  static c3_c*                                                    //  produce
  _gunn_unix_run(u2_wire wir_r,
                 u2_noun med,                                     //  retain
                 c3_c*   ext_c,                                   //  retain
                 u2_noun tap)                                     //  retain
  {
    c3_c* ret_c;
    c3_c* var_c;

    if ( 0 != (var_c = _gunn_env_rund()) ) {
      ret_c = _gunn_unix_rend(wir_r, var_c, ext_c, tap, med);
      free(var_c);
      return ret_c;
    }
    u2_bl_bail(wir_r, c3__fail); return 0;
  }

  /* _gunn_unix_prep(): map mode, spot, tape to a unix filename, or 0.
  */
  static c3_c*                                                    //  produce
  _gunn_unix_prep(u2_wire wir_r,
                  u2_noun med,                                    //  retain
                  u2_noun sot,                                    //  retain
                  u2_noun tap)                                    //  retain
  {
    c3_c* var_c;
    c3_c* ret_c;

    switch ( sot ) {
      default: break;

      case c3__actd: {
        return _gunn_unix_run(wir_r, med, "/act", tap);
      }
      case c3__boot: {
        return _gunn_unix_sys(wir_r, med, "/boot", tap);
      }
      case c3__curd: {
        if ( 0 != (var_c = _gunn_env_curd()) ) {
          ret_c = _gunn_unix_rend(wir_r, var_c, "", tap, med);
          free(var_c);
          return ret_c;
        }
        break;
      }
      case c3__fund: {
        return _gunn_unix_sys(wir_r, med, "/fun", tap);
      }
      case c3__home: {
        if ( 0 != (var_c = _gunn_env_name("HOME")) ) {
          ret_c = _gunn_unix_rend(wir_r, var_c, "", tap, med);
          free(var_c);
          return ret_c;
        }
        break;
      }
      case c3__jetd: {
        return _gunn_unix_sys(wir_r, med, "/jet", tap);
      }
      case c3__libd: {
        return _gunn_unix_sys(wir_r, med, "/lib", tap);
      }
      case c3__netd: {
        return _gunn_unix_run(wir_r, med, "/net", tap);
      }
      case c3__outd: {
        return _gunn_unix_run(wir_r, med, "/out", tap);
      }
      case c3__resd: {
        return _gunn_unix_sys(wir_r, med, "/res", tap);
      }
    }
     u2_err(wir_r, "spot", sot); 
     u2_bl_bail(wir_r, c3__fail);
     return 0;
  }

  /* _gunn_save_mode(): save to disk by mode.
  */
  static void
  _gunn_save_mode(u2_wire wir_r,
                  u2_noun cor,                                    //  retain
                  u2_noun med,                                    //  retain
                  u2_noun sot,                                    //  retain
                  u2_noun tap,                                    //  retain
                  u2_noun vax)                                    //  retain
  {
    c3_c *fil_c = _gunn_unix_prep(wir_r, med, sot, tap);

    switch ( med ) {
      default: u2_err(wir_r, "not ready", med); 
               u2_bl_bail(wir_r, c3__fail); return;

      case c3__atom: 
      case c3__watt: {
        u2_noun dat = u2_t(vax);

        if ( u2_no == u2_stud(dat) ) {
          u2_err(wir_r, "not atomic", u2_t(vax));
          u2_bl_bail(wir_r, c3__fail); return;
        }
        if ( u2_no == u2_ux_write(wir_r, dat, fil_c, 0) ) {
          perror(fil_c);
        }
        free(fil_c);
        break;
      }
      case c3__pile: {
        u2_noun pil = u2_rx(wir_r, u2_t(vax));
        u2_noun dat = _eyre_hook(wir_r, cor, "jam", pil);

        if ( u2_no == u2_ux_write(wir_r, dat, fil_c, 0) ) {
          perror(fil_c);
        }
        free(fil_c);

        u2_rz(wir_r, dat);
        break;
      }
    }
  }

  /* _gunn_save(): save to disk, or try to.
  */
  static void
  _gunn_save(u2_wire wir_r,
             u2_noun cor,                                         //  retain
             u2_noun myn,                                         //  retain
             u2_noun vax)                                         //  retain
  {
    u2_noun med, sot, pat;
    u2_noun rax, p_rax, q_rax;

    u2_bi_trel(wir_r, myn, &med, &sot, &pat);
    rax = _eyre_hook_cell(wir_r, cor, "hunt", u2_rx(wir_r, med),
                                              u2_rx(wir_r, pat));
    p_rax = u2_xh(wir_r, rax);
    q_rax = u2_xt(wir_r, rax);
    _gunn_save_mode(wir_r, cor, p_rax, sot, q_rax, vax);

    u2_rz(wir_r, rax);
  }

  /* _gunn_load_mode(): load by mode, spot and tape.
  */
  static u2_noun                                                  //  produce
  _gunn_load_mode(u2_wire wir_r,
                  u2_noun cor,                                    //  retain
                  u2_noun med,                                    //  retain
                  u2_noun sot,                                    //  retain
                  u2_noun tap)                                    //  retain
  {
    c3_c *fil_c = _gunn_unix_prep(wir_r, med, sot, tap);

    switch ( med ) {
      default: u2_err(wir_r, "not ready", med); 
               return u2_bl_bail(wir_r, c3__fail);

      case c3__atom: 
      case c3__watt: {
        u2_noun dat = u2_ux_read(wir_r, fil_c, 0);

        if ( u2_none == dat ) {
          perror(fil_c);
          return u2_bl_bail(wir_r, c3__fail);
        }
        else return dat;
      }
      case c3__pile: {
        u2_noun dat = u2_ux_read(wir_r, fil_c, 0);

        if ( u2_none == dat ) {
          perror(fil_c);
          return u2_bl_bail(wir_r, c3__fail);
        }
        else {
          return _eyre_hook(wir_r, cor, "cue", dat);
        }
      }
    }
  }

  /* _gunn_load(): load from disk, producing [(mode) *]
  */
  static u2_noun                                                  //  produce
  _gunn_load(u2_wire wir_r,
             u2_noun cor,                                         //  retain
             u2_noun myn)                                         //  retain
  {
    u2_noun med, sot, pat;
    u2_noun rax, p_rax, q_rax;
    u2_noun rut;

    u2_bi_trel(wir_r, myn, &med, &sot, &pat);
    rax = _eyre_hook_cell(wir_r, cor, "hunt", u2_rx(wir_r, med),
                                              u2_rx(wir_r, pat));
    p_rax = u2_xh(wir_r, rax);
    q_rax = u2_xt(wir_r, rax);
    rut = _gunn_load_mode(wir_r, cor, p_rax, sot, q_rax);

    u2_rz(wir_r, rax);
    return u2_bc(wir_r, u2_rx(wir_r, med), rut);
  }

  /* _gunn_fuel_list(): iterate through a fuel list.
  */
  static u2_noun                                                  //  produce
  _gunn_fuel_list(u2_wire wir_r,
                  u2_noun cor,                                    //  retain
                  u2_noun l_ful)                                  //  retain
  {
    if ( u2_nul == l_ful ) {
      return l_ful;
    } else return u2_bc
      (wir_r, _gunn_fuel(wir_r, cor, u2_h(l_ful)),
              _gunn_fuel_list(wir_r, cor, u2_t(l_ful)));
  }

  /* _gunn_fuel(): load disk objects in `ful`.
  */
  static u2_noun                                                  //  produce
  _gunn_fuel(u2_wire wir_r,
             u2_noun cor,                                         //  retain
             u2_noun ful)                                         //  submit
  {
    u2_noun p_ful, q_ful, ret;

    if ( u2_nul == ful ) {
      return u2_nul;
    } else switch ( u2_h(ful) ) {
      default: {
        u2_err(wir_r, "fuel", ful);
        ret = u2_nul; 
        break;
      }

      case c3__chew: u2_bi_cell(wir_r, u2_t(ful), &p_ful, &q_ful);
      {
        ret = u2_bt(wir_r, c3__chew, u2_rx(wir_r, p_ful), 
                                     _gunn_fuel_list(wir_r, cor, q_ful));
        break;
      }
      case c3__data: return ful;
      case c3__disk: p_ful = u2_t(ful);
      {
        ret = u2_bc(wir_r, c3__data, _gunn_load(wir_r, cor, p_ful));
        break;
      }
      case c3__gene: return ful;
      case c3__many: p_ful = u2_t(ful);
      {
        ret = u2_bc(wir_r, c3__many, _gunn_fuel_list(wir_r, cor, p_ful));
        break;
      }
      case c3__name: u2_bi_cell(wir_r, u2_t(ful), &p_ful, &q_ful);
      {
        ret = u2_bt(wir_r, c3__name, u2_rx(wir_r, p_ful),
                                     _gunn_fuel(wir_r, cor, q_ful));
        break;
      }
    }
    u2_rz(wir_r, ful);
    return ret;
  }

  /* _gunn_tool_fun(): load function in tool.
  */
  static u2_noun                                                  //  produce
  _gunn_tool_fun(u2_wire wir_r,
                 u2_noun cor,                                     //  retain
                 u2_noun fun)                                     //  retain
  {
    if ( c3__disk == u2_h(fun) ) {
      u2_noun t_fun = u2_t(fun);
      u2_noun myn = u2_bt(wir_r, c3__watt, c3__fund, u2_rx(wir_r, t_fun));
      u2_noun lod = _gunn_load(wir_r, cor, myn);
      u2_noun ret = u2_bc(wir_r, c3__data, u2_rx(wir_r, u2_t(lod)));

      u2_rz(wir_r, lod);
      u2_rz(wir_r, myn);
      return ret;
    }
    else return u2_rx(wir_r, fun);
  }

  /* _gunn_tool_lib(): load libraries in tool.
  */
  static u2_noun                                                  //  produce
  _gunn_tool_lib(u2_wire wir_r,
             u2_noun cor,                                     //  retain
             u2_noun lad)                                     //  retain
  {
    if ( u2_no == u2_dust(lad) ) {
      return u2_nul;
    } else {
      u2_noun i_lad = u2_h(lad);
      u2_noun t_lad = u2_t(lad);
      u2_noun pi_lad;

      switch ( u2_h(i_lad) ) {
        default: return u2_bl_bail(wir_r, c3__fail);

        case c3__data: {
          return u2_bc(wir_r, u2_rx(wir_r, i_lad),
                                _gunn_tool_lib(wir_r, cor, t_lad));
        }
        case c3__disk: pi_lad = u2_t(i_lad);
        {
          u2_noun myn = u2_bt(wir_r, c3__watt, c3__libd, u2_rx(wir_r, pi_lad));
          u2_noun lod = _gunn_load(wir_r, cor, myn);
          u2_noun ret = u2_bc(wir_r, u2_bc(wir_r, c3__data, 
                                                  u2_rx(wir_r, u2_t(lod))),
                                     _gunn_tool_lib(wir_r, cor, t_lad));

          u2_rz(wir_r, lod);
          u2_rz(wir_r, myn);
          return ret;
        }
      }
    }
  }

  /* _gunn_tool(): load disk objects in `tul`.
  */
  static u2_noun                                                  //  produce
  _gunn_tool(u2_wire wir_r,
             u2_noun cor,                                         //  retain
             u2_noun tul)                                         //  submit
  {
    u2_noun lad = u2_xh(wir_r, tul);
    u2_noun fun = u2_xt(wir_r, tul);
    u2_noun ret = u2_bc(wir_r, _gunn_tool_lib(wir_r, cor, lad),
                               _gunn_tool_fun(wir_r, cor, fun));

    u2_rz(wir_r, tul);
    return ret;
  }

  /* _gunn_show_tank(): display tank at tab.
  */
  static void
  _gunn_show_tank(u2_wire wir_r,
                  u2_noun cor,                                    //  retain
                  c3_l    tab_l,
                  u2_noun tac)                                    //  submit
  {
    c3_l    col_l = _eyre_columns();
    u2_noun wol = _eyre_hook_cell
      (wir_r, cor, "wash", u2_bc(wir_r, tab_l, col_l), tac);

    _eyre_print_wall(wir_r, wol);
    u2_rz(wir_r, wol);
  }

  /* _gunn_show_slab(): display slab.
  */
  static void
  _gunn_show_slab(u2_wire wir_r,
                  u2_noun cor,                                    //  retain
                  u2_noun sab)                                    //  submit
  {
    if ( u2_nul == sab ) {
      return;
    } else {
      u2_noun p_sab, q_sab, r_sab;

      u2_bi_trel(wir_r, sab, &p_sab, &q_sab, &r_sab);
      if ( u2_nul != p_sab ) {
        _gunn_show_tank
          (wir_r, cor, 0, _eyre_hook(wir_r, cor, "swat", u2_rx(wir_r, p_sab)));
      }
      if ( u2_nul != q_sab ) {
        _gunn_show_tank
          (wir_r, cor, 2, _eyre_hook(wir_r, cor, "swig", u2_rx(wir_r, q_sab)));
      }
      if ( u2_nul != r_sab ) {
        _gunn_show_tank
          (wir_r, cor, 2, _eyre_hook(wir_r, cor, "swim", u2_rx(wir_r, r_sab)));
      }
      u2_rz(wir_r, sab);
    }
  }

  /* _gunn_show(): display result with optional label.
  */
  static void
  _gunn_show(u2_wire wir_r,
             u2_noun cor,                                         //  retain
             u2_noun lab,                                         //  retain
             u2_noun vax)                                         //  submit
  {
    c3_l    tab_l = 0;
    u2_noun tac;

    if ( u2_blip != lab ) {
      c3_w  len_w = u2_met(3, lab);
      c3_y* txt_y = alloca(3+len_w);

      u2_bytes(0, len_w, txt_y, lab);
      txt_y[len_w] = ':';
      txt_y[1+len_w] = '\n';
      txt_y[2+len_w] = 0;

      puts((char *)txt_y);
      tab_l += 2;
    }
#if 1
    {
      tac = _eyre_hook(wir_r, cor, "soul", u2_rx(wir_r, u2_xh(wir_r, vax)));
      tac = _eyre_hook_cell(wir_r, cor, "whip", c3__type, tac);

      _gunn_show_tank(wir_r, cor, tab_l + 2, tac);
    }
#endif
    {
      tac = _eyre_hook(wir_r, cor, "sell", vax);

      _gunn_show_tank(wir_r, cor, tab_l, tac);
    }
  }

  /* _gunn_vent(): apply vent.  Produce new core.
  */
  static u2_noun                                                  //  produce
  _gunn_vent(u2_wire wir_r,
             u2_noun cor,                                         //  submit
             u2_noun vet,                                         //  submit
             u2_noun vax)                                         //  submit
  {
    u2_noun p_vet;

    if ( u2_no == u2_dust(vet) ) {
      _gunn_show(wir_r, cor, vet, vax);
      return cor;
    }
    else switch ( (p_vet = u2_xh(wir_r, vet)) ) {
      default: {
        u2_err(wir_r, "vent", vet);
        u2_rz(wir_r, vax);
        return cor;
      }

      case c3__bind: p_vet = u2_xt(wir_r, vet);
      {
        u2_noun bid = _eyre_hook_cell
                        (wir_r, cor, "bind", u2_rx(wir_r, p_vet), vax);

        u2_rz(wir_r, cor);
        return bid;
      }
      case c3__disk: p_vet = u2_xt(wir_r, vet);
      {
        _gunn_save(wir_r, cor, u2_rx(wir_r, p_vet), vax);

        u2_rz(wir_r, vax);
        return cor;
      }
      case c3__many: p_vet = u2_xt(wir_r, vet);
      {
        u2_noun tp_vet;

        if ( u2_nul == p_vet ) {
          return u2_rx(wir_r, cor);
        }
        else if ( u2_nul == (tp_vet = u2_xt(wir_r, p_vet)) ) {
          return _gunn_vent(wir_r, cor, u2_xh(wir_r, p_vet), vax);
        }
        else {
          u2_noun ip_vet = u2_xh(wir_r, p_vet);
          u2_noun nex = u2_bc(wir_r, c3__many, u2_rx(wir_r, tp_vet));
          u2_noun hed, tal, roc;

          hed = _eyre_hook_cell(wir_r, cor, "slot", 2, u2_rx(wir_r, vax));
          tal = _eyre_hook_cell(wir_r, cor, "slot", 3, u2_rx(wir_r, vax));

          roc = _gunn_vent(wir_r, cor, ip_vet, hed);
          cor = _gunn_vent(wir_r, roc, nex, tal);

          u2_rz(wir_r, nex);

          u2_rz(wir_r, vax);
          return cor;
        }
      }
    }
  }

/* _eyre_gunn(): execute and print a line.  Produce new core.
*/
static u2_noun                                                    //  produce
_eyre_gunn(u2_wire wir_r,
           u2_noun cor,                                           //  submit
           u2_noun txt)                                           //  submit
{
  u2_noun dyd = _eyre_hook(wir_r, cor, "scan", txt);
  u2_noun vet, ful, tul;

  u2_bi_trel(wir_r, dyd, &vet, &ful, &tul);
#if 0
  u2_err(wir_r, "tool", tul);
  u2_err(wir_r, "fuel", ful);
  u2_err(wir_r, "vent", vet);
#endif
  ful = _gunn_fuel(wir_r, cor, u2_rx(wir_r, ful));
  tul = _gunn_tool(wir_r, cor, u2_rx(wir_r, tul)); 
  {
    u2_noun von, sab;
  
    // u2_tx_do_profile(wir_r, u2_yes);
    u2_tx_open(wir_r);
    von = _eyre_hook_cell(wir_r, cor, "ride", tul, ful);
    sab = u2_tx_done(wir_r);
    // u2_tx_do_profile(wir_r, u2_no);

    if ( u2_nul != sab ) {
      _gunn_show_slab(wir_r, cor, sab);
    }
    return _gunn_vent(wir_r, cor, vet, von);
  }
}

#ifdef PROBE
/* _eyre_probe():
**
**   Load `kno` from source, using previous `las`; debug with previous `app`.
*/
static u2_noun                                                    //  produce
_eyre_probe(u2_wire wir_r,
            u2_noun ken,                                          //  retain
            u2_noun app,                                          //  retain
            c3_w    kno_w)
{
  u2_noun cun, nex;

  if ( u2_no == u2_rl_leap(wir_r, c3__rock) ) {
    c3_assert(0);
  }
  {
    u2_ray  kit_r = u2_bl_open(wir_r);

    if ( u2_bl_set(wir_r) ) {
      u2_bl_done(wir_r, kit_r);
      u2_rl_fall(wir_r);
      fprintf(stderr, "{no boot, %d}\n", kno_w);
      exit(1);
    }
    else {
      c3_c* pot_c = _eyre_path_ken(kno_w);
      u2_noun src = u2_ux_read(wir_r, pot_c, "watt");
      u2_noun sab;

      printf("{probe booting: %s}\n", pot_c);

      // u2_tx_do_profile(wir_r, u2_yes);
      u2_tx_open(wir_r);
      cun = _eyre_nock(wir_r, u2_yes, src, ken);
      sab = u2_tx_done(wir_r);
      // u2_tx_do_profile(wir_r, u2_no);

      if ( u2_nul != sab ) {
        _gunn_show_slab(wir_r, app, sab);
      }
      u2_bl_done(wir_r, kit_r);

      printf("{probe boot: %s: %x}\n", pot_c, u2_mug(cun));
      free(pot_c);
    }
  }
  nex = u2_rl_take(u2_wire_bas_r(wir_r), cun);
  u2_rl_fall(wir_r);

  return nex;
}
#endif

/* _eyre_app(): load generic application core (assumes gunn).
*/
static u2_noun                                                    //  produce
_eyre_app(u2_wire wir_r, 
          u2_noun ken,                                            //  retain
          c3_c*   lid_c)                                          //  retain
{
  u2_ray kit_r = u2_bl_open(wir_r);

  if ( u2_bl_set(wir_r) ) {
    u2_bl_done(wir_r, kit_r);
    fprintf(stderr, "{app: exit}\n");
    exit(1);
    return 0;
  } else {
    c3_c*   pot_c = _eyre_path_int(lid_c);
    u2_noun src   = u2_ux_read(wir_r, pot_c, "watt");
    u2_noun fom, app, sab;

    printf("app: loading: %s\n", pot_c);

#ifdef PERF
    u2_bx_boot(wir_r);
    // u2_tx_do_profile(wir_r, u2_yes);
    {
      u2_noun gen;

      u2_tx_open(wir_r);
      gen = _eyre_call_1(wir_r, u2_yes, ken, "ream:!%", src);
      u2_rz(wir_r, src);
      sab = u2_tx_done(wir_r);
      // u2_tx_do_profile(wir_r, u2_no);

      fom = _eyre_call_1
        (wir_r, u2_yes, ken, "|!(a=*gene q:(~(mint ut %noun) %noun a))", gen);
    }
#else
    fom = _eyre_nock(wir_r, u2_yes, src, ken);
    sab = u2_nul;
#endif
    app = _eyre_nock(wir_r, u2_yes, 0, fom);

    u2_rz(wir_r, fom);
    printf("app: %s, %x\n", pot_c, u2_mug(app));
    free(pot_c);

    if ( u2_nul != sab ) {
      _gunn_show_slab(wir_r, app, sab);
    }
    return app;
  }
}

/* _eyre_line(): execute and print a line, producing new core.
*/
static u2_noun                                                    //  produce
_eyre_line(u2_wire wir_r,
           u2_noun ken,                                           //  retain
           u2_noun cor,                                           //  submit
           u2_noun txt)                                           //  submit
{
  u2_ray kit_r = u2_bl_open(wir_r);

  if ( u2_bl_set(wir_r) ) {
    u2_bl_done(wir_r, kit_r);
    fprintf(stderr, "{lose}\n");

    return cor;
  } else {
    cor = _eyre_gunn(wir_r, cor, txt);

    u2_bl_done(wir_r, kit_r);
    return cor;
  }
}

/* _eyre_line_proto(): parse line with old kernel, execute with new.
*/
static void
_eyre_line_proto(u2_wire wir_r, 
                 u2_noun las,                                     //  retain
                 u2_noun ken,                                     //  retain
                 u2_noun txt)                                     //  retain
{
  u2_ray kit_r = u2_bl_open(wir_r);

  if ( u2_bl_set(wir_r) ) {
    u2_bl_done(wir_r, kit_r);
    fprintf(stderr, "{lose}\n");
  } else {
    u2_noun fol = _eyre_call_1(wir_r, u2_yes, las, "=>(!% make)", txt);
    u2_noun som = _eyre_nock(wir_r, u2_yes, 0, fol);
    u2_noun pro;
   
    u2_bx_boot(wir_r);
    // u2_err(wir_r, "som", som);
    // u2_err(wir_r, "ken", ken);
    pro = _eyre_nock(wir_r, u2_yes, som, ken);
    u2_err(wir_r, "pro", pro);

    // _eyre_dirt(wir_r, las, 0, pro); 
    u2_rz(wir_r, fol);
    u2_rz(wir_r, pro);

    u2_bl_done(wir_r, kit_r);
  }
}


/* main()::
*/
c3_i
main(c3_i   argc,
     c3_c** argv)
{
  u2_wire wir_r;
  c3_w    kno_w;
  c3_c*   fel_c;
  c3_c*   lid_c = 0;
  u2_noun ken;
  u2_noun las = 0;
  u2_noun app = 0;

  //  Parse arguments.
  //
  if ( argc == 1 ) {
    EyreSmoke = u2_no;
    kno_w = EyreFirstKernel;
#ifdef GUNN
    lid_c = strdup("gunn");
#else
    lid_c = strdup("hume");
#endif

    goto proceed;
  }
  if ( (0 == (kno_w = strtol(argv[1], 0, 10))) || (kno_w > EyreFirstKernel) ) {
    goto usage;
  }
  if ( 2 == argc ) {
#ifdef GUNN
    EyreSmoke = u2_no;
    lid_c = strdup("gunn");
#else
    EyreSmoke = u2_yes;
#endif
    if ( kno_w > (EyreFirstKernel - 1) ) {
      goto usage;
    }
    printf("{smoke test %d}\n", kno_w);
    goto proceed;
  }
  else if ( 3 == argc ) {
    if ( !(lid_c = strdup(argv[2])) ) {
      goto usage;
    }
    EyreSmoke = u2_no;
    goto proceed;
  }

  usage: {
    fprintf(stderr, "usage: eyre <$kernel> [$shell]\n");
    fprintf(stderr, "  watt/$kernel.watt\n");
    fprintf(stderr, "    (FirstKernel is %d, kernels count down)\n", 
            EyreFirstKernel);
    fprintf(stderr, "  watt/$shell.watt\n");

    exit(1);
  } proceed:

  //  Instantiate system utilities.
  {
    u2_boot();
    wir_r = u2_wr_init(c3__rock, u2_ray_of(0, 0), u2_ray_of(1, 0));
    fel_c = c3_comd_init();
  }


  //  Load the kernel(s) and/or shell.
  //
  {
    if ( u2_yes == EyreSmoke ) {
      las = _eyre_ken(wir_r, (kno_w + 1));

      Ken = las;
      Kno_w = (kno_w + 1);

      ken = _eyre_ken_load_soft(wir_r, las, kno_w);
      //  u2_err(wir_r, "ken", ken);
    }
    else {
      ken = _eyre_ken(wir_r, kno_w);
      app = _eyre_app(wir_r, ken, lid_c);

#ifdef PROBE
      {
        u2_noun one;
#ifdef DPROBE
        u2_noun two;
#endif
        one = _eyre_probe(wir_r, ken, app, (kno_w - 1));
#ifdef DPROBE
        two = _eyre_probe(wir_r, one, app, (kno_w - 2));
#endif
        exit(0);
      }
#endif
    }
  }

  //  Do some lines.
  //
  while ( 1 ) {
    c3_c* lin_c = c3_comd_line(fel_c);

    if ( !lin_c ) {
      break;
    }
    else {
      u2_noun lin = u2_bn_string(wir_r, lin_c);

      if ( u2_yes == EyreSmoke ) {
        _eyre_line_proto(wir_r, las, ken, lin);
      } else {
        app = _eyre_line(wir_r, ken, app, lin);
      }
      free(lin_c);
    }
  }
  return 0;
}
