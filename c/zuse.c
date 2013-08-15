/* c/zuse.c
**
** This file is in the public domain.
*/
#include <sys/stat.h>
#include <fcntl.h>

#define U2_GLOBAL
#define C3_GLOBAL
#include "all.h"

  /**   Interpreter data structures.
  **/
    /* struct zuse_state: zuse application state.
    */
      struct zuse_state {
        /*  wir - the execution wire.
        */
        u2_ray wir_r;

        /*  pyt - type of pit.
        */
        u2_noun pyt;

        /*  kol - kernel formula.
        */
        u2_noun kol;

        /*  pit - the innermost core.
        */
        u2_noun pit;

        /*  bot - the pair [pyt pit].
        */
        u2_noun bot;

        /*  ryd - a read gate.
        */
        u2_noun ryd;
      };


  /**   Public functions.
  **/
    /* zuse_boot(): create the zuse engine.
    */
      struct zuse_state*
      zuse_boot(const c3_c* src_c);

    /* zuse_line(): execute a zuse command.
    */
      void
      zuse_line(struct zuse_state* fod_f,
                const c3_c*        cmd_c,
                const c3_c*        arg_c);

    /* zuse_done(): terminate and free all.
    */
      void
      zuse_done(struct zuse_state* fod_f);
    

/* _zuse_save_rock(): save engine as noun.
*/
static void
_zuse_save_rock(struct zuse_state* fod_f,
                const c3_c*        src_c)
{
  u2_wire wir_r = fod_f->wir_r;
  u2_noun fat = u2_bn_cell(wir_r, fod_f->pyt, fod_f->kol);

  u2_ux_write_deep(wir_r, fat, src_c, "noun");
}

/* _zuse_gates(): add various convenience gates.
*/
static void 
_zuse_gates(struct zuse_state* fod_f)
{
#if 0
  u2_wire wir_r = fod_f->wir_r;

  fod_f->ryd = u2_bn_hook
    (wir_r, u2_bn_hook(wir_r, fod_f->pit, "plow"), "read");
#else
  fod_f->ryd = u2_none;
#endif
}

/* _zuse_load_rock(): load engine from rock image.
*/
static void
_zuse_load_rock(struct zuse_state* fod_f,
                const c3_c*        src_c) 
{
  u2_wire wir_r = fod_f->wir_r;
  u2_noun fat = u2_ux_read_deep(wir_r, src_c, "noun");

  if ( u2_none == fat ) {
    u2_bl_bail(wir_r);
  } 
  else {
    fod_f->pyt = u2_ba_sole(wir_r, u2_h(fat));
    fod_f->kol = u2_ba_sole(wir_r, u2_t(fat));
    fod_f->pit = u2_bn_nock(wir_r, _0, fod_f->kol);
    fod_f->bot = u2_bc(wir_r, u2_rx(wir_r, fod_f->pyt), 
                              u2_rx(wir_r, fod_f->pit));
  }
}

/* _zuse_load_cold(): load engine from cold source.
*/
static void
_zuse_load_cold(struct zuse_state* fod_f,
                const c3_c*        src_c)
{
  u2_wire wir_r = fod_f->wir_r;
  u2_noun src   = u2_ux_read(wir_r, src_c, "watt");

  if ( u2_none == src ) {
    u2_bl_bail(wir_r);
  }
  else {
    u2_noun gen, mil, pyt, kol, pit;

    gen = u2_fj_watt(wir_r, src);
    gen = u2_ba_sole(wir_r, gen);
    mil = u2_fj_plow_mill(wir_r, u2_bc(wir_r, c3__cube, _0), gen);
    pyt = u2_bi_h(wir_r, mil);
    kol = u2_bi_t(wir_r, mil);
    {
      // XX: bad dag interaction in old plow code
      //
      u2_rl_gain(wir_r, kol);
    }
    kol = u2_ba_sole(wir_r, kol);
    pit = u2_bn_nock(wir_r, _0, kol);

    fod_f->kol = kol;
    fod_f->pyt = pyt;
    fod_f->pit = pit;
    fod_f->bot = u2_bc(wir_r, u2_rx(wir_r, fod_f->pyt), 
                              u2_rx(wir_r, fod_f->pit));

    _zuse_save_rock(fod_f, src_c);
  }
}

/* zuse_boot(): create the zuse engine.
*/
struct zuse_state*
zuse_boot(const c3_c* src_c)
{
  struct zuse_state* fod_f = malloc(sizeof(struct zuse_state));
  u2_ray wir_r;

  u2_boot();
  wir_r = u2_wr_init(c3__rock, u2_ray_of(0, 0), u2_ray_of(1, 0));
  fod_f->wir_r = wir_r;

  {
    u2_ray jub_r = u2_bl_open(wir_r);

    fprintf(stderr, "boot: %s\n", src_c);
    if ( u2_bl_set(wir_r) ) {
      u2_bl_done(wir_r, jub_r);
      fprintf(stderr, "boot failed\n");

      free(fod_f);
      return 0;
    }
    else {
      u2_bx_boot(wir_r);

      if ( u2_yes == u2_ux_fresh(src_c, "watt", "noun") ) {
        _zuse_load_rock(fod_f, src_c);
      } else {
        _zuse_load_cold(fod_f, src_c);
      }
      _zuse_gates(fod_f);

      u2_bl_done(wir_r, jub_r);
      u2_bx_spot(wir_r, u2_nul);
      u2_bx_show(wir_r);

      return fod_f;
    }
  }
}

/* zuse_test(): attempt zuse recursion.
*/
void
zuse_test(struct zuse_state* fod_f,
          const char*        src_c)
{
  u2_wire wir_r = fod_f->wir_r;
  u2_weak src = u2_ux_read(wir_r, src_c, "watt");

  if ( (0 == fod_f) || (u2_none == src) ) {
    perror(src_c);
    return;
  }
  else {
    u2_ray jub_r = u2_bl_open(wir_r);

    fprintf(stderr, "test: %s\n", src_c);
    if ( u2_bl_set(wir_r) ) {
      u2_bl_done(wir_r, jub_r);

      fprintf(stderr, "test failed\n");
    }
    else {
      u2_noun vor;

      printf("about to read...\n");
      vor = u2_bn_mung(wir_r, fod_f->ryd, src);
      u2_err(wir_r, "read", vor);

      u2_bl_done(wir_r, jub_r);
    }
  }
}

/* zuse_test2(): another test for recursion.
*/
void
zuse_test2(struct zuse_state* fod_f,
           const char*        src_c)
{
  u2_wire wir_r = fod_f->wir_r;
  u2_noun src   = u2_ux_read(wir_r, src_c, "watt");

  if ( u2_none == src ) {
    u2_bl_bail(wir_r);
  }
  else {
    u2_noun gen, mil;

    gen = u2_fj_watt(wir_r, src);
    gen = u2_ba_sole(wir_r, gen);
    mil = u2_fj_plow_mill(wir_r, u2_bc(wir_r, c3__cube, _0), gen);

    printf("test2: plowed.\n");
  }
}

u2_weak
j2_mbc(watt_271, ream)(u2_wire wir_r, 
                       u2_weak txt);

u2_weak
j2_mbc(watt_270, ream)(u2_wire wir_r, 
                       u2_weak txt);

u2_weak
j2_mbc(watt_269, ream)(u2_wire wir_r, 
                       u2_weak txt);

void
j2_mcy(watt_271, ut, dupt)(u2_wire     wir_r,
                           u2_noun     van,
                           const c3_c* cap_c,
                           u2_noun     typ);

/* _zuse_ol_mint(): mint from gen, pit, gol.
*/
u2_weak                                                           //  transfer
_zuse_ol_mint(u2_wire wir_r,
              u2_noun pit,                                        //  retain
              u2_noun typ,                                        //  retain
              u2_noun gol,                                        //  retain
              u2_noun gen)                                        //  retain
{
  u2_noun fut = u2_sh_look(wir_r, pit, "ut");

  if ( u2_none == fut ) {
    printf("zuse: fut failed\n");
    return u2_none;
  } else {
    u2_noun tut = u2_nk_nock(wir_r, u2_rx(wir_r, pit), fut);

    if ( u2_none == tut ) {
      printf("zuse: tut failed\n");
      return u2_none;
    }
    else {
      u2_noun van = u2_rl_molt(wir_r, tut, u2_cv_sam, u2_rx(wir_r, typ), 0);

      u2_rl_lose(wir_r, tut);
      if ( u2_none == van ) {
        printf("zuse: van failed\n");
        return u2_none;
      }
      else {
        u2_noun gom = u2_sh_look(wir_r, van, "mint"); 

        if ( u2_none == gom ) {
          printf("zuse: gom failed\n");
          u2_rl_lose(wir_r, van); return u2_none;
        } else {
          u2_noun mil = u2_nk_nock(wir_r, van, gom);

          if ( u2_none == mil ) {
            printf("zuse: mil failed\n");
            return u2_none;
          }
          else {
            u2_noun ruq = u2_rc(wir_r, u2_rx(wir_r, gol), 
                                       u2_rx(wir_r, gen));

            if ( u2_none == ruq ) {
              printf("zuse: ruq failed\n");
              return u2_none;
            }
            else return u2_nk_mung(wir_r, mil, ruq);
          }
        }
      }
    }
  }
}
/* _zuse_nu_mint(): mint from gene, shoe, type, goal.
*/
u2_weak                                                           //  transfer
_zuse_nu_mint(u2_wire wir_r,
              u2_noun sho,                                        //  retain
              u2_noun typ,                                        //  retain
              u2_noun gol,                                        //  retain
              u2_noun gen)                                        //  retain
{
  return _zuse_ol_mint(wir_r, u2_t(sho), typ, gol, gen);
}

/* _zuse_dump_type(): print type with internal tools.
*/
void
_zuse_dump_type(u2_wire wir_r,
                u2_noun     pit,                                  //  retain
                const c3_c* cap_c,
                u2_noun     typ)
{
  u2_noun fut = u2_sh_look(wir_r, pit, "ut");

  if ( u2_none == fut ) {
    printf("zuse: fut failed\n");
  } else {
    u2_noun van = u2_nk_nock(wir_r, u2_rx(wir_r, pit), fut);

    j2_mcy(watt_271, ut, dupt)(wir_r, van, cap_c, typ);
    u2_rl_lose(wir_r, van);
  }
}

/* zuse_boot_gene(): boot from present shoe and gene, producing new shoe.
*/
u2_noun                                                           //  transfer
zuse_boot_gene(u2_wire wir_r, 
               u2_noun sho,                                       //  retain
               u2_noun gen)                                       //  retain
{
  u2_noun gam = _zuse_nu_mint(wir_r, sho, c3__noun, c3__noun, gen); 

  if ( u2_none == gam ) {
    fprintf(stderr, "  {boot: mint failed}\n");
    u2_bx_show(wir_r);
    u2_bx_spot(wir_r, u2_nul);

    return u2_none;
  }

  {
    u2_noun typ = u2_h(gam);
    u2_noun fol = u2_t(gam);
    u2_noun cor = u2_nk_nock(wir_r, _0, u2_rx(wir_r, fol));   //  !!
    u2_noun hos;

    if ( u2_none == cor ) {
      fprintf(stderr, "  {boot: nock failed}\n");

      hos = u2_none;
      u2_bx_show(wir_r);
      u2_bx_spot(wir_r, u2_nul);
    }
    else {
      hos = u2_bc(wir_r, u2_rx(wir_r, typ), cor);
    }
    u2_rz(wir_r, gam);

    return hos;
  }
}

/* zuse_fire_gene(): execute a stateless kernel expression, as a gene.
*/
void                                                              //  transfer
zuse_fire_gene(u2_wire wir_r, 
               u2_noun sho,                                       //  retain
               u2_noun hos,                                       //  retain
               u2_noun gen)                                       //  retain
{
  u2_noun gam = _zuse_nu_mint(wir_r, sho, u2_h(hos), c3__noun, gen); 

  if ( u2_none == gam ) {
    fprintf(stderr, "  {fire: mint failed}\n");
    return;
  }
  {
    u2_noun typ = u2_h(gam);
    u2_noun fol = u2_t(gam);

    _zuse_dump_type(wir_r, u2_t(sho), 0, typ);
    u2_bx_spot(wir_r, u2_nul);
    {
      u2_noun som = u2_nk_nock(wir_r, u2_rx(wir_r, u2_t(hos)), fol);

      if ( u2_none == som ) {
        fprintf(stderr, "  {fire: nock failed}\n");
      }
      else {
        u2_err(wir_r, 0, som);
        hos = u2_none;
      }
      u2_bx_show(wir_r);
      u2_bx_spot(wir_r, u2_nul);

      u2_rz(wir_r, gam);
      u2_rz(wir_r, som);
    }
  }
}

/* zuse_test3(): accurate use of a true kernel.
*/
void
zuse_test3(struct zuse_state* fod_f,
           const char*        src_c,
           const char*        arg_c)
{
  u2_wire wir_r = fod_f->wir_r;
  u2_noun src   = u2_ux_read(wir_r, src_c, "watt");

  if ( u2_none == src ) {
    printf("test3: %s: no file\n", src_c);
    u2_bl_bail(wir_r);
  }
  else {
    u2_noun gen = j2_mbc(watt_271, ream)(wir_r, src);

    // u2_err(wir_r, "gene", gen);
    if ( u2_none == gen ) {
      printf("test3: %s: no gene\n", src_c);
      return;
    }
    else {
      u2_noun lof = _zuse_ol_mint(wir_r, fod_f->pit, c3__noun, c3__noun, gen); 

      if ( u2_none == lof ) {
        printf("test: failed\n");
      }
      else {
        printf("::::  ::::  ::::  ::::\n");
        u2_bx_spot(wir_r, u2_nul); 
#if 0
        u2_err(wir_r, "type", u2_h(lof));
        u2_err(wir_r, "tool", u2_t(lof));
#endif
        if ( arg_c ) {
          u2_noun typ = u2_h(lof);
          u2_noun tul = u2_t(lof);
          u2_noun pug = u2_bn_nock(wir_r, _0, tul);
          u2_noun src = u2_rl_string(wir_r, arg_c);
          u2_noun ger = j2_mbc(watt_271, ream)(wir_r, src);
          u2_noun hup = _zuse_ol_mint(wir_r, fod_f->pit, typ, c3__noun, ger);

          if ( (u2_none != hup) && (u2_none != pug) ) {
            u2_weak muf = u2_nk_nock(wir_r, pug, u2_t(hup));

            _zuse_dump_type(wir_r, fod_f->pit, 0, u2_h(hup));
            if ( muf != u2_none ) {
              u2_err(wir_r, 0, muf);
            }
            u2_bx_spot(wir_r, u2_nul);
          }
        }
      }
    }
  }
}

/* zuse_next4(): forward iteration from source, still a hack.
*/
void
zuse_next4(struct zuse_state* fod_f,
           const char*        ken_c,        //  now 270
           const char*        nex_c,        //  now 269
           const char*        ult_c,        //  now 268
           const char*        arg_c)        //  command-line string
{
  u2_wire wir_r = fod_f->wir_r;
  u2_noun ken   = u2_ux_read(wir_r, ken_c, "watt");
  u2_noun nex   = u2_ux_read(wir_r, nex_c, "watt");
  u2_noun ult   = u2_ux_read(wir_r, ult_c, "watt");
  u2_noun arg   = arg_c ? u2_bn_string(wir_r, arg_c) : u2_none;

  if ( u2_none == ken ) {
    printf("zuse: %s: no file\n", ken_c);
    u2_bl_bail(wir_r);
  }
  if ( u2_none == nex ) {
    printf("zuse: %s: no file\n", nex_c);
    u2_bl_bail(wir_r);
  }
  if ( u2_none == ult ) {
    printf("zuse: %s: no file\n", ult_c);
    u2_bl_bail(wir_r);
  }

  { 
    u2_noun gen = j2_mbc(watt_271, ream)(wir_r, ken);
    u2_noun xen = j2_mbc(watt_270, ream)(wir_r, nex);
    u2_noun las = j2_mbc(watt_269, ream)(wir_r, ult);
    u2_noun gar = (arg_c ? j2_mbc(watt_269, ream)(wir_r, arg) : u2_none);

    /* Boot sequence!
    */
    {
      u2_noun sho, hos, osh;

      fprintf(stderr, "{boot: %s}\n", ken_c);
      sho = zuse_boot_gene(wir_r, fod_f->bot, gen);
      if ( u2_none == sho ) {
        return;
      }
      fprintf(stderr, "{boot: %s}\n", nex_c);
      hos = zuse_boot_gene(wir_r, sho, xen);
      if ( u2_none == hos ) {
        return;
      }
      fprintf(stderr, "{boot: %s}\n", ult_c);
      osh = zuse_boot_gene(wir_r, hos, las);
      if ( u2_none == osh ) {
        return;
      }

      if ( u2_none != gar ) {
        // fprintf(stderr, "{fire: %s}\n", ken_c);
        // zuse_fire_gene(wir_r, fod_f->bot, sho, gar);
        
        // fprintf(stderr, "{fire: %s}\n", nex_c);
        // zuse_fire_gene(wir_r, sho, hos, gar);

        fprintf(stderr, "{fire: %s}\n", ult_c);
        zuse_fire_gene(wir_r, hos, osh, gar);
      }

      u2_rz(wir_r, sho);
      u2_rz(wir_r, hos);
      u2_rz(wir_r, osh);
    }
  }
}

/* _zuse_from(): expression in pit space.
*/
static u2_noun
_zuse_from(struct zuse_state* fod_f,
           const c3_c*        exp_c)
{
  u2_ray  wir_r = fod_f->wir_r;
  u2_noun exp, gen, fol, val;

  exp = u2_bn_string(wir_r, exp_c);
  gen = u2_fj_watt(wir_r, exp);
  fol = u2_fj_plow_make(wir_r, fod_f->pyt, gen);
  val = u2_bn_nock(wir_r, fod_f->pit, fol);

  return val;
}

/* zuse_line(): execute a zuse line, as command and argument.
*/
void
zuse_line(struct zuse_state* fod_f,
          const c3_c*        cmd_c,
          const c3_c*        arg_c)
{
  u2_ray  wir_r = fod_f->wir_r;
  u2_ray  cap_r = u2_rail_cap_r(wir_r);

  LoomStop = 0;
  u2_bx_boot(wir_r);
  u2_rl_leap(wir_r, c3__rock);
  {
    u2_ray jub_r = u2_bl_open(wir_r);

    if ( u2_bl_set(wir_r) ) {
      u2_bl_done(wir_r, jub_r);
    }
    else {
      if ( !strcmp(cmd_c, "test") ) {
        // zuse_test(fod_f, "watt/t1-273");
        zuse_test3(fod_f, "watt/270", arg_c);
      }
      else if ( !strcmp(cmd_c, "next") ) {
        zuse_next4(fod_f, "watt/270", "watt/269", "watt/268", arg_c);
      }
      else {
#if 0
        u2_noun cmd = u2_bn_string(wir_r, cmd_c);
        u2_noun cor = u2_bn_mung(wir_r, fod_f->ryd, cmd);

        u2_burp(wir_r, 0, u2_fj_prep_noun(wir_r, cor));

        if ( arg_c ) {
          u2_noun arg = u2_bn_string(wir_r, arg_c);
          u2_noun sam = u2_bn_mung(wir_r, fod_f->ryd, arg);

          u2_burp(wir_r, 0, u2_fj_prep_noun(wir_r, sam));
        }
#else
        u2_noun gat, sam, pro;

        /* Construct gate from command.
        */
        gat = _zuse_from(fod_f, cmd_c);

        /* Construct sample from argument.
        */
        sam = _zuse_from(fod_f, arg_c);

        /* Construct product.
        */
        pro = u2_nk_mung(wir_r, gat, sam);

        /* Print, if applicable.
        */
        {
          if ( u2_none != pro ) {
            u2_prep pap = u2_fj_prep_noun(wir_r, pro);

            u2_burp(wir_r, 0, pap);
            u2_bx_spot(wir_r, u2_nul);
          }
        }
      }
#endif
    }
  }
  u2_nk_show(wir_r, stderr);

  u2_rl_fall(wir_r);
  u2_rail_cap_r(wir_r) = cap_r;

  u2_bx_show(wir_r);
}

/* zuse_done(): terminate and free all.
*/
void
zuse_done(struct zuse_state* fod_f)
{
}
