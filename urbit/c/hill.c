/* c/hill.c
**
** This file is in the public domain.
*/
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>

#define U2_GLOBAL
#define C3_GLOBAL
#include "all.h"

#define   PitZ  watt_267
#define   PitA  watt_266
#define   PitB  watt_265
#define   PitC  watt_264

#define   FileZ   "watt/267"
#define   FileA   "watt/266"
#define   FileB   "watt/265"
#define   FileC   "watt/264"

    /* External drivers.
    */
      extern u2_ho_driver j2_da(watt_264);
      extern u2_ho_driver j2_da(watt_265);
      extern u2_ho_driver j2_da(watt_266);
      extern u2_ho_driver j2_da(watt_267);

    /* Built-in battery drivers.   Null `cos` terminates. 
    */
      u2_ho_driver *HostDriverBase[] = {
        &j2_da(watt_264), 
        &j2_da(watt_265), 
        &j2_da(watt_266), 
        &j2_da(watt_267), 
        0
      };

  /**   Interpreter data structures.
  **/
    /*  ++  shoe  <[p=*type q=*noun]>
    */
    typedef u2_noun hi_shoz;                  //  type is PitZ type
    typedef u2_noun hi_shoa;                  //  type is PitA type
    typedef u2_noun hi_shob;                  //  type is PitB type

    /* struct hill_state
    */
      struct hill_state {
        /*  wir - the execution wire.
        */
        u2_ray wir_r;

        /*  soa - shoe A, type/core for PitA, File A by jet boot Z
        */
        hi_shoz soa;

        /*  sob - shoe B, type/core for PitB, File B by mint A
        */
        hi_shoa sob;

        /*  soc - shoe C, type/core for PitC, File C by mint B
        */
        hi_shob soc;
      };
      static struct hill_state *Hill;     //  XX - always & everywhere a hack!


  /**   Direct-jet hacks.  All but PitZ should be removed.
  **/
    /* Minting - for jet boot.
    */
      u2_noun                                                     //  transfer
      j2_mcy(PitZ, ut, mint)(u2_wire wir_r,
                             u2_noun van,                         //  retain
                             u2_noun sut,                         //  retain
                             u2_noun gol,                         //  retain
                             u2_noun gen);                        //  retain
      
    /* Parsing - a hack.
    */
      u2_weak
      j2_mbc(PitZ, ream)(u2_wire wir_r, 
                         u2_noun txt);
      u2_weak
      j2_mby(PitA, ream)(u2_wire wir_r, 
                         u2_noun txt);
      u2_weak
      j2_mby(PitB, ream)(u2_wire wir_r, 
                         u2_noun txt);

    /* Flying - also a hack.
    */
      u2_noun                                                     //  transfer
      j2_mcy(PitA, to, fly)(u2_wire wir_r,
                            u2_noun pup,                          //  retain
                            u2_noun bor);                         //  retain

  /**   Forward declarations.
  **/
#if 1
    static void
    _hill_print_delm(u2_wire, FILE*, u2_noun);
    static void
    _hill_print_term(u2_wire, FILE*, u2_noun);
    static void
    _hill_print_type(u2_wire, FILE*, const c3_c*, u2_noun);
#endif
    static void
    _hill_print_noun(u2_wire, FILE*, const c3_c*, u2_noun);

    static u2_noun
    _hill_nock(u2_wire, u2_noun, u2_noun);

/* _hill_fill_1(): fill one argument in shoe; type assumed
*/
static u2_noun                                                    //  produce
_hill_fill_1(u2_wire wir_r,
             u2_noun sho,                                         //  retain
             u2_noun a)                                           //  retain
{
  return u2_bc
    (wir_r, u2_rx(wir_r, u2_h(sho)),
            u2_bn_molt(wir_r, u2_t(sho), u2_cv_sam, a, 0));  
}

/* _hill_fill_2(): fill two arguments in shoe; type assumed
*/
static u2_noun                                                    //  produce
_hill_fill_2(u2_wire wir_r,
             u2_noun sho,                                         //  retain
             u2_noun a,                                           //  retain
             u2_noun b)                                           //  retain
{
  return u2_bc
    (wir_r, u2_rx(wir_r, u2_h(sho)),
            u2_bn_molt(wir_r, u2_t(sho), u2_cv_sam_2, a, 
                                         u2_cv_sam_3, b,
                                         0));  
}

/* _hill_columns(): return screen column width from OS.
*/
static c3_l
_hill_columns(void)
{
  c3_s buf_s[4];
  
  ioctl(0, TIOCGWINSZ, &buf_s);

  return buf_s[1] - 1;
}

#if 0
/* _hill_fill_3(): fill three arguments in shoe; type assumed
*/
static u2_noun                                                    //  produce
_hill_fill_3(u2_wire wir_r,
             u2_noun sho,                                         //  retain
             u2_noun a,                                           //  retain
             u2_noun b,                                           //  retain
             u2_noun c)                                           //  retain
{
  return u2_bc
    (wir_r, u2_rx(wir_r, u2_h(sho)),
            u2_bn_molt(wir_r, u2_t(sho), u2_cv_sam_2, a, 
                                         u2_cv_sam_6, b,
                                         u2_cv_sam_7, c,
                                         0));  
}
#endif

/* _hill_print_tape(): print a tape of txt to FIL_f.
*/
static void
_hill_print_tape(u2_wire     wir_r,                               
                 FILE*       fil_f,                               //  retain
                 u2_noun     tep)                                 //  retain
{
  if ( 0 == fil_f) fil_f = stdout;

  while ( u2_nul != tep ) {
    c3_c car_c;

    if ( u2_h(tep) >= 127 ) {
      car_c = '?';
    } else car_c = u2_h(tep);

    putc(car_c, fil_f);
    tep = u2_t(tep);
  }
}

/* _hill_print_wall(): print a wall of txt to FIL_f.
*/
static void
_hill_print_wall(u2_wire     wir_r,                               
                 FILE*       fil_f,                               //  retain
                 const c3_c* cap_c,                               //  retain
                 u2_noun     wal)                                 //  retain
{
  if ( 0 == fil_f) fil_f = stdout;

  if ( cap_c && *cap_c ) printf("%s\n", cap_c);

  while ( u2_nul != wal ) {
    if ( cap_c ) { putc(' ', fil_f); putc(' ', fil_f); }
    _hill_print_tape(wir_r, fil_f, u2_h(wal));
    putc(10, fil_f);

    wal = u2_t(wal);
  }
}

/* _hill_wake(): boot pit, from `[type form]` to `[type noun]`.
*/
static u2_noun                                                    //  produce
_hill_wake(u2_wire wir_r,
           u2_noun gam)                                           //  retain
{
  return u2_bc
    (wir_r, u2_rx(wir_r, u2_h(gam)),
            _hill_nock(wir_r, _0, u2_t(gam)));
}

/* _hill_z_mint_gen(): mint `gen` to `[type form]` with PitZ jets.
*/
static u2_noun                                                    //  produce
_hill_z_mint_gen(u2_wire wir_r,
                 u2_noun sut,                                     //  retain
                 u2_noun gol,                                     //  retain
                 u2_noun gen)                                     //  retain
{
  u2_noun gam;
  u2_noun van = u2_bc
    (wir_r, u2_bc(wir_r, 
                  c3__blur,
                  u2_bc(wir_r, u2_bq(wir_r, _0, _0, _0, _0),  // bq for 268 on
                               u2_bc(wir_r, _0, 42))),
            87);

  gam = j2_mcy(PitZ, ut, mint)(wir_r, van, sut, gol, gen);
  u2_rz(wir_r, van);

  return gam;
}

/* _hill_z_mint_txt(): mint `txt` to `[type form]` with PitZ jets.
*/
static u2_noun                                                    //  produce
_hill_z_mint_txt(u2_wire wir_r,
                 u2_noun sut,                                     //  retain
                 u2_noun gol,                                     //  retain
                 u2_atom txt)                                     //  retain
{
  u2_noun gen;

  if ( u2_none == (gen = j2_mbc(PitZ, ream)(wir_r, txt)) ) {
    fprintf(stderr, "{%s: parser failed}\n", FileZ);
    return u2_bl_bail(wir_r, c3__fail);
  }
  else {
    u2_noun ret = _hill_z_mint_gen(wir_r, sut, gol, gen);

    u2_rz(wir_r, gen);
    return ret;
  }
}

/* _hill_z_make_gen(): execute gene against shoe.
*/
static hi_shoz                                                    //  produce
_hill_z_make_gen(u2_wire wir_r,
                 hi_shoz sho,                                     //  retain
                 u2_noun gen)                                     //  retain
{
  u2_noun gam = _hill_z_mint_gen(wir_r, u2_h(sho), c3__noun, gen);
  u2_noun pro = _hill_nock(wir_r, u2_t(sho), u2_t(gam));
  u2_noun ret = u2_bc(wir_r, u2_rx(wir_r, u2_h(gam)), pro);

  u2_rz(wir_r, gam);
  return ret;
}

/* _hill_z_make_txt(): execute string against shoe.
*/
static hi_shoz                                                    //  produce
_hill_z_make_txt(u2_wire wir_r,
                 hi_shoz sho,                                     //  retain
                 u2_atom txt)                                     //  retain
{
  u2_noun gen, ret;

  if ( u2_none == (gen = j2_mbc(PitZ, ream)(wir_r, txt)) ) {
    fprintf(stderr, "{%s: parser failed}\n", FileZ);
    return u2_bl_bail(wir_r, c3__fail);
  }
  ret = _hill_z_make_gen(wir_r, sho, gen);

  u2_rz(wir_r, gen);
  return ret;
}

/* _hill_z_make_txt_c(): execute string against shoe, producing shoe.
*/
static u2_noun                                                    //  produce
_hill_z_make_txt_c(u2_wire     wir_r,
                   u2_noun     sho,                               //  retain
                   const c3_c* txt_c)                             //  retain
{
  u2_noun txt = u2_bn_string(wir_r, txt_c);
  u2_noun ret = _hill_z_make_txt(wir_r, sho, txt);

  u2_rz(wir_r, txt);
  return ret;
}

/* _hill_z_hang(): instantiate and fill a flat core.
*/
static u2_noun                                                    //  produce
_hill_z_hang(u2_wire     wir_r,
             u2_noun     pit,                                     //  retain
             const c3_c* txt_c,                                   //  retain
             u2_noun     sam)                                     //  retain
{
  u2_noun cor = _hill_z_make_txt_c(wir_r, pit, txt_c);
  u2_noun ret = _hill_fill_1(wir_r, cor, sam);

  u2_rz(wir_r, cor);
  return ret;
}

/* _hill_z_call_1(): invoke 1-argument function on core shoe; type assumed
*/
static u2_noun                                                    //  produce
_hill_z_call_1(u2_wire     wir_r,
               hi_shoz     pit,                                   //  retain
               const c3_c* txt_c,                                 //  retain
               u2_noun     a)                                     //  retain
{
  u2_noun cor = _hill_z_make_txt_c(wir_r, pit, txt_c);
  u2_noun lod = _hill_fill_1(wir_r, cor, a);
  u2_noun ret = _hill_z_make_txt_c(wir_r, lod, "$");

  u2_rz(wir_r, cor);
  u2_rz(wir_r, lod);

  return ret;
}

/* _hill_z_call_2(): invoke 2-argument function on core shoe; type assumed
*/
static u2_noun                                                    //  produce
_hill_z_call_2(u2_wire     wir_r,
               hi_shoz     pit,                                   //  retain 
               const c3_c* txt_c,                                 //  retain
               u2_noun     a,                                     //  retain
               u2_noun     b)                                     //  retain
{
  u2_noun cor = _hill_z_make_txt_c(wir_r, pit, txt_c);
  u2_noun lod = _hill_fill_2(wir_r, cor, a, b);
  u2_noun ret = _hill_z_make_txt_c(wir_r, lod, "$");

  u2_rz(wir_r, cor);
  u2_rz(wir_r, lod);

  return ret;
}

/* _hill_z_boot(): generate kernel A (with Z jets).
*/
static u2_noun                                                    //  produce
_hill_z_boot(u2_wire     wir_r,
             const c3_c* fil_c)                                   //  retain
{
  u2_noun txt = u2_ux_read(wir_r, fil_c, "watt");

  if ( u2_none == txt ) {
    fprintf(stderr, "{not found: %s.watt}\n", fil_c);
    return u2_bl_bail(wir_r, c3__fail);
  }
  else {
    u2_noun gam = _hill_z_mint_txt(wir_r, c3__noun, c3__noun, txt); 
    u2_noun soh = _hill_wake(wir_r, gam);

    u2_rz(wir_r, txt);
    u2_rz(wir_r, gam);
    
    return soh;
  }
}

/* _hill_a_mint_gen(): mint `txt` as `[type form]` with PitA shoe.
*/
static u2_noun                                                    //  produce
_hill_a_mint_gen(u2_wire wir_r,
                 hi_shoz soa,                                     //  retain
                 u2_noun sut,                                     //  retain
                 u2_noun gol,                                     //  retain
                 u2_atom gen)                                     //  retain
{
  u2_noun cor = _hill_z_hang(wir_r, soa, "ut", sut);
  u2_noun dog = _hill_z_call_2(wir_r, cor, "mint", gol, gen);
  u2_noun pro = u2_rx(wir_r, u2_t(dog));

  u2_rz(wir_r, cor);
  u2_rz(wir_r, dog);

  return pro;
}

/* _hill_a_mint_txt(): mint `txt` as `[type form]` with PitA shoe.
*/
static u2_noun                                                    //  produce
_hill_a_mint_txt(u2_wire wir_r,
                 hi_shoz soa,                                     //  retain
                 u2_noun sut,                                     //  retain
                 u2_noun gol,                                     //  retain
                 u2_atom txt)                                     //  retain
{
  u2_noun gen;

  if ( u2_none == (gen = j2_mby(PitA, ream)(wir_r, txt)) ) {
    fprintf(stderr, "{%s: parse failed}\n", FileA);
    return u2_bl_bail(wir_r, c3__fail);
  }
  else {
    u2_noun ret = _hill_a_mint_gen(wir_r, soa, sut, gol, gen);

    u2_rz(wir_r, gen);
    return ret;
  }
}

/* _hill_a_ram(): dump `bil` as a flat tape.
*/
static u2_noun
_hill_a_ram(u2_wire wir_r,
            hi_shoz soa,
            u2_noun bil)
{
  u2_noun cor = _hill_z_hang(wir_r, soa, "to", bil);
  u2_noun dog = _hill_z_make_txt_c(wir_r, cor, "ram");
  u2_noun pro = u2_rx(wir_r, u2_t(dog));

  u2_rz(wir_r, cor);
  u2_rz(wir_r, dog);

  return pro;
}

/* _hill_a_fly(): dump `bil` to `col`, producing wall.
*/
static u2_noun
_hill_a_fly(u2_wire wir_r,
            hi_shoz soa,
            u2_noun bil,
            c3_l    col_l)
{
  u2_noun cor = _hill_z_hang(wir_r, soa, "to", bil);
  u2_noun dog = _hill_z_call_1(wir_r, cor, "fly", col_l);
  u2_noun pro = u2_rx(wir_r, u2_t(dog));

  u2_rz(wir_r, cor);
  u2_rz(wir_r, dog);

  return pro;
}

/* _hill_a_dump(): dump `typ`, producing bill.
*/
static u2_noun                                                    //  produce
_hill_a_dump(u2_wire wir_r,
             hi_shoz soa,                                         //  retain
             u2_noun typ)                                         //  retain
{
  u2_noun cor = _hill_z_hang(wir_r, soa, "ut", typ);
  u2_noun dog = _hill_z_make_txt_c(wir_r, cor, "dump");
  u2_noun pro = u2_rx(wir_r, u2_t(dog));

  u2_rz(wir_r, cor);
  u2_rz(wir_r, dog);

  return pro;
}

/* _hill_a_delm(): dump decimal atom, producing bill.
*/
static u2_noun
_hill_a_delm(u2_wire wir_r,
             hi_shoz soa,
             u2_atom dem)
{
  u2_noun cor = _hill_z_hang(wir_r, soa, "go", dem);
  u2_noun dog = _hill_z_make_txt_c(wir_r, cor, "dem");
  u2_noun pro = u2_rx(wir_r, u2_t(dog));

  u2_rz(wir_r, cor);
  u2_rz(wir_r, dog);

  return pro;
}

/* _hill_a_term(): dump decimal atom, producing bill.
*/
static u2_noun
_hill_a_term(u2_wire wir_r,
             hi_shoz soa,
             u2_atom tem)
{
  u2_noun cor = _hill_z_hang(wir_r, soa, "go", tem);
  u2_noun dog = _hill_z_make_txt_c(wir_r, cor, "tem");
  u2_noun pro = u2_rx(wir_r, u2_t(dog));

  u2_rz(wir_r, cor);
  u2_rz(wir_r, dog);

  return pro;
}

/* _hill_a_gunt(): dump untyped noun, producing bill.
*/
static u2_noun
_hill_a_gunt(u2_wire wir_r,
             hi_shoz soa,
             u2_noun som)
{
  u2_noun cor = _hill_z_hang(wir_r, soa, "go", som);
  u2_noun dog = _hill_z_make_txt_c(wir_r, cor, "gut");
  u2_noun pro = u2_rx(wir_r, u2_t(dog));

  u2_rz(wir_r, cor);
  u2_rz(wir_r, dog);

  return pro;
}

/* _hill_a_make_gen(): execute gene against core.
*/
static hi_shoa                                                    //  produce
_hill_a_make_gen(u2_wire wir_r,
                 hi_shoz soa,                                     //  retain
                 hi_shoa cor,                                     //  retain
                 u2_noun gen)                                     //  retain
{
  u2_noun gam = _hill_a_mint_gen(wir_r, soa, u2_h(cor), c3__noun, gen);
  u2_noun pro = _hill_nock(wir_r, u2_t(cor), u2_t(gam));
  u2_noun ret = u2_bc(wir_r, u2_rx(wir_r, u2_h(gam)), pro);

  u2_rz(wir_r, gam);
  return ret;
}

/* _hill_a_make_txt(): execute string against core.
*/
static hi_shoa                                                    //  produce
_hill_a_make_txt(u2_wire wir_r,
                 hi_shoz soa,                                     //  retain
                 hi_shoa cor,                                     //  retain
                 u2_atom txt)                                     //  retain
{
  u2_noun gen, ret;

  if ( u2_none == (gen = j2_mbc(PitZ, ream)(wir_r, txt)) ) {
    fprintf(stderr, "{%s: parser failed}\n", FileZ);
    return u2_bl_bail(wir_r, c3__fail);
  }
  ret = _hill_a_make_gen(wir_r, soa, cor, gen);

  u2_rz(wir_r, gen);
  return ret;
}

/* _hill_a_make_txt_c(): execute string against shoe, producing shoe.
*/
static hi_shoa                                                    //  produce
_hill_a_make_txt_c(u2_wire     wir_r,
                   hi_shoz     soa,                               //  retain
                   hi_shoa     cor,                               //  retain
                   const c3_c* txt_c)                             //  retain
{
  u2_noun txt = u2_bn_string(wir_r, txt_c);
  u2_noun ret = _hill_a_make_txt(wir_r, soa, cor, txt);

  u2_rz(wir_r, txt);
  return ret;
}

/* _hill_a_hang(): instantiate and fill a flat core.
*/
static hi_shoa                                                    //  produce
_hill_a_hang(u2_wire     wir_r,
             hi_shoz     soa,                                     //  retain
             hi_shoa     sob,                                     //  retain
             const c3_c* txt_c,                                   //  retain
             u2_noun     sam)                                     //  retain
{
  u2_noun cor = _hill_a_make_txt_c(wir_r, soa, sob, txt_c);
  u2_noun ret = _hill_fill_1(wir_r, cor, sam);

  u2_rz(wir_r, cor);
  return ret;
}

/* _hill_a_call_1(): invoke 1-argument function on core shoe; type assumed
*/
static hi_shoa                                                    //  produce
_hill_a_call_1(u2_wire     wir_r,
               hi_shoz     soa,                                   //  retain
               hi_shoa     sob,                                   //  retain
               const c3_c* txt_c,                                 //  retain
               u2_noun     a)                                     //  retain
{
  u2_noun cor = _hill_a_make_txt_c(wir_r, soa, sob, txt_c);
  u2_noun lod = _hill_fill_1(wir_r, cor, a);
  u2_noun ret = _hill_a_make_txt_c(wir_r, soa, lod, "$");

  u2_rz(wir_r, cor);
  u2_rz(wir_r, lod);

  return ret;
}

/* _hill_a_call_2(): invoke 1-argument function on core shoe; type assumed
*/
static hi_shoa                                                    //  produce
_hill_a_call_2(u2_wire     wir_r,
               hi_shoz     soa,                                   //  retain
               hi_shoa     sob,                                   //  retain 
               const c3_c* txt_c,                                 //  retain
               u2_noun     a,                                     //  retain
               u2_noun     b)                                     //  retain
{
  u2_noun cor = _hill_a_make_txt_c(wir_r, soa, sob, txt_c);
  u2_noun lod = _hill_fill_2(wir_r, cor, a, b);
  u2_noun ret = _hill_a_make_txt_c(wir_r, soa, lod, "$");

  u2_rz(wir_r, cor);
  u2_rz(wir_r, lod);

  return ret;
}

/* _hill_a_boot(): generate kernel B (with kernel A).
*/
static u2_noun                                                    //  produce
_hill_a_boot(u2_wire     wir_r,
             hi_shoz     soa,                                     //  retain
             const c3_c* fil_c)                                   //  retain
{
  u2_noun txt = u2_ux_read(wir_r, fil_c, "watt");

  if ( u2_none == txt ) {
    fprintf(stderr, "{not found: %s.watt}\n", fil_c);
    return u2_bl_bail(wir_r, c3__fail);
  }
  else {
    u2_noun gam = _hill_a_mint_txt(wir_r, soa, c3__noun, c3__noun, txt); 
    u2_noun soh = _hill_wake(wir_r, gam);

    u2_rz(wir_r, txt);
    u2_rz(wir_r, gam);
    
    return soh;
  }
}

/* _hill_a_print_delm():
*/
static void
_hill_a_print_delm(u2_wire     wir_r,
                   hi_shoz     soa,                               //  retain
                   FILE*       fil_f,                             //  retain
                   u2_atom     dem)                               //  retain
{
  u2_noun bil = _hill_a_delm(wir_r, soa, dem);
  u2_noun tep = _hill_a_ram(wir_r, soa, bil);

  _hill_print_tape(wir_r, fil_f, tep);

  u2_rz(wir_r, bil);
  u2_rz(wir_r, tep);
}

/* _hill_a_print_term():
*/
static void
_hill_a_print_term(u2_wire     wir_r,
                   hi_shoz     soa,                               //  retain
                   FILE*       fil_f,                             //  retain
                   u2_atom     tem)                               //  retain
{
  u2_noun bil = _hill_a_term(wir_r, soa, tem);
  u2_noun tep = _hill_a_ram(wir_r, soa, bil);

  _hill_print_tape(wir_r, fil_f, tep);

  u2_rz(wir_r, bil);
  u2_rz(wir_r, tep);
}

/* _hill_a_print_type():
*/
static void
_hill_a_print_type(u2_wire     wir_r,
                   hi_shoz     soa,                               //  retain
                   FILE*       fil_f,                             //  retain
                   const c3_c* cap_c,                             //  retain
                   u2_noun     typ)                               //  retain
{
  u2_noun bil = _hill_a_dump(wir_r, soa, typ);
  u2_noun wal = _hill_a_fly(wir_r, soa, bil, _hill_columns());

  _hill_print_wall(wir_r, fil_f, cap_c, wal);

  u2_rz(wir_r, bil);
  u2_rz(wir_r, wal);
}

/* _hill_a_print_noun():
*/
static void
_hill_a_print_noun(u2_wire     wir_r,
                   hi_shoz     soa,                               //  retain
                   FILE*       fil_f,                             //  retain
                   const c3_c* cap_c,                             //  retain
                   u2_noun     typ)                               //  retain
{
  u2_noun bil = _hill_a_gunt(wir_r, soa, typ);
  u2_noun wal = _hill_a_fly(wir_r, soa, bil, _hill_columns());

  _hill_print_wall(wir_r, fil_f, cap_c, wal);

  u2_rz(wir_r, bil);
  u2_rz(wir_r, wal);
}

/* _hill_a_fire(): execute and print expression over pit B (with shoe A).
*/
static void                                                       //  produce
_hill_a_fire(u2_wire     wir_r,
             u2_noun     soa,                                     //  retain
             u2_noun     sob,                                     //  retain
             const c3_c* exp_c,                                   //  retain
             const c3_c* out_c)                                   //  retain
{
  u2_noun txt, gam, som;

  txt = u2_bn_string(wir_r, exp_c);
  gam = _hill_a_mint_txt(wir_r, soa, u2_h(sob), c3__noun, txt);

  _hill_a_print_type(wir_r, soa, 0, 0, u2_h(gam));

  u2_bx_boot(wir_r);
  som = _hill_nock(wir_r, u2_t(sob), u2_t(gam));
  u2_bx_show(wir_r);

  if ( u2_none == som ) {
    fprintf(stderr, "{none}\n");
  }
  else {
    if ( !out_c ) {
      _hill_print_noun(wir_r, 0, 0, som);
    } else if ( !strcmp("w", out_c) ) {
      _hill_print_wall(wir_r, 0, 0, som);
    }
    else if ( !strcmp("t", out_c) ) {
      _hill_print_tape(wir_r, 0, som); printf("\n");
    }
    else if ( !strcmp("d", out_c) ) {
      _hill_print_delm(wir_r, 0, som); printf("\n");
    }
    else if ( !strcmp("e", out_c) ) {
      _hill_print_term(wir_r, 0, som); printf("\n");
    }
    else if ( !strcmp("y", out_c) ) {
      _hill_print_type(wir_r, 0, 0, som);
    }
  }
  u2_rz(wir_r, txt);
  u2_rz(wir_r, gam);
  u2_rz(wir_r, som);
}

/* _hill_b_mint_gen(): mint `txt` as `[type form]` with PitB shoe.
*/
static u2_noun                                                    //  produce
_hill_b_mint_gen(u2_wire wir_r,
                 hi_shoz soa,                                     //  retain
                 hi_shoa sob,                                     //  retain
                 u2_noun sut,                                     //  retain
                 u2_noun gol,                                     //  retain
                 u2_atom gen)                                     //  retain
{
  u2_noun cor = _hill_a_hang(wir_r, soa, sob, "ut", sut);
  u2_noun dog = _hill_a_call_2(wir_r, soa, cor, "mint", gol, gen);
  u2_noun pro = u2_rx(wir_r, u2_t(dog));

  u2_rz(wir_r, cor);
  u2_rz(wir_r, dog);

  return pro;
}

/* _hill_b_jam(): jam with PitB shoe.
*/
static u2_noun
_hill_b_jam(u2_wire wir_r,
            hi_shoz soa,
            hi_shoa sob,
            u2_noun som)
{
  u2_noun dog = _hill_a_call_1(wir_r, soa, sob, "jam", som);
  u2_noun pro = u2_rx(wir_r, u2_t(dog));

  u2_rz(wir_r, dog);
  return pro;
}
            
/* _hill_b_cue(): cue with PitB shoe.
*/
static u2_noun
_hill_b_cue(u2_wire wir_r,
            hi_shoz soa,
            hi_shoa sob,
            u2_noun som)
{
  u2_noun dog = _hill_a_call_1(wir_r, soa, sob, "cue", som);
  u2_noun pro = u2_rx(wir_r, u2_t(dog));

  u2_rz(wir_r, dog);
  return pro;
}
            
/* _hill_b_ream_txt(): ream `txt` as gene.
*/
static u2_noun 
_hill_b_ream_txt(u2_wire wir_r,
                 hi_shoz soa,
                 hi_shoa sob,
                 u2_atom txt)
{
  u2_noun duk = _hill_a_call_1(wir_r, soa, sob, "ream", txt);
  u2_noun pro = u2_rx(wir_r, u2_t(duk));

  u2_rz(wir_r, duk);
  return pro;
}

/* _hill_b_mint_txt(): mint `txt` as `[type form]` with PitA shoe.
*/
static u2_noun                                                    //  produce
_hill_b_mint_txt(u2_wire wir_r,
                 hi_shoz soa,                                     //  retain
                 hi_shoa sob,                                     //  retain
                 u2_noun sut,                                     //  retain
                 u2_noun gol,                                     //  retain
                 u2_atom txt)                                     //  retain
{
  u2_noun gen = _hill_b_ream_txt(wir_r, soa, sob, txt);
  u2_noun ret = _hill_b_mint_gen(wir_r, soa, sob, sut, gol, gen);

  u2_rz(wir_r, gen);
  return ret;
}

/* _hill_b_boot(): generate kernel C (with kernel B).
*/
static u2_noun                                                    //  produce
_hill_b_boot(u2_wire     wir_r,
             hi_shoz     soa,                                     //  retain
             hi_shoa     sob,                                     //  retain
             const c3_c* fil_c)                                   //  retain
{
  u2_noun txt = u2_ux_read(wir_r, fil_c, "watt");

  if ( u2_none == txt ) {
    fprintf(stderr, "{not found: %s.watt}\n", fil_c);
    return u2_bl_bail(wir_r, c3__fail);
  }
  else {
    u2_noun gam = _hill_b_mint_txt(wir_r, soa, sob, c3__noun, c3__noun, txt); 
    u2_noun soh = _hill_wake(wir_r, gam);

    u2_rz(wir_r, txt);
    u2_rz(wir_r, gam);
    
    return soh;
  }
}

/* _hill_b_eyre(): generate the first eyre kernel (with kernel B).
*/
static u2_noun                                                    //  produce
_hill_b_eyre(u2_wire     wir_r,
             hi_shoz     soa,                                     //  retain
             hi_shoa     sob,                                     //  retain
             const c3_c* fil_c)                                   //  retain
{
  u2_noun txt = u2_ux_read(wir_r, fil_c, "watt");

  if ( u2_none == txt ) {
    fprintf(stderr, "{not found: %s.watt}\n", fil_c);
    return u2_bl_bail(wir_r, c3__fail);
  }
  else {
    u2_noun gam = _hill_b_mint_txt(wir_r, soa, sob, c3__noun, c3__noun, txt); 
    u2_noun nok = u2_rx(wir_r, u2_t(gam));

    u2_rz(wir_r, txt);
    u2_rz(wir_r, gam);
 
    return nok;
  }
}

/* _hill_b_fly(): dump `bil` to `col`, producing wall.
*/
static u2_noun
_hill_b_fly(u2_wire wir_r,
            hi_shoz soa,
            hi_shoa sob,
            u2_noun bil,
            c3_l    col_l)
{
  u2_noun cor = _hill_a_hang(wir_r, soa, sob, "to", bil);
  u2_noun dog = _hill_a_call_1(wir_r, soa, cor, "fly", col_l);
  u2_noun pro = u2_rx(wir_r, u2_t(dog));

  u2_rz(wir_r, cor);
  u2_rz(wir_r, dog);

  return pro;
}

/* _hill_b_dump(): dump `typ`, producing bill.
*/
static u2_noun                                                    //  produce
_hill_b_dump(u2_wire wir_r,
             hi_shoz soa,                                         //  retain
             hi_shoa sob,                                         //  retain
             u2_noun typ)                                         //  retain
{
  u2_noun cor = _hill_a_hang(wir_r, soa, sob, "ut", typ);
  u2_noun dog = _hill_a_make_txt_c(wir_r, soa, cor, "dump");
  u2_noun pro = u2_rx(wir_r, u2_t(dog));

  u2_rz(wir_r, cor);
  u2_rz(wir_r, dog);

  return pro;
}

/* _hill_b_print_type():
*/
static void
_hill_b_print_type(u2_wire     wir_r,
                   hi_shoz     soa,                               //  retain
                   hi_shoa     sob,                               //  retain
                   FILE*       fil_f,                             //  retain
                   const c3_c* cap_c,                             //  retain
                   u2_noun     typ)                               //  retain
{
  u2_noun bil = _hill_b_dump(wir_r, soa, sob, typ);
  u2_noun wal = _hill_b_fly(wir_r, soa, sob, bil, _hill_columns());

  _hill_print_wall(wir_r, fil_f, cap_c, wal);

  u2_rz(wir_r, bil);
  u2_rz(wir_r, wal);
}

/* _hill_b_fire(): execute and print expression over pit C (with shoe B).
*/
static void                                                       //  produce
_hill_b_fire(u2_wire     wir_r,
             u2_noun     soa,                                     //  retain
             u2_noun     sob,                                     //  retain
             u2_noun     soc,                                     //  retain
             const c3_c* exp_c,                                   //  retain
             const c3_c* out_c)                                   //  retain
{
  u2_noun txt, gam, som;

  txt = u2_bn_string(wir_r, exp_c);
  gam = _hill_b_mint_txt(wir_r, soa, sob, u2_h(soc), c3__noun, txt);

  _hill_b_print_type(wir_r, soa, sob, 0, 0, u2_h(gam));

  if ( out_c && !strcmp("p", out_c) ) {
    u2_rz(wir_r, txt);
    u2_rz(wir_r, gam);
    return;
  }
  u2_bx_boot(wir_r);
  som = _hill_nock(wir_r, u2_t(soc), u2_t(gam));
  u2_bx_show(wir_r);

  if ( u2_none == som ) {
    fprintf(stderr, "{none}\n");
  }
  else {
    if ( !out_c ) {
      _hill_print_noun(wir_r, 0, 0, som);
    } else if ( !strcmp("w", out_c) ) {
      _hill_print_wall(wir_r, 0, 0, som);
    }
    else if ( !strcmp("t", out_c) ) {
      _hill_print_tape(wir_r, 0, som); printf("\n");
    }
    else if ( !strcmp("d", out_c) ) {
      _hill_print_delm(wir_r, 0, som); printf("\n");
    }
    else if ( !strcmp("e", out_c) ) {
      _hill_print_term(wir_r, 0, som); printf("\n");
    }
    else if ( !strcmp("y", out_c) ) {
      _hill_print_type(wir_r, 0, 0, som);
    }
  }
  u2_rz(wir_r, txt);
  u2_rz(wir_r, gam);
  u2_rz(wir_r, som);
}

/* _hill_print_delm(): print wrapper for decimal.
*/
static void
_hill_print_delm(u2_wire     wir_r,
                 FILE*       fil_f,                               //  retain
                 u2_noun     som)                                 //  retain
{
  _hill_a_print_delm(wir_r, Hill->soa, fil_f, som);
}

/* _hill_print_term(): print wrapper for terminal.
*/
static void
_hill_print_term(u2_wire     wir_r,
                 FILE*       fil_f,                               //  retain
                 u2_noun     som)                                 //  retain
{
  _hill_a_print_term(wir_r, Hill->soa, fil_f, som);
}

/* _hill_print_type(): print wrapper for type.
*/
static void
_hill_print_type(u2_wire     wir_r,
                 FILE*       fil_f,                               //  retain
                 const c3_c* cap_c,                               //  retain
                 u2_noun     typ)                                 //  retain
{
  _hill_a_print_type(wir_r, Hill->soa, fil_f, cap_c, typ);
}

/* _hill_print_noun(): print wrapper for noun.
*/
static void
_hill_print_noun(u2_wire     wir_r,
                 FILE*       fil_f,                               //  retain
                 const c3_c* cap_c,                               //  retain
                 u2_noun     som)                                 //  retain
{
  _hill_a_print_noun(wir_r, Hill->soa, fil_f, cap_c, som);
}

/* _hill_print_spot(): print wrapper for file location, at tab of 2.
*/
static void
_hill_print_spot(u2_wire wir_r,
                 FILE*   fil_f,                                   //  retain
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
    u2_noun blt = _hill_a_term(wir_r, Hill->soa, p_sot);
    u2_noun bla = _hill_a_delm(wir_r, Hill->soa, pq_sot);
    u2_noun blb = _hill_a_delm(wir_r, Hill->soa, qq_sot);
    u2_noun blc = _hill_a_delm(wir_r, Hill->soa, pr_sot);
    u2_noun bld = _hill_a_delm(wir_r, Hill->soa, qr_sot);
    u2_noun tlt = _hill_a_ram(wir_r, Hill->soa, blt);
    u2_noun tla = _hill_a_ram(wir_r, Hill->soa, bla);
    u2_noun tlb = _hill_a_ram(wir_r, Hill->soa, blb);
    u2_noun tlc = _hill_a_ram(wir_r, Hill->soa, blc);
    u2_noun tld = _hill_a_ram(wir_r, Hill->soa, bld);

    fprintf(fil_f, " {");
    if ( u2_nul != tlt ) {
      _hill_print_tape(wir_r, fil_f, tlt);
      fprintf(fil_f, ": ");
    }

    _hill_print_tape(wir_r, fil_f, tla);
    fprintf(fil_f, ".");
    _hill_print_tape(wir_r, fil_f, tlb);
    fprintf(fil_f, ":");
    _hill_print_tape(wir_r, fil_f, tlc);
    fprintf(fil_f, ".");
    _hill_print_tape(wir_r, fil_f, tld);
    fprintf(fil_f, "}\n");
     
    u2_rz(wir_r, blt);
    u2_rz(wir_r, bla);
    u2_rz(wir_r, blb);
    u2_rz(wir_r, blc);
    u2_rz(wir_r, bld);

    u2_rz(wir_r, tlt);
    u2_rz(wir_r, tla);
    u2_rz(wir_r, tlb);
    u2_rz(wir_r, tlc);
    u2_rz(wir_r, tld);

    return;
  }
  fprintf(fil_f, "  {spot!}\n");
  u2_err(wir_r, "{{invalid}}", sot);
}

/* _hill_print_bean(): print wrapper for flat meaning.
*/
static void
_hill_print_bean(u2_wire wir_r,
                 FILE*   fil_f,                                   //  retain
                 u2_noun ben)                                     //  retain
{
#if 0
  u2_noun wal = _hill_a_fly(wir_r, Hill->soa, ben, _hill_columns());
#else
  //  This will bail safely if the benl is bad, rather than 
  //  recursing back into _hill_nock().
  //
  u2_noun wal = j2_mcy(PitA, to, fly)(wir_r, ben, _hill_columns());
#endif
  _hill_print_wall(wir_r, fil_f, "", wal);
  u2_rz(wir_r, wal);
  return;
}

/* _hill_print_mean(): print wrapper for meta-meaning.
*/
static void
_hill_print_mean(u2_wire wir_r,
                 FILE*   fil_f,                                   //  retain
                 u2_noun mon)                                     //  retain
{
  if ( u2_yes == u2_dust(mon) ) {
    u2_noun ben = u2_nk_nock(wir_r, u2_rx(wir_r, mon), u2_t(mon));

    if ( u2_none != ben ) {
      _hill_print_bean(wir_r, fil_f, ben);
      u2_rz(wir_r, ben);
      return;
    }
    else fprintf(fil_f, "  {maen!}\n");
  }
  fprintf(fil_f, "  {mean!}\n");
}

/* _hill_print_tent(): print wrapper for trace entry.
*/
static void
_hill_print_tent(u2_wire wir_r,
                 FILE*   fil_f,                                   //  retain
                 u2_noun tax)                                     //  retain
{
  if ( u2_yes == u2_dust(tax) ) switch ( u2_h(tax) ) {
    case c3__spot: _hill_print_spot(wir_r, fil_f, u2_t(tax)); return;
    case c3__bean: _hill_print_bean(wir_r, fil_f, u2_t(tax)); return;
    case c3__mean: _hill_print_mean(wir_r, fil_f, u2_t(tax)); return;
  }
  fprintf(fil_f, "  {tent!}\n");
}

/* _hill_print_trac(): print wrapper for trace stack.
*/
static void
_hill_print_trac(u2_wire wir_r,
                 FILE*   fil_f,                                   //  retain
                 u2_noun tax)                                     //  retain
{
  while ( u2_nul != tax ) {
    _hill_print_tent(wir_r, fil_f, u2_h(tax));
    tax = u2_t(tax);
  }
}

/* _hill_nock(): control and trace wrapper for interpreter.
*/
static u2_noun                                                    //  produce
_hill_nock(u2_wire wir_r,
           u2_noun bus,                                           //  retain
           u2_noun fol)                                           //  retain
{
  u2_noun pro;

  pro = u2_nk_nock(wir_r, u2_rx(wir_r, bus), fol);

  if ( u2_none != pro ) {
    return pro;
  }
  else {
    u2_ray kit_r = u2_bl_open(wir_r);

    if ( u2_bl_set(wir_r) ) {
      u2_bl_done(wir_r, kit_r);
      fprintf(stderr, "{trace failed!}\n");
    }
    else {
      u2_noun tax;
     
      fprintf(stderr, "{trace}\n");
      tax = u2_rx(wir_r, u2_wire_tax(wir_r));
      u2_wire_tax(wir_r) = u2_nul;

      _hill_print_trac(wir_r, stderr, tax);
      u2_rz(wir_r, tax);

      u2_bl_done(wir_r, kit_r);
    }

    u2_bl_bail(wir_r, c3__fail);
    return u2_none;
  }
}

/* hill_boot(): create the hill engine.
*/
struct hill_state*                                                //  produce
hill_boot(void)
{
  struct hill_state* hil_h = malloc(sizeof(struct hill_state));
  u2_ray wir_r;

  u2_boot();
  wir_r = u2_wr_init(c3__rock, u2_ray_of(0, 0), u2_ray_of(1, 0));

  Hill = hil_h;
  Hill->wir_r = wir_r;
  Hill->soa = u2_none;
  Hill->sob = u2_none;
  Hill->soc = u2_none;

  /* Mint the shoes.  Impeccable memory practices.
  */
  {
    u2_noun soa = u2_none;
    u2_noun sob = u2_none;
    u2_noun soc = u2_none;

    do {
      /* Boot shoe A.
      */
      if ( u2_no == u2_rl_leap(wir_r, c3__rock) ) {
        c3_assert(0);
      }
      u2_bx_boot(wir_r);
      {
        u2_ray  kit_r = u2_bl_open(wir_r);

        if ( u2_bl_set(wir_r) ) {
          u2_bl_done(wir_r, kit_r);
          u2_rl_fall(wir_r);
          fprintf(stderr, "{no boot, a}\n");
          break;
        }
        else {
          soa = _hill_z_boot(wir_r, FileA);
          u2_bl_done(wir_r, kit_r);
          u2_bx_spot(wir_r, u2_nul);
          u2_bx_show(wir_r);
        }
      }
      fprintf(stderr, "{cold boot: %s, with %s jets: %x}\n", 
          FileA, FileZ, u2_mug(soa));
      Hill->soa = u2_rl_take(u2_wire_bas_r(wir_r), soa);
      u2_rl_fall(wir_r);

      /* Boot shoe B.
      */
      if ( u2_no == u2_rl_leap(wir_r, c3__rock) ) {
        c3_assert(0);
      }
      u2_bx_boot(wir_r);
      {
        u2_ray  kit_r = u2_bl_open(wir_r);

        if ( u2_bl_set(wir_r) ) {
          u2_bl_done(wir_r, kit_r);
          u2_rl_fall(wir_r);
          fprintf(stderr, "{no boot, b}\n");
          break;
        }
        else {
          sob = _hill_a_boot(wir_r, soa, FileB);
          u2_bl_done(wir_r, kit_r);

          u2_bx_spot(wir_r, u2_nul);
          u2_bx_show(wir_r);
        }
      }
      fprintf(stderr, "{warm boot: %s, with %s: %x}\n", 
          FileB, FileA, u2_mug(sob));
      Hill->sob = u2_rl_take(u2_wire_bas_r(wir_r), sob);
      u2_rl_fall(wir_r);

      /* Boot shoe C.
      */
      if ( u2_no == u2_rl_leap(wir_r, c3__rock) ) {
        c3_assert(0);
      }
      u2_bx_boot(wir_r);
      {
        u2_ray  kit_r = u2_bl_open(wir_r);

        if ( u2_bl_set(wir_r) ) {
          u2_bl_done(wir_r, kit_r);
          u2_rl_fall(wir_r);
          fprintf(stderr, "{no boot, c}\n");
          u2_bx_show(wir_r);
          break;
        }
        else {
          soc = _hill_b_eyre(wir_r, soa, sob, FileC);
          u2_bl_done(wir_r, kit_r);

          u2_bx_spot(wir_r, u2_nul);
          u2_bx_show(wir_r);
        }
      }
      fprintf(stderr, "{last boot: %s, with %s: %x}\n", 
          FileC, FileB, u2_mug(soc));
      Hill->soc = u2_rl_take(u2_wire_bas_r(wir_r), soc);
      u2_rl_fall(wir_r);

      /* Testing basics of soc.
      */
      printf("testing eyre...\n");
      {
        u2_noun foo = u2_rl_string(wir_r, "|!(a=@ (dec a))");
        u2_noun bar = u2_nk_nock(wir_r, foo, Hill->soc);

        if ( u2_none == bar ) {
          printf("no bar\n");
        }
        else {
          u2_noun moo = u2_nk_nock(wir_r, _0, bar);
            
          if ( u2_none == moo ) {
            printf("no moo\n");
          } else {
            u2_noun zor = u2_nk_mung(wir_r, moo, 13);

            u2_err(wir_r, "zor", zor);
          }
        }
      }
      printf("tested.\n");

#if 1
      {
        u2_noun soa = Hill->soa;
        u2_noun sob = Hill->sob;
        u2_noun dat = Hill->soc;
        u2_noun pak, bag;

        fprintf(stderr, "jam test: jam\n");
        u2_bx_boot(wir_r);
        pak = _hill_b_jam(wir_r, soa, sob, dat);
        u2_bx_show(wir_r);

        fprintf(stderr, "jam test: %d bits\n", u2_met(0, pak));
        u2_ux_write(wir_r, pak, "watt/264", "noun");

        fprintf(stderr, "jam test: cue\n");
        u2_bx_boot(wir_r);
        bag = _hill_b_cue(wir_r, soa, sob, pak);
        u2_bx_show(wir_r);

        if ( u2_yes == u2_sing(bag, dat) ) {
          fprintf(stderr, "jam test: match\n");
        } else {
          fprintf(stderr, "jam test: NO MATCH\n");
        }
      }
#endif
      return Hill;
    } while (0);

    free(Hill);
    return 0;
  }
}

/* hill_lose(): terminate and free state.
*/
void
hill_lose(struct hill_state* hil_h)                               //  submit
{
  u2_wire wir_r = hil_h->wir_r;

  u2_rz(wir_r, hil_h->soa);
  u2_rz(wir_r, hil_h->sob);
}

int FooBar;

/* hill_line(): execute a hill command line.
*/
void
hill_line(struct hill_state* hil_h,
          const c3_c*        lin_c)
{
  u2_wire wir_r = hil_h->wir_r;
  hi_shoa soa   = hil_h->soa;
  hi_shoz sob   = hil_h->sob;
  hi_shob soc   = hil_h->soc;
  const c3_c* out_c = 0;

  u2_bx_boot(wir_r);
  u2_bx_spot(wir_r, u2_nul);

  //  XX - a heinous hack.
  //
#if 0
  if ( !strcmp(lin_c, "!") ) {
    FooBar = 1;
    {
      u2_ray  kit_r = u2_bl_open(wir_r);

      if ( u2_bl_set(wir_r) ) {
        u2_bl_done(wir_r, kit_r);
        fprintf(stderr, "{no boot, c}\n");
      }
      else {
        soc = _hill_b_boot(wir_r, soa, sob, FileC);
        fprintf(stderr, "{test boot: %s, with %s: %x}\n", FileC, FileB,
            u2_mug(soc));
        u2_bl_done(wir_r, kit_r);
      }
      LoomStop = 0;
      u2_bx_show(wir_r);
      return;
    }
  }
  else 
#endif
    if ( !strncmp(lin_c, "w ", 2) ) {
    lin_c += 2; out_c = "w";
  } else if ( !strncmp(lin_c, "t ", 2) ) {
    lin_c += 2; out_c = "t";
  } else if (  !strncmp(lin_c, "d ", 2) ) {
    lin_c += 2; out_c = "d";
  } else if (  !strncmp(lin_c, "e ", 2) ) {
    lin_c += 2; out_c = "e";
  } else if (  !strncmp(lin_c, "p ", 2) ) {
    lin_c += 2; out_c = "p";
  } else if (  !strncmp(lin_c, "y ", 2) ) {
    lin_c += 2; out_c = "y";
  }
  else out_c = 0;

  {
    u2_ray kit_r = u2_bl_open(wir_r);

    if ( u2_bl_set(wir_r) ) {
      u2_bl_done(wir_r, kit_r);
      fprintf(stderr, "{exit}\n");
    } else {
      _hill_a_fire(wir_r, soa, sob, lin_c, out_c);
      // _hill_b_fire(wir_r, soa, sob, soc, lin_c, out_c);

      u2_bl_done(wir_r, kit_r);
    }
  }

  LoomStop = 0;
}
