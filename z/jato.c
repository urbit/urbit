/* z/jato.c
**
** This file is in the public domain.
*/
#include "all.h"

  /** Global data structures.
  **/
    static struct u3_zj_def _zj_defs[] = {
#     define _zj_wet(nam, mug) \
        { #nam, mug, u3_zx_##nam },
#     define _zj_dry(nam, mug) \
        { #nam, mug,  0 },
#include "z/jets.h"
        {}
    };
    static struct u3_zj_def *_zj_list = 0;

/* _zj_add():
**
**   Add (def, cor, pri) to the jet search space.
*/
static void
_zj_add(u3_z     z,
        struct   u3_zj_def *def,
        u3_fox   cor,
        u3_fox   par,
        uint32_t pri)
{
  u3_fox ham = u3_zh(z, cor);
  u3_fox bat = u3_zt(z, cor);

  if ( u3_no == u3_lr_dust(z, ham) ) {
    def->con = 0;
  } else {
    def->con = u3_zt(z, ham);
  }
  if ( 0 == def->mug ) {
    def->mug = u3_lm_mug(z, bat);
    fprintf(stderr, "jet: %s: %x\n", def->nam, def->mug);
  } else {
    if ( def->mug != u3_lm_mug(z, bat) ) {
      fprintf(stderr, "jet: mismatch: %s, %x, %x\n", 
          def->nam, def->mug, u3_lm_mug(z, bat));
      return;
    }
  }
  def->bat = bat;
  def->pri = pri; 

  def->nex = _zj_list;
  _zj_list = def;
}

/* u3_zj_load():
**
**   Load jet by prop and battery.
*/
void
u3_zj_load(u3_z   z,
           u3_fox pup,
           u3_fox cor)
{
  u3_fox            bat = u3_zt(z, cor);
  c3_w              mug = u3_lm_mug(z, bat);
  enum u3_zj_code   sax = u3_zj_look(z, bat);
  struct u3_zj_def  *def;
  u3_fox            par, nam, pri;

  for ( def = _zj_list; def; def = def->nex ) {
    if ( mug == def->mug ) {
      return;
    }
  }

  if ( (sax == u3_zj_code_none) && 
       (u3_yes == u3_lr_trel(z, pup, &par, &nam, &pri) ) )
  {
    uint32_t         i;
    struct u3_zj_def *def;

    for ( i=0; (def = &_zj_defs[i])->nam; i++ ) {
      if ( u3_yes == u3_lr_sing_c(z, (c3_c *)def->nam, nam) ) {
        _zj_add(z, def, cor, par, u3_lr_word(z, 0, pri));
      }
    }
  }
}

/* u3_zj_look():
**
**   Look for a jet match - gate, [[sample context] battery].
*/
enum u3_zj_code 
u3_zj_look(u3_z   z,
           u3_fox bat)
{
#if 0
  return u3_zj_code_none;
#else
  c3_w             mug = u3_lm_mug(z, bat);
  struct u3_zj_def *def;

  for ( def = _zj_list; def; def = def->nex ) {
    if ( mug == def->mug ) {
      return (def->pas ? (def - _zj_defs) : u3_zj_code_none);
    }
  }
  return u3_zj_code_none;
#endif
}

/* u3_zj_bat():
**
**   Return the bat formula for a jet.
*/
u3_fox
u3_zj_bat(u3_z            z,
          enum u3_zj_code code_sax)
{
  return _zj_defs[code_sax].bat;
}

/* u3_zc_tank():
**
**   Raise an unrecoverable exception.  Call with
**
**     c3__exit: true exit detected
**     c3__fail: failure to compute
**     c3__punt: confused, return to soft code
*/
u3_fox 
u3_zc_tank(u3_z    z,
           u3_mote gaz)
{
  longjmp(z->j.jmp_lum, gaz);
  return 0;
}

/* u3_zc_use():
**
**   Exit iff (rat) is none.
*/
u3_fox
u3_zc_use(u3_z   z,
          u3_rat rat)
{
  if ( rat == u3_none ) {
    return u3_zc_tank(z, c3__exit);
  }
  else return rat;
}

/* u3_zj_fire():
**
**   Fire a jet - core, [[sam con] bat].
**
**   Set *pod and/or return error condition:
**
**     0         : jet executed correctly
**     c3__exit: true exit detected
**     c3__fail: failure to compute
**     c3__punt: return to soft code
*/
u3_mote
u3_zj_fire(u3_z            z,
           u3_fox          *pod,
           enum u3_zj_code code_sax,
           u3_fox          cor)
{
  struct u3_zj_def *gof = &_zj_defs[code_sax];
  u3_mote          zec;

#if 0
  u3_fox           ham, sam, con, bat;
  if ( u3_no == u3_lr_cell(z, cor, &ham, &bat) ) {
    printf("punt 1\n");
    return c3__punt;
  }
  else if ( u3_no == u3_lr_sing(z, bat, gof->bat) ) {
    printf("punt 2\n");
    return c3__punt;
  }
  else if ( u3_no == u3_lr_cell(z, ham, &sam, &con) ) {
    printf("punt 3\n");
    return c3__punt;
  }
  else if ( u3_no == u3_lr_sing(z, con, gof->con) ) {
    printf("punt 4\n");
    return c3__punt;
  }
#endif

  {
    if ( (zec = setjmp(z->j.jmp_lum)) ) {
      *pod = u3_none;
      return zec;
    }
    else {
      *pod = gof->pas(z, cor);

      // return ( (z->j.w_opt < jet_gof->w_pry) ? c3__test : 0 );
      return 0;
    }
  }
}

/* u3_zc_bytes():
**
**   Copy (w_a) bytes from (y_b) into an atom on the hat of (l).
*/
u3_fox
u3_zc_bytes(u3_z       z,
            c3_w       w_a,
            const c3_y *y_b)
{
  u3_rat vog = u3_ln_bytes(z, w_a, y_b);

  return (vog == u3_none) ? u3_zc_tank(z, c3__fail) : vog;
}

/* u3_zc_string():
**
**   u3_zc_bytes(z, strlen(c_a), (u3_y *)c_a);
*/
u3_fox
u3_zc_string(u3_z       z,
             const c3_c *c_a)
{
  u3_rat vog = u3_ln_string(z, c_a);

  return (vog == u3_none) ? u3_zc_tank(z, c3__fail) : vog;
}

/* u3_zc_cell(): 
**
**   Produce the cell [a b] on the hat of (z).
*/
u3_fox
u3_zc_cell(u3_z   z,
           u3_fox a,
           u3_fox b)
{
  u3_rat vog = u3_ln_cell(z, a, b);

  return (vog == u3_none) ? u3_zc_tank(z, c3__fail) : vog;
}

/* u3_zc_mp():
**
**   Copy the GMP integer (mp_a) into an atom on the hat of (z).
**   Free (mp_a).
*/
u3_fox
u3_zc_mp(u3_z  z,
         mpz_t mp_a)
{
  u3_rat vog = u3_ln_mp(z, mp_a);

  return (vog == u3_none) ? u3_zc_tank(z, c3__fail) : vog;
}

/* u3_zc_trel(): 
**
**   Produce the trel [a b c] on the hat of (z).
*/
u3_fox
u3_zc_trel(u3_z   z,
           u3_fox a,
           u3_fox b,
           u3_fox c)
{
  u3_rat vog = u3_ln_cell(z, a, b);

  return (vog == u3_none) ? u3_zc_tank(z, c3__fail) : vog;
}

/* u3_zc_words():
**
**   Copy (w_a) words from (w_b) into an atom on the hat of (z).
*/
u3_fox
u3_zc_words(u3_z       z,
            c3_w       w_a,
            const c3_w *w_b)
{
  u3_rat vog = u3_ln_words(z, w_a, w_b);

  return (vog == u3_none) ? u3_zc_tank(z, c3__fail) : vog;
}
