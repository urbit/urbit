/* z/public.c
**
** This file is in the public domain.
*/
#include <sys/stat.h>
#include <fcntl.h>
#include <gmp.h>
#include "all.h"
#include "z/public.h"

/* uz_l_boot():
**
**   Create a zeno machine, mallocing (1 << siz) 32-bit words.
**   Set state to the boot kernel.
**
**   Return 0 if malloc fails.
*/
uz_machine
uz_l_boot(uint8_t siz)
{
  uz_machine mac;
  u3_b_init();

  mac = malloc(sizeof *mac);
  memset((void *)mac, 0, sizeof *mac);

  mac->zen = u3_z_new(siz);
 
  // [[%cube 0] 0]
  //
  mac->har = uz_k_cell(mac, uz_k_cell(mac, uz_s4('c','u','b','e'), 0), 0);

  return mac;
}

/* uz_l_free():
**
**   Free a zeno machine.
*/
void
uz_l_free(uz_machine mac)
{
  free(mac->zen);
  free(mac);
}

/* uz_l_except():
**
**   Set an exception target.  Called with uz_s4('t','a','n','k')
**   or uz_s4('e','x','i','t').  (env) must be live while engine runs!
*/
void
uz_l_except(uz_machine mac,
            jmp_buf    env)
{
  memcpy(mac->env, env, sizeof(jmp_buf));
}

/* uz_x_exit(): 
**
**   Report formal failure (undefined computation).
*/
uz_noun
uz_x_exit(uz_machine mac)
{
  longjmp(mac->env, c3__exit);
}

/* uz_x_tank():
**
**   Report informal failure (uncalculated computation).
*/
uz_noun
uz_x_tank(uz_machine mac)
{
  printf("tank!\n");
  abort();
  longjmp(mac->env, c3__tank);
}

/* uz_x_trip():
**
**   Report informal failure (internal error).
*/
uz_noun
uz_x_trip(uz_machine mac)
{
  longjmp(mac->env, c3__trip);
}

/* uz_x_stub():
**
**   Report informal failure (uncalculated computation).
*/
uz_noun
uz_x_stub(uz_machine mac)
{
  printf("stub!\n");
  abort();
  longjmp(mac->env, c3__stub);
}

/* uz_k_nock():
**
**   Execute nock directly.
**
**    sub: subject
**    fol: formula
*/
uz_noun
uz_k_nock(uz_machine mac,
          uz_noun    sub,
          uz_noun    fol)
{
  u3_rat dez = u3_ln_nock(mac->zen, sub, fol);

  if ( dez == u3_none ) {
    return uz_x_exit(mac);
  }
  else return dez;
}

/* uz_k_cell(), uz_k_trel(), uz_k_qual(), uz_k_quil():
**
**   Create a tuple.
*/
uz_noun
uz_k_cell(uz_machine mac,
          uz_noun    a,
          uz_noun    b)
{
  return u3_ln_cell(mac->zen, a, b);
}

uz_noun
uz_k_trel(uz_machine mac,
          uz_noun    a,
          uz_noun    b,
          uz_noun    c)
{
  return u3_ln_trel(mac->zen, a, b, c);
}

uz_noun
uz_k_qual(uz_machine mac,
          uz_noun    a,
          uz_noun    b,
          uz_noun    c,
          uz_noun    d)
{
  return u3_ln_cell(mac->zen, a, u3_ln_trel(mac->zen, b, c, d));
}

uz_noun
uz_k_quil(uz_machine mac,
          uz_noun    a,
          uz_noun    b,
          uz_noun    c,
          uz_noun    d,
          uz_noun    e)
{
  return u3_ln_trel(mac->zen, a, b, u3_ln_trel(mac->zen, c, d, e));
}

/* uz_k_string():
**
**   Create a string atom.
*/
uz_noun
uz_k_string(uz_machine mac,
            const char *str)
{
  return u3_ln_string(mac->zen, str);
}

/* uz_words():
**
**   Create a word atom, LSW first.
*/
uz_noun
uz_k_words(uz_machine mac,
           uint32_t   len,
           uint32_t   *mem)
{
  return u3_ln_words(mac->zen, len, mem);
}

/* uz_k_bytes():
**
**   Create a byte atom, LSB first.
*/
uz_noun
uz_k_bytes(uz_machine mac,
           uint32_t   len,
           uint8_t    *mem)
{
  return u3_ln_bytes(mac->zen, len, mem);
}

/* uz_k_mp():
**
**   Create an atom from a GMP integer.  Free the GMP.
*/
uz_noun
uz_k_mp(uz_machine mac,
        mpz_t      amp)
{
  return u3_ln_mp(mac->zen, amp);
}

/* uz_k_file():
**
**  Load the Unix file [unx] as an atom.
*/
uz_noun
uz_k_file(uz_machine mac,
          uz_noun    unx)
{
  uint32_t len = u3_lr_bin(mac->zen, 3, unx);
  uint8_t  *pat = alloca(len + 1);

  u3_lr_bytes(mac->zen, 0, len, pat, unx);
  pat[len] = 0;
  {
    int         fid = open((char *)pat, O_RDONLY, 0666);
    struct stat sta;
    uint32_t    siz;
    uint8_t     *buf;
    uz_noun     dat; 

    if ( (fid < 0) || (fstat(fid, &sta) < 0) ) {
      perror((char *)pat);
      return uz_x_trip(mac);
    }

    siz = sta.st_size;
    buf = malloc(sta.st_size);

    if ( siz != read(fid, buf, siz) ) {
      perror((char *)pat);
      return uz_x_trip(mac);
    }
    close(fid);

    dat = uz_k_bytes(mac, siz, buf);
    free(buf);

    return dat;
  }
}

/* uz_n_tap():
**
**   True (1) iff [a] is a cell.
*/
uint8_t
uz_n_tap(uz_machine mac,
         uz_noun    a)
{
  return !u3_lr_dust(mac->zen, a);
}

/* uz_n_eq():
**
**   True (1) iff [a] and [b] are the same noun.
*/
uint8_t
uz_n_eq(uz_machine mac,
        uz_noun    a,
        uz_noun    b)
{
  return !u3_lr_sing(mac->zen, a, b);
}

/* uz_n_mug():
**
**   Return the 31-bit cached insecure hash of [a].
*/
uint32_t
uz_n_mug(uz_machine mac,
         uz_noun    a)
{
  return u3_lm_mug(mac->zen, a);
}

/* uz_c_head(): 
**
**  Extract head.
*/
uz_noun
uz_c_head(uz_machine mac,
          uz_noun    a)
{
  return u3_lr_h(mac->zen, a);
}

/* uz_c_tail(): 
**
**  Extract tail.
*/
uz_noun
uz_c_tail(uz_machine mac,
          uz_noun    a)
{
  return u3_lr_t(mac->zen, a);
}

/* uz_c_cell():
**
**   True (1) iff [a] is a cell [*b *c].
*/
uint8_t
uz_c_cell(uz_machine mac,
          uz_noun    a,
          uz_noun    *b,
          uz_noun    *c)
{
  return !u3_lr_cell(mac->zen, a, b, c);
}

/* uz_c_trel():
**
**   True (1) iff [a] is a triple [*b *c *d].
*/
uint8_t
uz_c_trel(uz_machine mac,
          uz_noun    a,
          uz_noun    *b,
          uz_noun    *c,
          uz_noun    *d)
{
  return !u3_lr_trel(mac->zen, a, b, c, d);
}

/* uz_c_qual():
**
**   True (1) iff [a] is a quadruple [*b *c *d *e].
*/
uint8_t
uz_c_qual(uz_machine mac,
          uz_noun    a,
          uz_noun    *b,
          uz_noun    *c,
          uz_noun    *d,
          uz_noun    *e)
{
  return !u3_lr_qual(mac->zen, a, b, c, d, e);
}

/* uz_c_quil():
**
**   True (1) iff [a] is a quintuple [*b *c *d *e *f].
*/
uint8_t
uz_c_quil(uz_machine mac,
          uz_noun    a,
          uz_noun    *b,
          uz_noun    *c,
          uz_noun    *d,
          uz_noun    *e,
          uz_noun    *f)
{
  return !u3_lr_quil(mac->zen, a, b, c, d, e, f);
}

/* uz_c_p():
**
**   True (1) iff [a] is of the form [b *c].
*/
uint8_t
uz_c_p(uz_machine mac,
       uz_noun    a,
       uz_noun    b,
       uz_noun    *c)
{
  return !u3_lr_p(mac->zen, a, b, c);
}

/* uz_c_pq():
**
**   True (1) iff [a] is of the form [b *c *d].
*/
uint8_t
uz_c_pq(uz_machine mac,
        uz_noun    a,
        uz_noun    b,
        uz_noun    *c,
        uz_noun    *d)
{
  return !u3_lr_pq(mac->zen, a, b, c, d);
}

/* uz_c_pqr():
**
**   True (1) iff [a] is of the form [b *c *d *e].
*/
uint8_t
uz_c_pqr(uz_machine mac,
         uz_noun    a,
         uz_noun    b,
         uz_noun    *c,
         uz_noun    *d,
         uz_noun    *e)
{
  return !u3_lr_pqr(mac->zen, a, b, c, d, e);
}

/* uz_a_mp():
**
**   Copy [b] into (a).
*/
void
uz_a_mp(uz_machine mac,
        mpz_t      a,
        uz_noun    b)
{
  u3_lr_mp(mac->zen, a, b);
}

/* uz_a_bin(): 
**
**   Return the size of [b] in bits, rounded up to
**   (1 << a). 
**
**   For example, (a == 3) returns the size in bytes.
*/
uint32_t
uz_a_bin(uz_machine mac,
         uint8_t    a,
         uz_noun    b)
{
  return u3_lr_bin(mac->zen, a, b);
}

/* uz_a_word():
**
**   Return word (a) of [b].
*/
  uint32_t
  uz_a_word(uz_machine mac,
            uint32_t    a,
            uz_noun    b);

/* uz_a_words():
**
**  Copy words (a) through (a + b - 1) from (d) to (c).
*/
  void
  uz_a_words(uz_machine mac,
             uint32_t   a,
             uint32_t   b,
             uint32_t   *c,
             uz_noun    d);

/* uz_a_bytes():
**
**  Copy bytes (a) through (a + b - 1) from (d) to (c).
*/
void
uz_a_bytes(uz_machine mac,
           uint32_t   a,
           uint32_t   b,
           uint8_t    *c,
           uz_noun    d)
{
  return u3_lr_bytes(mac->zen, a, b, c, d);
}

/* uz_a_file():
**
**  Copy [dat] into the Unix file [unx].
*/
  void
  uz_a_file(uz_machine mac,
            uz_noun    unx,
            uz_noun    dat);


/** Typed computation.
***
***   Functions labeled _g_ generate a clam - [type noun].
***   Functions labeled _r_ replace the subject with the clam.
***   Functions labeled _p_ post the clam on the subject.
**/

/* _uz_g_run_gene():
**
**   Perform a typed computation - running [typ som] through
**   [gen], producing a cell [type noun].
**
**   If (ben) is 1, print benchmark.
*/
static uz_noun
_uz_g_run_gene(uz_machine mac,
               uz_noun    typ,
               uz_noun    som,
               uz_noun    gen,
               uint8_t    ben)
{
  uz_noun cam = uz_t_full(mac, typ, gen);
  uz_noun val;

#if 1
  uz_noun res;
  struct u3_z_bench naq;

  memset(&naq, 0, sizeof(naq));
  res = u3_z_run(mac->zen, &val, som, uz_ct(mac, cam), &naq);

  if ( ben ) {
    printf(" <%lld steps, %d words>\n",
            naq.d_ruy,
            (naq.w_maz - naq.w_vil) + (naq.w_buc - naq.w_tew));
  }

  switch ( res ) {
    case 0: return uz_k_cell(mac, uz_ch(mac, cam), val);

    case c3__exit: return uz_x_exit(mac);
    case c3__trip: return uz_x_trip(mac);
    case c3__tank: return uz_x_tank(mac);

    default: return uz_x_trip(mac);
  }
#else
  // uz_f_print(mac, "form", uz_ct(mac, cam));
  // uz_f_print(mac, "type", uz_ch(mac, cam));
  // printf("\n");

  val = uz_k_nock(mac, som, uz_ct(mac, cam));
  return uz_k_cell(mac, uz_ch(mac, cam), val);
#endif
}

/* uz_g_lame():
**
**   Compute in a lame way that might work today.  Delete.
*/
uz_noun
uz_g_lame(uz_machine mac,
          uz_noun    fig,
          uz_noun    gen)
{
  uz_noun typ;

  uz_f_print(mac, "gene", gen);
  typ = uz_k_cell
    (mac, uz_s4('c','r','i','b'), 
          uz_k_cell
            (mac, 
             uz_k_cell(mac, 'a', uz_s4('a','t','o','m')),
             0));

  return _uz_g_run_gene(mac, typ, uz_ct(mac, fig), gen, 0);
}

/* uz_g_combine():
**
**   Evaluate a lambda.
*/
uz_noun
uz_g_combine(uz_machine mac);
             
/* uz_g_compute(), uz_r_compute(), uz_p_compute():
**
**   Compute a function of an external argument.
**
**    gen: gene (producing function)
**    fig: clam
*/
uz_noun
uz_g_compute(uz_machine mac, 
             uz_noun    fig,
             uz_noun    gen)
{
  uz_noun dor = uz_g_express(mac, gen);

  // printf("\n\n");
  // uz_f_print_type(mac, "dor: typ", uz_ch(mac, dor));
  // uz_f_print(mac, "dor: nut", uz_ct(mac, dor));

  {
    // A hacky implementation - do type properly.
    //
    uz_noun typ_dor = uz_ch(mac, dor);
    uz_noun som_dor = uz_ct(mac, dor);
    // uz_noun typ_fig = uz_ch(mac, fig);
    uz_noun som_fig = uz_ct(mac, fig); 
    {
      uz_noun som_nex, typ_nex;
      uz_noun cal;
      
      // uz_f_print(mac, "typ_dor", typ_dor);
      // uz_f_print(mac, "som_dor", som_dor); 

      // typ_fig discarded - bad
      //
      som_nex = uz_k_cell
        (mac, 
         uz_k_cell(mac, som_fig, uz_cht(mac, som_dor)),
         uz_ct(mac, som_dor)
        );
      typ_nex = typ_dor;

      // cal: invocation gene
      //
      cal = uz_k_trel(mac, uz_s4('p','o','r','t'), 0, 0);

      return _uz_g_run_gene(mac, typ_nex, som_nex, cal, 1);
    }
  }
}    

/* uz_g_express(), uz_r_express(), uz_p_express():
**
**   Express a gene against the subject.
**
**    gen: gene
*/
uz_noun
uz_g_express(uz_machine mac,
             uz_noun    gen)
{
  return _uz_g_run_gene(mac, uz_ch(mac, mac->har), 
                             uz_ct(mac, mac->har),
                             gen, 
                             1);
}

void
uz_r_express(uz_machine mac,
             uz_noun    gen)
{
  mac->har = _uz_g_run_gene(mac, uz_ch(mac, mac->har), 
                                 uz_ct(mac, mac->har),
                                 gen, 
                                 0);
}

uz_noun
uz_p_express(uz_machine mac,
             uz_noun    gen);

/* uz_p_constant():
**
**   Post a typed constant.
**
**    han: name
**    fig: clam
*/
void
uz_p_constant(uz_machine mac,
              uz_noun    han,
              uz_noun    fig);


/** Built-in functions, typically kernel implemented.
**/

/* uz_t_full(): 
**
**   Compile [type gene] to [type form].
*/
uz_noun
uz_t_full(uz_machine mac,
          uz_noun    typ,
          uz_noun    gen)
{
  u3_mote how;
  u3_rat  rat = u3_b_mill(mac->zen, typ, gen, &how);

  if ( u3_none == rat ) {
    return uz_x_exit(mac);
  }
  else return rat;
}

/* uz_t_make(): 
**
**   Compile [type gene] to [type form].
*/
uz_noun
uz_t_make(uz_machine mac,
          uz_noun    typ,
          uz_noun    gen)
{
  u3_mote how;
  u3_rat  rat = u3_b_mill(mac->zen, typ, gen, &how);

  if ( u3_none == rat ) {
    return uz_x_exit(mac);
  }
  else return uz_ct(mac, rat);
}

/* uz_t_make(): 
**
**   Compile [type gene] to [form].
*/
uz_noun
uz_t_make(uz_machine mac,
          uz_noun    typ,
          uz_noun    gen);

/* uz_t_watt():
**
**   Compile source text to watt gene.
**
**    src: source text, as atom
*/
uz_noun
uz_t_watt(uz_machine mac,
          uz_noun    src)
{
  u3_rat rat = u3_b_read(mac->zen, src);

  if ( u3_none == rat ) {
    return uz_x_exit(mac);
  }
  else return rat;
}
 
/* uz_t_vere():
**
**   Compile source text to vere command.
**
**    src: source text, as atom
*/
uz_noun
uz_t_vere(uz_machine mac,
          uz_noun    src)
{
  u3_rat rat = u3_b_vere(mac->zen, src);

  if ( u3_none == rat ) {
    return uz_x_exit(mac);
  }
  else return rat;
}

/* uz_t_hume():
**
**   Compile source text to untyped noun.
**
**    src: source text, as atom
*/
uz_noun
uz_t_hume(uz_machine mac,
          uz_noun    src)
{
  u3_rat rat = u3_b_hume(mac->zen, src);

  if ( u3_none == rat ) {
    return uz_x_exit(mac);
  }
  else return rat;
}

/** Memory management - for advanced users only.
**/
/* uz_m_depart():
**
**   Depart, returning a shoe.
*/
uz_shoe
uz_m_depart(uz_machine mac)
{
  return u3_lm_flap(mac->zen);
}

/* uz_m_retreat():
**
**   Retreat to saved shoe.
*/
void
uz_m_retreat(uz_machine mac,
             uz_shoe    sho)
{
  u3_lm_flop(mac, sho);
}

/* uz_m_zap():
**
**   Discard all departed storage.
*/
void
uz_m_zap(uz_machine mac)
{
  u3_lm_flog(mac);
}

/* uz_f_print():
**
**   Print [noun], with [caption] if nonzero as a caption.
*/
void
uz_f_print(uz_machine mac,
           const char *cap,
           uz_noun    non)
{
  u3_b_print(mac->zen, cap, non);
}

/* uz_f_print_type():
**
**   Print [noun] as a type, using the type printer.
*/
void
uz_f_print_type(uz_machine mac,
                const char *cap,
                uz_noun    non)
{
  u3_b_print_type(mac->zen, cap, non);
}

#if 0
/* uz_boot():
**
**   Create a zeno machine, mallocing (1 << size) 32-bit words.
**   Return 0 if malloc fails.
**
**   Optimization level: 0 to 15 - 15 is fastest.
*/
uz_machine
uz_boot(unsigned char size,
        unsigned char speed)
{
  //_find_fun();
  u3_b_init();

  return u3_z_new(size, speed);
}

/* uz_free():
**
**   Free a zeno machine.
*/
void
uz_free(uz_machine machine)
{
  free(machine);
}

/* uz_new_string():
**
**   Create a string atom.
*/
uz_noun
uz_new_string(uz_machine machine,
              const char *string)
{
  return u3_ln_string(machine, string);
}

/* uz_print():
**
**   Print [noun], with [caption] if nonzero as a caption.
*/
void
uz_print(uz_machine machine,
         const char *caption,
         uz_noun    noun)
{
  u3_z z = machine;

  return u3_b_print(&z->l, caption, noun);
}

/* uz_line():
**
**   Execute a command line.  Benchmark if (bench) is set.
**
**   *product will be set iff the result is uz_good.
*/
enum uz_code
uz_line(uz_machine      machine,
        uz_noun         *product,
        uz_noun         line,
        struct uz_bench *bench)
{
  u3_z   z   = machine;
  u3_rat wug = line;
  u3_rat jop;
  
  if ( u3_none == wug ) {
    return uz_fail;
  }
  jop = u3_b_read(&z->l, wug);

  if ( u3_none == jop ) {
    return uz_exit;
  }
  else {
#if 0
    u3_fox vad = u3_ln_cell(z, c3__cube, 0);
    u3_rat neb = u3_b_mill(&z->l, jop, vad);

    if ( u3_none == neb ) {
      return u3_none;
    }
    else {
      u3_fox zom = u3_t(z, neb);
      u3_fox hel = u3_z_do(z, 0, zom);
      u3_fox gix;

      u3_b_print(&z->l, "dec", z->q.dec);
      gix = u3_z_run(z, product, hel, z->q.dec);

      switch ( gix ) {
        default: u3_assert(0); return uz_fail;

        case 0:          return uz_good;
        case c3__exit: return uz_exit;
        case c3__fail: return uz_fail;
      }
      return 0;
    }
#else
    /* zul: current mold
    ** heg: current grit
    */
    u3_fox zul = u3_h(z, z->q.tef);
    u3_fox heg = u3_t(z, z->q.tef);
    u3_rat bir = u3_b_mill(&z->l, jop, zul);

    if ( u3_none == bir ) {
      return u3_none;
    }
    else {
      /* muc: line form
      */
      u3_fox            muc = u3_t(z, bir);
      u3_fox            gix;
      struct u3_z_bench naq;

      gix = u3_z_run(z, product, heg, muc, bench ? &naq : 0);

      switch ( gix ) {
        default: u3_assert(0); return uz_fail;

        case c3__exit: return uz_exit;
        case c3__fail: return uz_fail;
        case 0: {
#if 0
            u3_fox            vug;
            vug = u3_ln_nock(z, heg, muc);
            u3_b_print(&z->l, "pure", vug);
            printf("zeno:\n");
          if ( u3_yes == u3_lr_sing(z, vug, *product) ) {
            printf("<good>\n");
          } else printf("<bad>\n");
#endif
          if ( bench ) {
            bench->steps = naq.d_ruy;
            bench->words = (naq.w_maz - naq.w_vil) + 
                           (naq.w_buc - naq.w_tew);
            bench->copies = naq.w_cop;
            bench->kicks = 0;
          }
          return uz_good;
        }
      }
    }
#endif
  }
}
#endif
