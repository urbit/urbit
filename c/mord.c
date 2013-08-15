/* c/mord.c
**
** This file is in the public domain.
*/
#include <setjmp.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <gmp.h>
#include <assert.h>
#include "z/public.h"

  /** Forward declarations.
  **/
    static uz_noun
    _mord_source(uz_machine, uz_noun);

/* _mord_print(): print a clam [type noun]
*/
static void
_mord_print(uz_machine mac,
            const char *cap,
            uz_noun    fig)
{
  uz_f_print_type(mac, cap, uz_ch(mac, fig));
  uz_f_print(mac, cap, uz_ct(mac, fig));
}

/* _mord_source_crib(): load a crib source.
*/
static uz_noun
_mord_source_crib(uz_machine mac,
                  uz_noun    lar)
{
  uz_noun i_lar  = uz_ch(mac, lar);
  uz_noun t_lar  = uz_ct(mac, lar);
  uz_noun pi_lar = uz_ch(mac, i_lar);
  uz_noun qi_lar = uz_ct(mac, i_lar);
  uz_noun fig    = _mord_source(mac, qi_lar);
  uz_noun p_fig  = uz_ch(mac, fig);
  uz_noun q_fig  = uz_ct(mac, fig);
  uz_noun cul;

  if ( uz_n_eq(mac, 0, pi_lar) ) {
    cul = p_fig;
  } else {
    cul = uz_k_trel(mac, uz_s4('f','a','c','e'), pi_lar, p_fig);
  }

  if ( uz_n_eq(mac, 0, uz_ct(mac, lar)) ) {
    return uz_k_cell(mac, cul, q_fig);
  }
  else {
    uz_noun pem   = _mord_source_crib(mac, t_lar);
    uz_noun p_pem = uz_ch(mac, pem);
    uz_noun q_pem = uz_ct(mac, pem);

    return uz_k_cell
      (mac, uz_k_trel(mac, uz_s4('c','e','l','l'), cul, p_pem),
            uz_k_cell(mac, q_fig, q_pem));
  }
}

/* _mord_source_file(): load a file source.
*/
static uz_noun
_mord_source_file(uz_machine mac,
                  uz_noun    unx)
{
  return uz_k_file(mac, unx);
}

/* _mord_source_atom(): load an atom source.
*/
static uz_noun
_mord_source_atom(uz_machine mac,
                  uz_noun    viq)
{
  uz_noun p_viq;

  if ( uz_c_p(mac, viq, uz_s4('p','a','l','t'), &p_viq) ) {
    return uz_k_cell(mac, uz_s4('a','t','o','m'), p_viq);
  }
  else if ( uz_c_p(mac, viq, uz_s4('c','r','a','d'), &p_viq) ) {
    return uz_k_cell(mac, uz_s4('b','l','u','r'), p_viq);
  }
  else if ( uz_c_p(mac, viq, uz_s4('r','o','c','k'), &p_viq) ) {
    return uz_k_cell
      (mac, uz_k_cell(mac, uz_s4('c','u','b','e'), p_viq), p_viq);
  }
  else return uz_x_tank(mac);
}

/* _mord_source_exp(): load an expression source.
*/
static uz_noun
_mord_source_exp(uz_machine mac,
                 uz_noun    gen)
{
  return uz_g_express(mac, gen);
}

/* _mord_source(): load a source.
*/
static uz_noun
_mord_source(uz_machine mac,
             uz_noun    dim)
{
  uz_noun p_dim;

  if ( uz_c_p(mac, dim, uz_s4('m','a','l','g'), &p_dim) ) {
    return _mord_source_crib(mac, p_dim);
  }
  else if ( uz_c_p(mac, dim, uz_s4('d','r','u','n'), &p_dim) ) {
    return _mord_source_file(mac, p_dim);
  }
  else if ( uz_c_p(mac, dim, uz_s4('n','a','r','v'), &p_dim) ) {
    return _mord_source_atom(mac, p_dim);
  }
  else if ( uz_c_p(mac, dim, uz_s4('t','r','i','b'), &p_dim) ) {
    return _mord_source_exp(mac, p_dim);
  }
  else return uz_x_tank(mac);
}

/* _mord_filter_watt_lame(): apply a lame filter.
*/
static uz_noun
_mord_filter_watt_lame(uz_machine mac,
                       uz_noun    fig,
                       uz_noun    gar)
{
  uz_noun src = uz_k_file(mac, gar);
  uz_noun gen = uz_t_watt(mac, src);

  return uz_g_lame(mac, fig, gen);
}

/* _mord_filter_watt_program(): apply a program filter.
*/
static uz_noun
_mord_filter_watt_program(uz_machine mac,
                          uz_noun    fig,
                          uz_noun    fev)
{
  uz_noun src = uz_k_file(mac, fev);
  uz_noun gen = uz_t_watt(mac, src);

  // uz_f_print(mac, "gen", gen);

  return uz_g_compute(mac, fig, gen);
}

/* _mord_filter_watt_exp():
*/
static uz_noun
_mord_filter_watt_exp(uz_machine mac,
                      uz_noun    fig,
                      uz_noun    gen)
{
  return uz_g_compute(mac, fig, gen);
}

/* _mord_filter_nock_exp():
*/
static uz_noun
_mord_filter_nock_exp(uz_machine mac,
                      uz_noun    fig,
                      uz_noun    fol)
{
  return uz_k_cell
    (mac, uz_s4('b','l','u','r'), uz_k_nock(mac, uz_ct(mac, fig), fol));
}

/* _mord_filter_nock_program(): apply a program filter.
*/
static uz_noun
_mord_filter_nock_program(uz_machine mac,
                          uz_noun    fig,
                          uz_noun    nar)
{
  uz_noun src = uz_k_file(mac, nar);
  uz_noun fol = uz_t_hume(mac, src);

  return _mord_filter_nock_exp(mac, fig, fol);
}

/* _mord_filter(): apply a transformation filter.
**
**    fig: [tip nun] - source
**    kal: filter    - transformation filter
*/
static uz_noun
_mord_filter(uz_machine mac,
             uz_noun    fig,
             uz_noun    kal)
{
  uz_noun p_kal;

  if ( uz_c_p(mac, kal, uz_s4('z','e','c','t'), &p_kal) ) {
    return _mord_filter_watt_exp(mac, fig, p_kal);
  }
  else if ( uz_c_p(mac, kal, uz_s4('g','a','m','p'), &p_kal) ) {
    return _mord_filter_watt_program(mac, fig, p_kal);
  }
  else if ( uz_c_p(mac, kal, uz_s4('l','a','m','e'), &p_kal) ) {
    return _mord_filter_watt_lame(mac, fig, p_kal);
  }
  else if ( uz_c_p(mac, kal, uz_s4('b','l','a','n'), &p_kal) ) {
    return _mord_filter_nock_exp(mac, fig, p_kal);
  }
  else if ( uz_c_p(mac, kal, uz_s4('z','o','r','k'), &p_kal) ) {
    return _mord_filter_nock_program(mac, fig, p_kal);
  }
  else return uz_x_tank(mac);
}

/* _mord_transform(): transform a source.
**
**    fig: [tip nun] - source
**    gar: *filter   - transformation pipeline, first to last
*/
static uz_noun
_mord_transform(uz_machine mac,
                uz_noun    fig,
                uz_noun    gar)
{
  if ( uz_n_eq(mac, 0, gar) ) {
    return fig;
  }
  else {
    return _mord_transform
      (mac, _mord_filter(mac, fig, uz_ch(mac, gar)), uz_ct(mac, gar));
  }
}

#if 0
/* _mord_construct(): process construction.  Produces [tip nun].
*/
static uz_noun
_mord_construct(uz_machine mac,
                uz_noun    pel)
{
  return _mord_transform
    (mac, _mord_source(mac, uz_ch(mac, pel)), uz_ct(mac, pel));
}
#endif

/* _mord_command(): process command.
*/
static void
_mord_command(uz_machine mac,
              uz_noun    fex)
{
  uz_shoe sho = uz_m_depart(mac);
  {
    uz_noun lon = uz_g_express(mac, fex);

    _mord_print(mac, 0, lon);
  }
  uz_m_retreat(mac, sho);
  uz_m_zap(mac);
}

/* mord_line(): process command line.
*/
void 
mord_line(uz_machine mac, 
          uz_noun lug)
{
  jmp_buf env;
  uz_noun wef;

  if ( (wef = setjmp(env)) ) {
    uint8_t str[5];

    str[4] = 0;
    uz_a_bytes(mac, 0, 4, str, wef);
    printf("[%s]\n", str);
  }
  else {
    uz_l_except(mac, env);
    {
      uz_noun fex = uz_t_watt(mac, lug);

      _mord_command(mac, fex);
    }
  }
}
  
  /** Dumb scanning and dumping.
  **/
    static void
    _mord_dump_in(uz_machine mac, FILE *fil, uz_noun non);
    static void
    _mord_dump(uz_machine mac, FILE *fil, uz_noun non);
    static uz_noun
    _mord_scan_cell(uz_machine mac, FILE *fil);
    static uz_noun
    _mord_scan(uz_machine mac, FILE *fil);

/* Return true iff (atom) is an ASCII string of (3) or more bytes,
** using no characters besides a-z and -.
*/
static uint8_t
_mord_term(uz_machine mac,
           uz_noun    tat,
           uint32_t   qb)
{
  uint32_t sb = uz_a_bin(mac, 3, tat);

  if ( sb >= qb) {
    uint8_t *xb = alloca(sb);
    uint32_t i;

    uz_a_bytes(mac, 0, sb, xb, tat);

    for ( i=0; i < sb; i++ ) {
      if ( ((xb[i] < 'a') || (xb[i] > 'z')) && (xb[i] != '-') ) {
        return 0;
      }
    }
    return 1;
  }
  else return 0;
}

/* _mord_dump_in(): dump in cell.
*/
static void
_mord_dump_in(uz_machine mac,
              FILE       *fil,
              uz_noun    non)
{
  if ( !uz_n_tap(mac, non) ) {
    _mord_dump(mac, fil, non);
  }
  else {
    _mord_dump(mac, fil, uz_ch(mac, non));
    fprintf(fil, " ");
    _mord_dump_in(mac, fil, uz_ct(mac, non));
  }
}

/* mord_dump(): dump noun to file.
*/
void
_mord_dump(uz_machine mac,
           FILE       *fil,
           uz_noun    non)
{
  if ( !uz_n_tap(mac, non) ) {
    mpz_t amp;

    if ( _mord_term(mac, non, 2) ) {
      uint32_t sb = uz_a_bin(mac, 3, non);
      uint8_t *xb = alloca(sb + 1);

      uz_a_bytes(mac, 0, sb, xb, non);
      xb[sb] = 0;
      fprintf(fil, "%%%s", xb);
    }
    else {
      uz_a_mp(mac, amp, non);
      gmp_fprintf(fil, "%Zd", amp);
      mpz_clear(amp);
    }
  }
  else {
    fputc('[', fil);
    _mord_dump(mac, fil, uz_ch(mac, non));
    fprintf(fil, " ");
    _mord_dump_in(mac, fil, uz_ct(mac, non));
    fputc(']', fil);
  }
}

/* _mord_scan_cell(): scan cell or tuple.
*/
static uz_noun
_mord_scan_cell(uz_machine mac, 
                FILE       *fil)
{
  uz_noun hed = _mord_scan(mac, fil);
  int     c   = fgetc(fil);

  if ( c == ' ' ) {
    uz_noun tal = _mord_scan_cell(mac, fil);

    return uz_k_cell(mac, hed, tal);
  }
  else { 
    assert(c == ']');
    return hed;
  }
}

/* _mord_scan(): scan noun from file.
*/
static uz_noun
_mord_scan(uz_machine mac,
           FILE       *fil)
{
  int c = fgetc(fil);

  if ( c == '[' ) {
    return _mord_scan_cell(mac, fil);
  } 
  else if ( c == '%' )  {
    char buf[1025];

    fscanf(fil, "%1024[a-z-]", buf);
    return uz_k_string(mac, buf);
  }
#if 0
    mpz_t amp;

    mpz_init(amp);
    while ( 1 ) {
      c=fgetc(fil);

      if ( (c == '-') || ((c >= 'a') && (c <= 'z')) ) {
        mpz_mul_ui(amp, amp, 256);
        mpz_add_ui(amp, amp, c);
      }
      else {
        ungetc(c, fil);

        return uz_k_mp(mac, amp);
      }
    }
#endif
  else {
    mpz_t amp;

    ungetc(c, fil);
    mpz_init(amp);
    gmp_fscanf(fil, "%Zd", amp);
    return uz_k_mp(mac, amp);
  }
}

uz_noun
_mord_kernel(uz_machine mac,
             const char *src,
             const char *cax)
{
  FILE *fil;
  uz_noun ker;

  if ( 1 ) {
 //  if ( !(fil = fopen(cax, "r")) ) {
    uz_noun tex = uz_k_file(mac, uz_k_string(mac, src));

    printf("[mord: building kernel]\n");
    ker = uz_t_watt(mac, tex);

#if 0
    if ( !(fil = fopen(cax, "w")) ) {
      perror(cax);
      exit(1);
    }
    _mord_dump(mac, fil, ker);
     printf("[saved: %s]\n", cax);
    fclose(fil);
#endif

    return ker;
  }
  else {
    printf("[loading: %s]\n", cax);

    ker = _mord_scan(mac, fil);
    fclose(fil);
    return ker;
  }
}

/* mord_boot(): boot mord.
*/
void
mord_boot(uz_machine mac)
{
  jmp_buf env;
  uz_noun wef;

  if ( (wef = setjmp(env)) ) {
    uint8_t str[5];

    str[4] = 0;
    uz_a_bytes(mac, 0, 4, str, wef);
    printf("[%s]\n", str);
  }
  else {
    uz_l_except(mac, env);
    {
      uz_noun ker = _mord_kernel(mac, "watt/watt.watt", "watt/298.nock");

      uz_r_express(mac, ker);

      uz_f_print_type(mac, "boot: type", uz_ch(mac, mac->har));
      // uz_f_print(mac, "boot: noun", uz_ct(mac, mac->har));
    }
  }
}
