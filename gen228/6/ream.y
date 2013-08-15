/* The boot parser for watt.
**
** This file is in the public domain.
*/

/* Pre-bison prologue.
*/
%{
# include "all.h"

#   define Pt1           k_228__a
#   define Pt2           k_228__a__b
#   define Pt3           k_228__a__b__c
#   define Pt4           k_228__a__b__c__d
#   define Pt5           k_228__a__b__c__d__e
#   define Pt6           k_228__a__b__c__d__e__f

  /* Everything is a noun - no yacc type declarations!
  */
#   define YYSTYPE u2_noun

  /* Our fake scanner.
  */
    struct _u2_scanner {
      u2_ray  wir_r;
      u2_noun cor;
      u2_flag bug;
      u2_noun scan;     /* result - set by parser */

      /* Error escape.
      */
      jmp_buf env;

      /* Per-parse state.
      */
      struct {
        u2_weak tape;   /* if set, character list */
        u2_atom tube;   /* part data (ie, source document) */
        u2_book bowl;   /* parts - (mark sack). */
      } p;
      
      /* Scanning state.
      */
      struct {
        uint32_t token;    /* initial type token, if any */
        c3_w    pb;       /* byte position */
        c3_w    xw_line;  /* current line */
        c3_w    xw_col;   /* current column */
      } s;
    };
#   define ywir_r (scanner->wir_r)

  /* Forward declarations.
  */
    static u2_noun _watt_locate(struct _u2_scanner *, const void *, u2_noun);

  /* Construction macros.
  */
#   define _ycell(a, b)            u2_bc(ywir_r, a, b)
#   define _ytrel(a, b, c)         u2_bt(ywir_r, a, b, c)
#   define _yqual(a, b, c, d)      u2_bq(ywir_r, a, b, c, d)
#   define _yquil(a, b, c, d, e)   u2_bu(ywir_r, a, b, c, d, e)

#   define _ybook(a)               j2_mcc(Pt4, by, gas)(ywir_r, u2_nul, a);
%}

/* Bison directives.
*/
  /* With the mighty power of GLR... 
  */
  %glr-parser
  %name-prefix="y228_"

  /* We laugh at your petty shift-reduce conflicts.
  */
  %expect 83

  %pure-parser
  %locations
  %parse-param {struct _u2_scanner *scanner}
  %lex-param {struct _u2_scanner *scanner}


/* Support routines.
*/
%{
  static u2_noun                                                  //  produce
  _yflop(u2_wire wir_r, 
         u2_noun l)                                               //  submit
  {
    u2_noun f = j2_mbc(Pt2, flop)(wir_r, l);

    u2_rz(wir_r, l);
    return f;
  }

  static u2_noun
  _yfon_byte(struct _u2_scanner *scanner, 
            u2_noun x,
            u2_noun y,
            u2_noun z)
  {
    u2_wire wir_r = scanner->wir_r;
    u2_noun fug = u2_bq(wir_r, x, y, z, u2_nul);
    u2_noun ret = u2_bn_tape(wir_r, fug);

    u2_rz(wir_r, fug);
    return ret;
  }

  static u2_noun                                                  //  produce
  _yfon_far(struct _u2_scanner *scanner,
           u2_noun x)                                             //  submit
  {
    u2_wire wir_r = scanner->wir_r;
    u2_noun cor = scanner->cor;
    u2_noun far = u2_bn_hook(wir_r, cor, "far");
    u2_noun ret = u2_bn_mong(wir_r, far, x);

    u2_rz(wir_r, far);
    return ret;
  }
  static u2_noun                                                  //  produce
  _yfon_fyr(struct _u2_scanner *scanner,
           u2_noun x)                                             //  submit
  {
    u2_wire wir_r = scanner->wir_r;
    u2_noun cor = scanner->cor;
    u2_noun fyr = u2_bn_hook(wir_r, cor, "fyr");
    u2_noun ret = u2_bn_mong(wir_r, fyr, x);

    u2_rz(wir_r, fyr);
    return ret;
  }
  static u2_noun                                                  //  produce
  _yfon_16(struct _u2_scanner *scanner,
          u2_atom x,                                              //  submit
          u2_atom y)                                              //  submit
  {
    return (x << 8) | (x ^ y);
  }
  static u2_noun                                                  //  produce
  _yfon_32(struct _u2_scanner *scanner,
          u2_atom x,                                              //  submit
          u2_atom y)                                              //  submit
  {
    u2_wire wir_r = scanner->wir_r;
    uint32_t z = (x << 16) | (x ^ y);

    return u2_bn_words(scanner->wir_r, 1, &z); 
  }
  static u2_noun                                                  //  produce
  _yfon_list(struct _u2_scanner *scanner,
            u2_noun l)                                            //  submit
  {
    u2_wire wir_r = scanner->wir_r;
    u2_noun p = j2_mbc(Pt2, flop)(wir_r, l);
    u2_noun r = j2_mbc(Pt3, can)(wir_r, 5, p);

    u2_rz(wir_r, p);
    u2_rz(wir_r, l);
    return r;
  }
%}

%%

file 
  : g gene g        { scanner->scan = $2; }
  ;

gene
  : tall  { $$ = _watt_locate(scanner, &@1, $1); }
  | wide  { $$ = _watt_locate(scanner, &@1, $1); }
  ;

wide
  : wide_x
  | wide_x si_cab wide { $$ = _ytrel(c3__clms, $1, $3); }
  /* | wide_x si_sig { $$ = _yqual(c3__clms, $1, c3__smts, c3__null); } */
  ;

wide_x
  : wide_a
  | wide_rope si_col wide
    { $$ = _ytrel(c3__tsgl, $1, $3); }
  ;

wide_a
  : wide_c
  | tok_term si_tis wide
    { $$ = _ytrel(c3__ktts, $1, $3); }
  ;

wide_c
  : wide_hard
  | wide_base
  | wide_rope
  | wide_funk
  | wide_cage
  | wide_call
  | wide_cnsg
  | wide_norm
  | wide_path
  ;

    wide_hard
      : hard      { $$ = _ycell(c3__dtsg, $1); }
      | hard '*'  { $$ = _ycell(c3__dtpt, $1); }
      ;
 
      hard 
        : '0' 'x' tok_chex
          { $$ = _ycell(c3_s2('u', 'x'), $3); }
        | tok_delm
          { $$ = _ycell(c3_s2('u', 'd'), $1); }
        | si_pam
          { $$ = _ycell('f', _0); }
        | si_bar
          { $$ = _ycell('f', _1); }
        | si_mus tok_fon
          { $$ = _ycell(c3_s2('u', 'p'), $2); }
        | si_mus si_sig
          { $$ = _ycell(c3_s2('u', 'p'), 0); }
        | tok_loct
          { $$ = _ycell(c3_s2('t', 'a'), $1); }
        | si_cen tok_term
          { $$ = _ycell(c3_s3('t', 'a', 'm'), $2); }
        | si_cen si_cen
          { $$ = _ycell(c3_s3('t', 'a', 'm'), _0); }
        ;
  
    wide_base
      : si_wut
        { $$ = _ycell(c3__smts, c3__flag); }
      | si_tar
        { $$ = _ycell(c3__smts, c3__noun); }
      | si_ket
        { $$ = _ycell(c3__smts, c3__cell); }
      | si_pat
        { $$ = _ycell(c3__smts, c3__atom); }
      | si_sig
        { $$ = _ycell(c3__smts, c3__null); }
      ;
   
    wide_rope
      : rope            
        { $$ = _ytrel(c3__cnts, $1, _0); }
      ;
    wide_cage
      : si_sel g bank_wide g si_ser 
        { $$ = _ycell(c3__cltr, $3); }
      ; 

    wide_call
      : si_pal g bank_wide g si_par
        { $$ = _ycell(c3__cnms, $3); }
      | si_col si_pal g bank_wide g si_par
        { $$ = _ycell(c3__smcl, $4); }
      ;

    wide_cnsg
      : si_sig si_pal rope w gene w bank_wide si_par
        { $$ = _yqual(c3__cnsg, $3, $5, _ycell(c3__cltr, $7)); }
 
    wide_path
      : si_fas path
        { $$ = _ycell(c3__clsg, _yflop(ywir_r, $2)); }
 
    path
      : thin              { $$ = _ycell($1, u2_nul); }
      | thin si_fas path  { $$ = _ycell($1, $3); }
      ;

    thin
      : tok_term
        { $$ = _ytrel(c3__dtsg, c3_s3('t','a','m'), $1); }
      | si_sel wide si_ser
        { $$ = $2; }
      ;

  /** Wide: funky stuff.
  **/
    wide_funk
      : si_kel g bank_wide g si_ker
        { $$ = _ytrel(c3__hstr, $3, u2_nul); }
      | si_cen si_kel g bank_wide g si_ker
        { $$ = _ytrel(c3__hscn, $4, u2_nul); }
      | si_bar si_pal g bank_wide g si_par
        { $$ = _ycell(c3__wtbr, $4); }
      | si_pam si_pal g bank_wide g si_par
        { $$ = _ycell(c3__wtpm, $4); }
      | rope si_pal rack_wide si_par
        { $$ = _ytrel(c3__cnts, $1, $3); }
      | si_zap wide
        { $$ = _ycell(c3__wtzp, $2); }
      | si_com wide 
        { $$ = _ycell(c3__ktsg, $2); }
      | si_sig si_sel g bank_wide g si_ser
        { $$ = _ytrel(c3__hssg, $4, u2_nul); }
      | si_tar wide
        { $$ = _ycell(c3__cntr, $2); }
      | si_tic wide si_tic wide
        { $$ = _ytrel(c3__ktms, $2, $4); }
      | si_tis si_pal g wide w wide g si_par
        { $$ = _ytrel(c3__dtts, $4, $6); }
      | si_lus si_pal g wide g si_par
        { $$ = _ycell(c3__dtls, $4); }
      ;

  /** Hints.
  **/
    hint
      : tok_term
      | tok_term '.' wide { $$ = _ycell($1, $3); }
      ;

    hont
      : tok_term
      | tok_term '.' wide   { $$ = _ycell($1, $3); }
      | tok_term '.' w gene { $$ = _ycell($1, $4); }
      ;

    huny
      : si_tis         { $$ = 1; } 
      | si_tis huny    { $$ = 1 + $2; }     // XX vulnerable
      ;

    chit_tall
      : f w chit_rack_tall f
        { $$ = $3; }
      | chit_wide
      ;
      chit_rack_tall
        :                                  { $$ = _0; }
        | chit_pair_tall w chit_rack_tall  { $$ = _ycell($1, $3); } 
        ;

      chit_pair_tall
        : si_cen tok_term w gene      { $$ = _ycell($2, $4); } 
        ;

    chit_wide
      : si_pal chit_rack_wide si_par
        { $$ = $2 }
      | si_sig 
        { $$ = u2_nul; }
      ;
      chit_rack_wide
        : g                                       { $$ = _0; }
        | chit_pair_wide g                        { $$ = _ycell($1, _0); }
        | chit_pair_wide si_com g chit_rack_wide  { $$ = _ycell($1, $4); }
        ;
      chit_pair_wide
        : si_cen tok_term w wide      { $$ = _ycell($2, $4); }
        ;

    chop
      : '%' tok_term                  
        { $$ = $2; } 
      | '%' tok_term si_dot tok_delm  
        { $$ = _ycell($2, $4); }
      | '%' tok_term si_col tok_term si_dot tok_delm
        { $$ = _ytrel($2, $4, $6); }
      | '%' tok_term si_col tok_term si_dot tok_delm si_dot tok_delm
        { $$ = _yqual($2, $4, $6, $8); }


  /** Wide: normals.
  **/
    wide_norm: di_hesbar body_i_wide    
              { $$ = _yqual($1, u2_h($2), u2_t($2), u2_nul); }
    wide_norm: di_hessig body_d_wide    { $$ = _ytrel($1, $2, u2_nul); }
    wide_norm: di_hesket body_d_wide    { $$ = _ytrel($1, $2, u2_nul); }
    wide_norm: di_hestis body_b_wide
              { $$ = _yqual($1, u2_h($2), u2_t($2), u2_nul); }
    wide_norm: di_hescen body_d_wide    { $$ = _ytrel($1, $2, u2_nul); }
    wide_norm: di_hespam body_i_wide
              { $$ = _yqual($1, u2_h($2), u2_t($2), u2_nul); }
    wide_norm: di_hestar body_d_wide    { $$ = _ytrel($1, $2, u2_nul); }

    wide_norm: di_wutcol body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_wutdot body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_wutgal body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_wutgar body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_wuttis body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_wutpam body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_wutbar body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_wutsig body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_wutzap body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_wutlus body_m_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_wutmus body_h_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_tislus body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_tismus body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_tisgar body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_tisgal body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_bartis body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barcol body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barmus body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_bardot body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barcen body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barlus body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_bartar body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barzap body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_coltar body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_colsig body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_colmus body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_collus body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_colket body_f_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_dottis body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_dotlus body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_dotwut body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_dottar body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_kettis body_g_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_ketmus body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_ketcol body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_ketgal body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_ketdot body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_ketgar body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_ketsig body_a_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_zaptis body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_zapcom body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_zaphax body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_zapzap body_l_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_zapfas body_l_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_zapcen body_l_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_zapcol body_a_wide    { $$ = $2; scanner->bug = u2_no; }
    wide_norm: di_zapsem body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_sigbar body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigtis body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigmus hint_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sighax body_g_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_siggal hint_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigcol hint_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigdot hint_e_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigket body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_siggar hint_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigcen hint_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_siglus hint_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigpam body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigsig body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_semcol body_i_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_semsig body_i_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_centis body_j_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_centar body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_cenmus body_k_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_cenbar body_p_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_cendot body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_cencol body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_cenlus body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_cenket body_f_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_censig body_q_wide    { $$ = _ycell($1, $2); }

  /** Wide - bodies.
  **/
    body_a_wide: si_pal g wide g si_par
      { $$ = $3; }
    body_b_wide: si_pal g wide w wide g si_par
      { $$ = _ycell($3, $5); }
    body_c_wide: si_pal g wide w wide w wide g si_par
      { $$ = _ytrel($3, $5, $7); }
    body_d_wide: si_pal g bank_wide g si_par
      { $$ = $3; }
    body_f_wide: si_pal g wide w wide w wide w wide g si_par
      { $$ = _yqual($3, $5, $7, $9); }
    body_g_wide: si_pal g term w wide g si_par
      { $$ = _ycell($3, $5); }
    body_h_wide: si_pal g wide w rack_wide si_par
      { $$ = _ycell($3, $5); }
    body_i_wide: si_pal g wide w bank_wide g si_par
      { $$ = _ycell($3, $5); }
    body_j_wide: si_pal g rope w rack_wide si_par
      { $$ = _ycell($3, $5); }
    body_k_wide: si_pal g wide w wide g si_par
      { $$ = _ytrel($3, $5, u2_nul); }
    body_l_wide:
      { $$ = u2_nul; }
    body_m_wide: si_pal g wide w wide w rack_wide si_par
      { $$ = _ytrel($3, $5, $7); }
/*
    body_o_wide: si_pal g wide g si_par
      { $$ = $3; }
*/
    body_p_wide: si_pal g rope w wide w rack_wide si_par
      { $$ = _ytrel($3, $5, $7); }
    body_q_wide: si_pal g rope w wide w wide si_par
      { $$ = _ytrel($3, $5, $7); }

    hint_b_wide: si_pal g hint w wide g si_par
      { $$ = _ycell($3, $5); }


    bank_wide
      : wide             { $$ = _ycell($1, _0); }
      | wide w bank_wide { $$ = _ycell($1, $3); }
      ;

    pair_wide
      : wide w wide      { $$ = _ycell($1, $3); }
      ;
/*
    dish_wide
      : term w wide      { $$ = _ycell($1, $3); }
      ;
*/
    rack_wide
      : g                             { $$ = _0; }
      | pair_wide g                   { $$ = _ycell($1, _0); }
      | pair_wide si_com g rack_wide  { $$ = _ycell($1, $4); }
      ;

  /** Wide - interesting hints.
  **/
    hint_a_wide: si_sel tok_delm w tok_delm si_ser w gene 
      { $$ = _ycell(_ycell($2, $4), $7); }
      
    hint_b_wide: hont w wide
      { $$ = _ycell($1, $3); }

    hint_c_wide
      : si_pal huny w wide si_par  { $$ = _ycell($2, $4); }
      | si_pal wide si_par         { $$ = _ycell(0, $2); }
      ;

    hint_d_wide
      : chop w gene w chit_wide w gene
        { $$ = _yqual($1, $3, $5, $7); }
      ;

    hint_e_wide
      : chop w gene
        { $$ = _ycell($1, $3); }
      ;

tall
  : tall_norm

  /** Tall - normals.
  **/
    tall_norm: di_hesbar w body_hu_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_hesket w body_hw_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_hestis w body_hv_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_hescen w body_hw_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_hespam w body_hu_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_hessig w body_hw_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_hestar w body_hw_tall   { $$ = _ycell($1, $3); }

    tall_norm: di_bartis w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barcol w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barmus w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_bardot w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barcen w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barlus w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_bartar w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barzap w body_b_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_tismus w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_tisgal w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_tisgar w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_tislus w body_b_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_wutbar w body_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_wuttis w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_wutlus w body_m_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_wutmus w body_h_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_wutcol w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_wutgal w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_wutdot w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_wutgar w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_wutpam w body_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_wutsig w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_wutzap w body_a_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_colmus w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_colket w body_f_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_collus w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_colsig w body_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_coltar w body_d_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_dottis w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_dotwut w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_dotlus w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_dottar w body_b_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_kettis w body_g_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_ketmus w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_ketcol w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_ketgal w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_ketdot w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_ketgar w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_ketsig w body_a_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_cenbar w body_p_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_centis w body_j_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_cencol w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_cenmus w body_k_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_cendot w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_cenket w body_f_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_cenlus w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_censig w body_q_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_centar w body_a_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_pamtis w body_ex_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_pamcol w body_ex_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_pammus w body_ex_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_pamdot w body_e_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_pamcen w body_e_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_pamlus w body_ex_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_pamzap w body_ex_tall   { $$ = _ycell($1, $3); }

    tall_norm: di_sigbar w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigtis w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigmus w hint_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sighax w body_g_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sighes w body_g_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigcol w hint_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_siggal w hint_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigdot w hint_e_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigket w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_siggar w hint_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigcen w hint_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_siglus w hint_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigpam w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigsig w body_b_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_semcol w body_i_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_semsig w body_i_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_zapcom w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_zaphax w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_zapcol w body_a_tall    { $$ = $3; scanner->bug = u2_no; }
    tall_norm: di_zaptis w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_zapsem w body_b_tall    { $$ = _ycell($1, $3); }

  /** Tall - bodies.
  **/
    body_a_tall: gene                       { $$ = $1; }
    body_b_tall: gene w gene                { $$ = _ycell($1, $3); }
    body_c_tall: gene w gene w gene         { $$ = _ytrel($1, $3, $5); }
    body_d_tall: bank_tall                  { $$ = $1; }
    body_e_tall: wing                       { $$ = $1; }
    body_ex_tall: gene w wing               { $$ = _ycell($1, $3); }
    body_f_tall: gene w gene w gene w gene  { $$ = _yqual($1, $3, $5, $7); }
    body_g_tall: term w gene                { $$ = _ycell($1, $3); }
    body_h_tall: gene w rack_tall           { $$ = _ycell($1, $3); }
    body_i_tall: gene w bank_tall           { $$ = _ycell($1, $3); }
    body_j_tall: rope w rack_tall           { $$ = _ycell($1, $3); }
    body_k_tall: gene w gene                { $$ = _ytrel($1, $3, u2_nul); }
    body_m_tall: gene w gene w rack_tall    { $$ = _ytrel($1, $3, $5); }
/*
    body_o_tall: gene                       { $$ = $1; }
*/
    body_p_tall: rope w gene w rack_tall    { $$ = _ytrel($1, $3, $5); }
    body_q_tall: rope w gene w gene         { $$ = _ytrel($1, $3, $5); }

    body_hu_tall: gene w tall_star neck     { $$ = _ytrel($1, $3, $4); }
    body_hv_tall: gene w gene w neck        { $$ = _ytrel($1, $3, $5); }
    body_hw_tall: tall_star neck            { $$ = _ycell($1, $2); }

  /** Tall - interesting hints.
  **/
    hint_a_tall: si_sel tok_delm w tok_delm si_ser w gene 
      { $$ = _ycell(_ycell($2, $4), $7); }
      
    hint_b_tall: hont w gene                
      { $$ = _ycell($1, $3); }

    hint_c_tall
      : huny w gene   { $$ = _ycell($1, $3); }
      | gene          { $$ = _ycell(0, $1); }
      ;

    hint_d_tall
      : chop w gene w chit_tall w gene
        { $$ = _yqual($1, $3, $5, $7); }
      ;

    hint_e_tall
      : chop w gene
        { $$ = _ycell($1, $3); }
      ;

  /** Tall - feet and wings.
  **/
    neck
      : f                   { $$ = u2_nul; }
      | '=' '-' w wing      { $$ = $4; }
      ;

    wing
      :  foot_star e        { $$ = _ybook($1); u2_rz(ywir_r, $1); }
      ;

    foot
      : '+' '+' w term w gene
        { $$ = _yqual($4, u2_yes, u2_yes, $6); }
      | '+' '-' w term w gene
        { $$ = _yqual($4, u2_yes, u2_no, $6); }
      | '*' '*' w term w wing
        { $$ = _ytrel($4, u2_no, $6); }
      ;

      foot_star
        :                   { $$ = u2_nul; }
        | foot w foot_star  { $$ = _ycell($1, $3); }
        ;
 
  /** Tall - body parts.
  **/
    bank_tall
      : tall_star f                     { $$ = $1; }
      ;
      tall_star
        :                               { $$ = _0; }
        | gene w tall_star              { $$ = _ycell($1, $3); }
        ;

    rack_tall
      : tall_tall_star f                { $$ = $1; }
      ;
      tall_tall_star
        :                               { $$ = _0; }
        | gene w gene w tall_tall_star  { $$ = _ycell(_ycell($1, $3), $5); }
        ;


  /** Rope: reference path.
  **/
    rope
      : cord                    { $$ = _ycell($1, _0); }
      | cord si_dot g rope      { $$ = _ycell($1, $4); }
      | hess                    { $$ = $1; }
      | hess si_dot g rope      { $$ = j2_mbc(Pt2, weld)(ywir_r, $1, $4); }
      ;

      hess
        : si_hes                { $$ = _ycell(_0, _0); }
        | si_hes hess           { $$ = _ytrel(_0, _ycell(_0, _2), $2); }
        ;
      cord
        : axis                  { $$ = _ycell(_0, $1); }
        | tok_term              { $$ = $1; }
        ;

      axis
        : si_dot                { $$ = _1; }
        | si_dot tok_delm       { $$ = $2; }
        | axis_beto             { $$ = $1; }
        ;
        axis_beto
          : si_mus              { $$ = _2; }
          | si_lus              { $$ = _3; }
          | si_mus axis_galu    { $$ = j2_mbc(Pt3, peg)(ywir_r, _2, $2); }
          | si_lus axis_galu    { $$ = j2_mbc(Pt3, peg)(ywir_r, _3, $2); }
          ;
        axis_galu
          : si_gal              { $$ = _2; }
          | si_gar              { $$ = _3; }
          | si_gal axis_beto    { $$ = j2_mbc(Pt3, peg)(ywir_r, _2, $2); }
          | si_gar axis_beto    { $$ = j2_mbc(Pt3, peg)(ywir_r, _3, $2); }
          ;

  /** Digraphs (with stem)
  **/
    di_wutcol: si_wut si_col  { $$ = c3__wtcl; }
    di_wutdot: si_wut si_dot  { $$ = c3__wtdt; }
    di_wutgar: si_wut si_gar  { $$ = c3__wtgr; }
    di_wutgal: si_wut si_gal  { $$ = c3__wtgl; }
    di_wutpam: si_wut si_pam  { $$ = c3__wtpm; }
    di_wutbar: si_wut si_bar  { $$ = c3__wtbr; }
    di_wutsig: si_wut si_sig  { $$ = c3__wtsg; }
    di_wutzap: si_wut si_zap  { $$ = c3__wtzp; }
    di_wuttis: si_wut si_tis  { $$ = c3__wtts; }
    di_wutlus: si_wut si_lus  { $$ = c3__wtls; }
    di_wutmus: si_wut si_mus  { $$ = c3__wtms; }

    di_hesbar: si_hes si_bar  { $$ = c3__hsbr; }
    di_hesket: si_hes si_ket  { $$ = c3__hskt; }
    di_hestis: si_hes si_tis  { $$ = c3__hsts; }
    di_hescen: si_hes si_cen  { $$ = c3__hscn; }
    di_hespam: si_hes si_pam  { $$ = c3__hspm; }
    di_hessig: si_hes si_sig  { $$ = c3__hssg; }
    di_hestar: si_hes si_tar  { $$ = c3__hstr; }

    di_bartis: si_bar si_tis  { $$ = c3__brts; }
    di_barmus: si_bar si_mus  { $$ = c3__brms; }
    di_barcol: si_bar si_col  { $$ = c3__brcl; }
    di_bardot: si_bar si_dot  { $$ = c3__brdt; }
    di_barcen: si_bar si_cen  { $$ = c3__brcn; }
    di_barlus: si_bar si_lus  { $$ = c3__brls; }
    di_bartar: si_bar si_tar  { $$ = c3__brtr; }
    di_barzap: si_bar si_zap  { $$ = c3__brzp; }

    di_tisgal: si_tis si_gal  { $$ = c3__tsgl; }
    di_tisgar: si_tis si_gar  { $$ = c3__tsgr; }
    di_tislus: si_tis si_lus  { $$ = c3__tsls; }
    di_tismus: si_tis si_mus  { $$ = c3__tsms; }
 
    di_coltar: si_col si_tar  { $$ = c3__cltr; }
    di_colsig: si_col si_sig  { $$ = c3__clsg; }
    di_colmus: si_col si_mus  { $$ = c3__clms; }
    di_collus: si_col si_lus  { $$ = c3__clls; }
    di_colket: si_col si_ket  { $$ = c3__clkt; }

    di_dottis: si_dot si_tis  { $$ = c3__dtts; }
    di_dotlus: si_dot si_lus  { $$ = c3__dtls; }
    di_dotwut: si_dot si_wut  { $$ = c3__dtwt; }
    di_dottar: si_dot si_tar  { $$ = c3__dttr; }
   
    di_kettis: si_ket si_tis  { $$ = c3__ktts; }
    di_ketmus: si_ket si_mus  { $$ = c3__ktms; }
    di_ketcol: si_ket si_col  { $$ = c3__ktcl; }
    di_ketgal: si_ket si_gal  { $$ = c3__ktgl; }
    di_ketdot: si_ket si_dot  { $$ = c3__ktdt; }
    di_ketgar: si_ket si_gar  { $$ = c3__ktgr; }
    di_ketsig: si_ket si_sig  { $$ = c3__ktsg; }

    di_centis: si_cen si_tis  { $$ = c3__cnts; }
    di_centar: si_cen si_tar  { $$ = c3__cntr; }
    di_cenbar: si_cen si_bar  { $$ = c3__cnbr; }
    di_censig: si_cen si_sig  { $$ = c3__cnsg; }
    di_cenmus: si_cen si_mus  { $$ = c3__cnms; }
    di_cencol: si_cen si_col  { $$ = c3__cncl; }
    di_cendot: si_cen si_dot  { $$ = c3__cndt; }
    di_cenlus: si_cen si_lus  { $$ = c3__cnls; }
    di_cenket: si_cen si_ket  { $$ = c3__cnkt; }

    di_pamtis: si_pam si_tis  { $$ = c3__pmts; }
    di_pammus: si_pam si_mus  { $$ = c3__pmms; }
    di_pamcol: si_pam si_col  { $$ = c3__pmcl; }
    di_pamdot: si_pam si_dot  { $$ = c3__pmdt; }
    di_pamcen: si_pam si_cen  { $$ = c3__pmcn; }
    di_pamlus: si_pam si_lus  { $$ = c3__pmls; }
    di_pamzap: si_pam si_zap  { $$ = c3__pmzp; }

    di_semcol: si_sem si_col  { $$ = c3__smcl; }
    di_semsig: si_sem si_sig  { $$ = c3__smsg; }

    di_sigtis: si_sig si_tis  { $$ = c3__sgts; }
    di_sigbar: si_sig si_bar  { $$ = c3__sgbr; }
    di_sigmus: si_sig si_mus  { $$ = c3__sgms; }
    di_sighax: si_sig si_hax  { $$ = c3__sghx; }
    di_sighes: si_sig si_hes  { $$ = c3__sghs; }
    di_sigcol: si_sig si_col  { $$ = c3__sgcl; }
    di_siggal: si_sig si_gal  { $$ = c3__sggl; }
    di_sigdot: si_sig si_dot  { $$ = c3__sgdt; }
    di_sigket: si_sig si_ket  { $$ = c3__sgkt; }
    di_siggar: si_sig si_gar  { $$ = c3__sggr; }
    di_sigcen: si_sig si_cen  { $$ = c3__sgcn; }
    di_siglus: si_sig si_lus  { $$ = c3__sgls; }
    di_sigpam: si_sig si_pam  { $$ = c3__sgpm; }
    di_sigsig: si_sig si_sig  { $$ = c3__sgsg; }

    di_zaptis: si_zap si_tis  { $$ = c3__zpts; }
    di_zapcom: si_zap si_com  { $$ = c3__zpcm; }
    di_zaphax: si_zap si_hax  { $$ = c3__zphx; }
    di_zapcen: si_zap si_cen  { $$ = c3__zpcn; }
    di_zapsem: si_zap si_sem  { $$ = c3__zpsm; }
    di_zapfas: si_zap si_fas  { $$ = c3__zpfs; }
    di_zapzap: si_zap si_zap  { $$ = c3__zpzp; }
    di_zapcol: si_zap si_col  { $$ = c3__zpdg; scanner->bug = u2_yes; }
 
  /* Signs.
  */
    si_wut: '?'
    si_bar: '|'
    si_soc: '\''
    si_cab: '_'
    si_com: ','
    si_hax: '#'
    si_col: ':'
    si_gal: '<'
    si_dot: '.'
    si_kel: '{'
    si_hes: '$'
    si_ket: '^'
    si_gar: '>'
    si_tis: '='
    si_sem: ';'
    si_pal: '('
    si_cen: '%'
    si_ser: ']'
    si_sel: '['
    si_par: ')'
    si_lus: '+'
    si_pam: '&'
    si_mus: '-'
    si_tar: '*'
    si_bas: '\\'
    si_ker: '}'
    si_sig: '~'
    si_tic: '`'
    /* si_doc: '"' */
    si_fas: '/'
    si_pat: '@'
    si_zap: '!'

  /** Basic tokens.
  **/
    term
      : tok_term
      | si_hes    { $$ = _0; }
      ;

    tok_term
      : tok_term_pre
      | tok_term_pre tok_term_load
        { u2_noun a = _ycell($1, $2);
          $$ = u2_bn_tape(ywir_r, a); 
          u2_rz(ywir_r, a); }
      ;
        tok_term_pre
          : ca

        tok_term_load
          : ca { $$ = _ycell($1, _0); }
                 | cd { $$ = _ycell($1, _0); }
                 | '-' { $$ = _ycell($1, _0); }
                 | ca tok_term_load  { $$ = _ycell($1, $2); }
                 | cd tok_term_load  { $$ = _ycell($1, $2); }
                 | '-' tok_term_load { $$ = _ycell($1, $2); }
                 ;

    tok_chex 
      : '0' 
        { $$ = _0; }
      | tok_chex_pre tok_chex_load
        { u2_noun a = _ycell($1, $2);
          $$ = u2_bn_heximal(ywir_r, a); 
          u2_rz(ywir_r, a); }
      ;
        tok_chex_pre
          : cn | ch
          ;

        tok_chex_load
          : { $$ = _0; }
                    | cd gap tok_chex_load { $$ = _ycell($1, $3); }
                    | ch gap tok_chex_load { $$ = _ycell($1, $3); }
                    ;


    tok_delm
      : '0' { $$ = _0; }
      | tok_delm_pre tok_delm_load
        { 
          u2_noun a = _ycell($1, $2); 
          $$ = u2_bn_decimal(ywir_r, a); 
          u2_rz(ywir_r, a);
        }
      ;
        tok_delm_pre: cn;
        tok_delm_load: { $$ = _0; }
                    | cd tok_delm_load { $$ = _ycell($1, $2); }
                    ;
    tok_fonq
      : ca ca ca 
        { $$ = _yfon_byte(scanner, $1, $2, $3); }

    tok_far: tok_fonq           { $$ = _yfon_far(scanner, $1); }
    tok_fyr: tok_fonq           { $$ = _yfon_fyr(scanner, $1); }
    tok_ff: tok_far tok_fyr     { $$ = _yfon_16(scanner, $1, $2); }
    tok_fff: tok_far '-' tok_ff { $$ = _yfon_32(scanner, $1, $3); }
    tok_ffff: tok_ff '-' tok_ff { $$ = _yfon_32(scanner, $1, $3); }

    tok_fon
      : tok_fpre tok_frest
        { $$ = _yfon_list(scanner, u2_bc(ywir_r, u2_bc(ywir_r, 1, $1), $2)); }

    tok_fpre
      : si_bar { $$ = 0; }
      | tok_far
      | tok_ff
      | tok_fff
      | tok_ffff
      ;

    tok_frest
      : '-' '-' tok_ffff tok_frest
        { $$ = u2_bc(ywir_r, u2_bc(ywir_r, 1, $3), $4); }
      | { $$ = u2_nul; }
      ;

    tok_loct
      : si_soc loct_mid si_soc
        { $$ = u2_bn_tape(ywir_r, $2); u2_rz(ywir_r, $2); }
      ;
        loct_mid: { $$ = _0; }
                 | cq gap loct_mid { $$ = _ycell($1, $3); }
                 ;


  /** Whitespace.
  **/
    gap
      : si_bas g si_fas  { $$ = _0; }
      |                  { $$ = _0; }
      ;

    g:            { $$ = _0; }
     | cw g       { $$ = _0; }
     | comment g  { $$ = _0; }
     ;

    w: cw         { $$ = _0; }
     | comment    { $$ = _0; }
     | cw w       { $$ = _0; }
     | comment w  { $$ = _0; }
     ;

    e: '-' '-'
        ;

    f: '=' '='
        ;

    comment: ':' ':' comment_body '\n' { $$ = _0; }
           ;
      comment_body
        : 
        | cl comment_body
        ;

  /** Characters and character classes.
  **/
    ca
      : 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'
      | 'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z'
      | 'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'
      | 'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z'
      ;

    ch: 'a'|'b'|'c'|'d'|'e'|'f';
    cd: '0'|cn;
    cn: '1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9';

    cp: cm|'\''|'\\';

    cm
      : '~'|'`'|'!'|'@'|'#'|'$'|'%'|'^'|'&'|'*'|'('|')' 
      | '{'|'['|'}'|']'|'|'|':'|';'|'"'|'<'|'>' 
      | ','|'.'|'?'|'/'|'_'|'-'|'+'|'='
      ;

    cw: ' ' | '\n';
    cl: ca | cd | cp | ' ';
    cq: ca | cd | cm | ' ' 
      | '\\' '\\' { $$ = $2; }
      | '\\' '\'' { $$ = $2; }
      ;

%%

/* Annotate (gene) with spot.
*/
static u2_noun
_watt_locate(struct _u2_scanner *scanner,
             const void *vlocp,
             u2_noun gene)
{
  u2_wire wir_r = scanner->wir_r;
  const YYLTYPE *llocp = vlocp;   /* bufalo estupido */

  if ( u2_no == scanner->bug ) {
    return gene;
  } else {
    return u2_bt
      (wir_r,
       c3__zpcb,
       u2_bt
        (wir_r, 
         u2_nul,
         u2_bc
          (wir_r, 
           (llocp->first_line), 
           (llocp->first_column)),
         u2_bc
          (wir_r,
           (llocp->last_line), 
           (llocp->last_column))),
        gene);
  }
}

/* Initialize (scanner) for (sack).
*/
static void
_scanner_init_sack(struct _u2_scanner *scanner,
                   u2_ray  wir_r,
                   u2_noun cor,
                   u2_noun sack)
{
  scanner->wir_r = wir_r;
  scanner->scan = u2_none;
  scanner->cor = cor;
  scanner->bug = u2_no;
  scanner->s.token = 0;
  scanner->s.pb = 0;

  scanner->p.tape = u2_none;
  scanner->s.xw_line = 1;
  scanner->s.xw_col = 1;

  if ( u2_yes == u2_stud(sack) ) {
    scanner->p.tube = sack;
    scanner->p.bowl = _0;
  }
  else {
    scanner->p.tube = u2_h(sack);
    scanner->p.bowl = u2_t(sack);
  }
}

/* Initialize (scanner) for (clip).
*/
static void
_scanner_init_clip(struct _u2_scanner *scanner,
                   u2_ray  wir_r,
                   u2_noun cor,
                   u2_noun clip)
{
  scanner->wir_r = wir_r;
  scanner->scan = u2_none;
  scanner->cor = cor;
  scanner->bug = u2_no;
  scanner->s.token = 0;
  scanner->s.pb = 0;

  if ( u2_no == u2_dust(clip) ) {
    u2_bl_bail(wir_r, c3__fail);
  }
  else {
    u2_noun hair = u2_h(clip);
    u2_noun tape = u2_t(clip);

    scanner->p.tape = tape;
 
    if ( (u2_no == u2_stud(u2_h(hair))) || (u2_no == u2_stud(u2_t(hair))) ) {
      u2_bl_bail(wir_r, c3__fail);
    }
    else {
      scanner->s.xw_line = u2_word(0, u2_h(hair));
      scanner->s.xw_col = u2_word(0, u2_t(hair));
    }
  }
}

/* functions
*/
  u2_noun                                                         //  transfer
  j2_mby(Pt6, ream)(u2_ray  wir_r, 
                    u2_noun cor,
                    u2_noun txt)                                  //  retain
  {
    struct _u2_scanner scanner;

    _scanner_init_sack(&scanner, wir_r, cor, txt);

    if ( 0 != setjmp(scanner.env) ) {
      fprintf(stderr, "ream error: %d, %d\n", 
                      scanner.s.xw_line, scanner.s.xw_col);
      return u2_bl_bail(wir_r, c3__fail);
    }
    else {
      if ( !y228_parse(&scanner) ) {
        return scanner.scan;
      }
      else {
        return u2_bl_bail(wir_r, c3__fail);
      }
    }
  }
  u2_noun                                                         //  transfer
  j2_mby(Pt6, vest)(u2_wire wir_r, 
                    u2_noun cor,
                    u2_noun tub)                                  //  retain
  {
    struct  _u2_scanner scanner;
    u2_noun hor;

    _scanner_init_clip(&scanner, wir_r, cor, tub);

    if ( 0 != setjmp(scanner.env) ) {
      hor = u2_bc(wir_r, scanner.s.xw_line, scanner.s.xw_col);

      return u2_bc(wir_r, hor, u2_nul);
    }
    else {
      if ( !y228_parse(&scanner) ) {
        hor = u2_bc(wir_r, scanner.s.xw_line, scanner.s.xw_col);

        return u2_bt(wir_r, u2_rx(wir_r, hor), 
                            u2_nul,
                            u2_bt(wir_r, scanner.scan, hor, u2_nul));
      }
      else {
        return u2_bl_bail(wir_r, c3__fail);
      }
    }
  }
  u2_noun                                                         //  transfer
  j2_mb(Pt6, ream)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun txt;

    if ( u2_none == (txt = u2_frag(u2_cw_sam, cor)) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt6, ream)(wir_r, cor, txt);
    }
  }
  u2_noun
  j2_mb(Pt6, vest)(u2_wire wir_r,
                   u2_noun cor)
  {
    u2_noun tub;

    if ( u2_none == (tub = u2_frag(u2_cw_sam, cor)) ) {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mby(Pt6, vest)(wir_r, cor, tub);
    }
  }

/* structures
*/
# define Tier6_y  u2_jet_live
// # define Tier6_y  u2_jet_dead
// # define Tier6_y  (u2_jet_live | u2_jet_test)

  u2_ho_jet 
  j2_mbj(Pt6, ream)[] = { 
    { ".3", 
       c3__hevy, 
       j2_mb(Pt6, ream), 
       Tier6_y,
       u2_none, u2_none },
    { }
  };

  u2_ho_jet 
  j2_mbj(Pt6, vest)[] = { 
    { ".3", 
       c3__hevy, 
       j2_mb(Pt6, vest), 
       Tier6_y,
       u2_none, u2_none },
    { }
  };

/* Trivial scanner.
*/
int 
y228_lex(YYSTYPE *lvalp, YYLTYPE *llocp, struct _u2_scanner *scanner)
{
  if ( scanner->s.token ) {
    int token = scanner->s.token;

    scanner->s.token = 0;
    return token;
  }
  else {
    c3_y xb;

    if ( u2_none != scanner->p.tape ) {
      if ( u2_no == u2_dust(scanner->p.tape) ) {
        *lvalp = 0;
        return 0;
      }
      else {
        u2_noun b = u2_h(scanner->p.tape);

        if ( b > 255 ) {
          return y228_error(llocp, scanner, "tape error");
        }
        else {
          xb = b;
          scanner->p.tape = u2_t(scanner->p.tape);
        }
      }
    }
    else xb = u2_byte(scanner->s.pb, scanner->p.tube);

    llocp->first_line = llocp->last_line = scanner->s.xw_line;
    llocp->first_column = llocp->last_column = scanner->s.xw_col;

    scanner->s.pb += 1;
    if ( xb == '\n' ) {
      scanner->s.xw_line += 1;
      scanner->s.xw_col = 1;
    }
    else {
      scanner->s.xw_col += 1;
    }

    *lvalp = xb;
    return xb;
  }
}  

/* Error stub.
*/
int y228_error(YYLTYPE *llocp, struct _u2_scanner *scanner, char const *msg)
{
#if 0
  printf("%s: (%d:%d - %d:%d)\n", 
    msg, llocp->first_line, llocp->first_column,
         llocp->last_line, llocp->last_column);
#endif
  scanner->s.xw_line = llocp->first_line;
  scanner->s.xw_col = llocp->first_column;

  longjmp(scanner->env, 1);
}
