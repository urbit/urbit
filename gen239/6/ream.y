/* The boot parser for watt.
**
** This file is in the public domain.
*/

/* Pre-bison prologue.
*/
%{
# include "all.h"

#   define Pt1           k_239__a
#   define Pt2           k_239__a__b
#   define Pt3           k_239__a__b__c
#   define Pt4           k_239__a__b__c__d
#   define Pt5           k_239__a__b__c__d__e
#   define Pt6           k_239__a__b__c__d__e__f

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
  %name-prefix="y239_"

  /* We laugh at your petty shift-reduce conflicts.
  */
  %expect 80

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
  | wide_x si_cab wide { $$ = _ytrel(c3__dgsp, $1, $3); }
  /* | wide_x si_sig { $$ = _yqual(c3__dgsp, $1, c3__tmlc, c3__null); } */
  ;

wide_x
  : wide_a
  | wide_rope si_deg wide
    { $$ = _ytrel(c3__lcdl, $1, $3); }
  ;

wide_a
  : wide_c
  | tok_term si_lyc wide
    { $$ = _ytrel(c3__ktlc, $1, $3); }
  ;

wide_c
  : wide_hard
  | wide_base
  | wide_rope
  | wide_funk
  | wide_cage
  | wide_call
  | wide_mtsg
  | wide_norm
  | wide_path
  ;

    wide_hard
      : hard      { $$ = _ycell(c3__dtsg, $1); }
      | hard '*'  { $$ = _ycell(c3__dtwt, $1); }
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
        | si_sep tok_fon
          { $$ = _ycell(c3_s2('u', 'p'), $2); }
        | si_sep si_sig
          { $$ = _ycell(c3_s2('u', 'p'), 0); }
        | tok_loct
          { $$ = _ycell(c3_s2('t', 'a'), $1); }
        | si_mit tok_term
          { $$ = _ycell(c3_s3('t', 'a', 'm'), $2); }
        | si_mit si_mit
          { $$ = _ycell(c3_s3('t', 'a', 'm'), _0); }
        ;
  
    wide_base
      : si_cas
        { $$ = _ycell(c3__tmlc, c3__flag); }
      | si_tar
        { $$ = _ycell(c3__tmlc, c3__noun); }
      | si_ket
        { $$ = _ycell(c3__tmlc, c3__cell); }
      | si_wat
        { $$ = _ycell(c3__tmlc, c3__atom); }
      | si_sig
        { $$ = _ycell(c3__tmlc, c3__null); }
      ;
   
    wide_rope
      : rope            
        { $$ = _ytrel(c3__mtlc, $1, _0); }
      ;
    wide_cage
      : si_den g bank_wide g si_ned 
        { $$ = _ycell(c3__dgtr, $3); }
      ; 

    wide_call
      : si_lep g bank_wide g si_pel
        { $$ = _ycell(c3__mtsp, $3); }
      | si_deg si_lep g bank_wide g si_pel
        { $$ = _ycell(c3__tmdg, $4); }
      ;

    wide_mtsg
      : si_sig si_lep rope w gene w bank_wide si_pel
        { $$ = _yqual(c3__mtsg, $3, $5, _ycell(c3__dgtr, $7)); }
 
    wide_path
      : si_von path
        { $$ = _ycell(c3__dgsg, _yflop(ywir_r, $2)); }
 
    path
      : thin              { $$ = _ycell($1, u2_nul); }
      | thin si_von path  { $$ = _ycell($1, $3); }
      ;

    thin
      : tok_term
        { $$ = _ytrel(c3__dtsg, c3_s3('t','a','m'), $1); }
      | si_den wide si_ned
        { $$ = $2; }
      ;

  /** Wide: funky stuff.
  **/
    wide_funk
      : si_des g bank_wide g si_sed
        { $$ = _ytrel(c3__hstr, $3, u2_nul); }
      | si_mit si_des g bank_wide g si_sed
        { $$ = _ytrel(c3__hsmt, $4, u2_nul); }
      | si_bar si_lep g bank_wide g si_pel
        { $$ = _ycell(c3__csbr, $4); }
      | si_pam si_lep g bank_wide g si_pel
        { $$ = _ycell(c3__cspm, $4); }
      | rope si_lep rack_wide si_pel
        { $$ = _ytrel(c3__mtlc, $1, $3); }
      | si_zap wide
        { $$ = _ycell(c3__cszp, $2); }
      | si_com wide 
        { $$ = _ycell(c3__ktsg, $2); }
      | si_sig si_den g bank_wide g si_ned
        { $$ = _ytrel(c3__hssg, $4, u2_nul); }
      | si_tar wide
        { $$ = _ycell(c3__mttr, $2); }
      | si_tec wide si_tec wide
        { $$ = _ytrel(c3__ktsp, $2, $4); }
      | si_lyc si_lep g wide w wide g si_pel
        { $$ = _ytrel(c3__dtlc, $4, $6); }
      | si_pes si_lep g wide g si_pel
        { $$ = _ycell(c3__dtps, $4); }
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
      : si_lyc         { $$ = 1; } 
      | si_lyc huny    { $$ = 1 + $2; }     // XX vulnerable
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
        : si_mit tok_term w gene      { $$ = _ycell($2, $4); } 
        ;

    chit_wide
      : si_lep chit_rack_wide si_pel
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
        : si_mit tok_term w wide      { $$ = _ycell($2, $4); }
        ;

    chop
      : '%' tok_term                  
        { $$ = $2; } 
      | '%' tok_term si_dot tok_delm  
        { $$ = _ycell($2, $4); }
      | '%' tok_term si_deg tok_term si_dot tok_delm
        { $$ = _ytrel($2, $4, $6); }
      | '%' tok_term si_deg tok_term si_dot tok_delm si_dot tok_delm
        { $$ = _yqual($2, $4, $6, $8); }


  /** Wide: normals.
  **/
    wide_norm: di_hesbar body_i_wide    
              { $$ = _yqual($1, u2_h($2), u2_t($2), u2_nul); }
    wide_norm: di_hessig body_d_wide    { $$ = _ytrel($1, $2, u2_nul); }
    wide_norm: di_hesket body_d_wide    { $$ = _ytrel($1, $2, u2_nul); }
    wide_norm: di_heslyc body_b_wide    
              { $$ = _yqual($1, u2_h($2), u2_t($2), u2_nul); }
    wide_norm: di_hesmit body_d_wide    { $$ = _ytrel($1, $2, u2_nul); }
    wide_norm: di_hespam body_i_wide
              { $$ = _yqual($1, u2_h($2), u2_t($2), u2_nul); }
    wide_norm: di_hestar body_d_wide    { $$ = _ytrel($1, $2, u2_nul); }

    wide_norm: di_casdeg body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_casdot body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_casdel body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_casled body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_caslyc body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_caspam body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_casbar body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_cassig body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_caszap body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_cassep body_h_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_lycpes body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_lycsep body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_lycled body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_lycdel body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_barlyc body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_bardeg body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barsep body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_bardot body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barmit body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barpes body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_bartar body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barzap body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_degtar body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_degsig body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_degsep body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_degpes body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_degket body_f_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_dotlyc body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_dotpes body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_dotcas body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_dottar body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_ketlyc body_g_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_ketsep body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_ketdeg body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_ketdel body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_ketled body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_ketsig body_a_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_zaplyc body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_zapcom body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_zapdax body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_zapzap body_l_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_zapmit body_l_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_zapdeg body_a_wide    { $$ = $2; scanner->bug = u2_no; }
    wide_norm: di_zaptam body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_sigbar body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_siglyc body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigsep hint_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigdax body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigdel hint_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigdeg hint_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigdot hint_e_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigket body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigled hint_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigmit hint_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigpes hint_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigpam body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigsig body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_tamdeg body_i_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_tamsig body_i_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_mitlyc body_j_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mittar body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitsep body_k_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitbar body_p_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitdot body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitdeg body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitpes body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitket body_f_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitsig body_q_wide    { $$ = _ycell($1, $2); }

  /** Wide - bodies.
  **/
    body_a_wide: si_lep g wide g si_pel
      { $$ = $3; }
    body_b_wide: si_lep g wide w wide g si_pel
      { $$ = _ycell($3, $5); }
    body_c_wide: si_lep g wide w wide w wide g si_pel
      { $$ = _ytrel($3, $5, $7); }
    body_d_wide: si_lep g bank_wide g si_pel
      { $$ = $3; }
    body_f_wide: si_lep g wide w wide w wide w wide g si_pel
      { $$ = _yqual($3, $5, $7, $9); }
    body_g_wide: si_lep g term w wide g si_pel
      { $$ = _ycell($3, $5); }
    body_h_wide: si_lep g wide w rack_wide si_pel
      { $$ = _ycell($3, $5); }
    body_i_wide: si_lep g wide w bank_wide g si_pel
      { $$ = _ycell($3, $5); }
    body_j_wide: si_lep g rope w rack_wide si_pel
      { $$ = _ycell($3, $5); }
    body_k_wide: si_lep g wide w wide g si_pel
      { $$ = _ytrel($3, $5, u2_nul); }
    body_l_wide:
      { $$ = u2_nul; }
/*
    body_o_wide: si_lep g wide g si_pel
      { $$ = $3; }
*/
    body_p_wide: si_lep g rope w wide w rack_wide si_pel
      { $$ = _ytrel($3, $5, $7); }
    body_q_wide: si_lep g rope w wide w wide si_pel
      { $$ = _ytrel($3, $5, $7); }

    hint_b_wide: si_lep g hint w wide g si_pel
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
    hint_a_wide: si_den tok_delm w tok_delm si_ned w gene 
      { $$ = _ycell(_ycell($2, $4), $7); }
      
    hint_b_wide: hont w wide
      { $$ = _ycell($1, $3); }

    hint_c_wide
      : si_lep huny w wide si_pel  { $$ = _ycell($2, $4); }
      | si_lep wide si_pel         { $$ = _ycell(0, $2); }
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
    tall_norm: di_heslyc w body_hv_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_hesmit w body_hw_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_hespam w body_hu_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_hessig w body_hw_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_hestar w body_hw_tall   { $$ = _ycell($1, $3); }

    tall_norm: di_barlyc w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_bardeg w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barsep w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_bardot w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barmit w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barpes w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_bartar w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barzap w body_b_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_lycsep w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_lycdel w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_lycled w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_lycpes w body_b_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_casbar w body_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_caslyc w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_cassep w body_h_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_casdeg w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_casdel w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_casdot w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_casled w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_caspam w body_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_cassig w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_caszap w body_a_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_degsep w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_degket w body_f_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_degpes w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_degsig w body_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_degtar w body_d_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_dotlyc w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_dotcas w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_dotpes w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_dottar w body_b_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_ketlyc w body_g_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_ketsep w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_ketdeg w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_ketdel w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_ketled w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_ketsig w body_a_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_mitbar w body_p_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitlyc w body_j_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitdeg w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitsep w body_k_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitdot w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitket w body_f_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitpes w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitsig w body_q_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mittar w body_a_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_pamlyc w body_ex_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_pamdeg w body_ex_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_pamsep w body_ex_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_pamdot w body_e_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_pammit w body_e_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_pampes w body_ex_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_pamzap w body_ex_tall   { $$ = _ycell($1, $3); }

    tall_norm: di_sigbar w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_siglyc w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigsep w hint_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigdax w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigdeg w hint_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigdel w hint_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigdot w hint_e_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigket w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigled w hint_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigmit w hint_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigpes w hint_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigpam w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigsig w body_b_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_tamdeg w body_i_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_tamsig w body_i_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_zapcom w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_zapdax w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_zapdeg w body_a_tall    { $$ = $3; scanner->bug = u2_no; }
    tall_norm: di_zaplyc w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_zaptam w body_b_tall    { $$ = _ycell($1, $3); }

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
    hint_a_tall: si_den tok_delm w tok_delm si_ned w gene 
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
          : si_sep              { $$ = _2; }
          | si_pes              { $$ = _3; }
          | si_sep axis_galu    { $$ = j2_mbc(Pt3, peg)(ywir_r, _2, $2); }
          | si_pes axis_galu    { $$ = j2_mbc(Pt3, peg)(ywir_r, _3, $2); }
          ;
        axis_galu
          : si_del              { $$ = _2; }
          | si_led              { $$ = _3; }
          | si_del axis_beto    { $$ = j2_mbc(Pt3, peg)(ywir_r, _2, $2); }
          | si_led axis_beto    { $$ = j2_mbc(Pt3, peg)(ywir_r, _3, $2); }
          ;

  /** Digraphs (with stem)
  **/
    di_casdeg: si_cas si_deg  { $$ = c3__csdg; }
    di_casdot: si_cas si_dot  { $$ = c3__csdt; }
    di_casled: si_cas si_led  { $$ = c3__csld; }
    di_casdel: si_cas si_del  { $$ = c3__csdl; }
    di_caspam: si_cas si_pam  { $$ = c3__cspm; }
    di_casbar: si_cas si_bar  { $$ = c3__csbr; }
    di_cassig: si_cas si_sig  { $$ = c3__cssg; }
    di_caszap: si_cas si_zap  { $$ = c3__cszp; }
    di_caslyc: si_cas si_lyc  { $$ = c3__cslc; }
    di_cassep: si_cas si_sep  { $$ = c3__cssp; }

    di_hesbar: si_hes si_bar  { $$ = c3__hsbr; }
    di_hesket: si_hes si_ket  { $$ = c3__hskt; }
    di_heslyc: si_hes si_lyc  { $$ = c3__hslc; }
    di_hesmit: si_hes si_mit  { $$ = c3__hsmt; }
    di_hespam: si_hes si_pam  { $$ = c3__hspm; }
    di_hessig: si_hes si_sig  { $$ = c3__hssg; }
    di_hestar: si_hes si_tar  { $$ = c3__hstr; }

    di_barlyc: si_bar si_lyc  { $$ = c3__brlc; }
    di_barsep: si_bar si_sep  { $$ = c3__brsp; }
    di_bardeg: si_bar si_deg  { $$ = c3__brdg; }
    di_bardot: si_bar si_dot  { $$ = c3__brdt; }
    di_barmit: si_bar si_mit  { $$ = c3__brmt; }
    di_barpes: si_bar si_pes  { $$ = c3__brps; }
    di_bartar: si_bar si_tar  { $$ = c3__brtr; }
    di_barzap: si_bar si_zap  { $$ = c3__brzp; }

    di_lycdel: si_lyc si_del  { $$ = c3__lcdl; }
    di_lycled: si_lyc si_led  { $$ = c3__lcld; }
    di_lycpes: si_lyc si_pes  { $$ = c3__lcps; }
    di_lycsep: si_lyc si_sep  { $$ = c3__lcsp; }
 
    di_degtar: si_deg si_tar  { $$ = c3__dgtr; }
    di_degsig: si_deg si_sig  { $$ = c3__dgsg; }
    di_degsep: si_deg si_sep  { $$ = c3__dgsp; }
    di_degpes: si_deg si_pes  { $$ = c3__dgps; }
    di_degket: si_deg si_ket  { $$ = c3__dgkt; }

    di_dotlyc: si_dot si_lyc  { $$ = c3__dtlc; }
    di_dotpes: si_dot si_pes  { $$ = c3__dtps; }
    di_dotcas: si_dot si_cas  { $$ = c3__dtcs; }
    di_dottar: si_dot si_tar  { $$ = c3__dttr; }
   
    di_ketlyc: si_ket si_lyc  { $$ = c3__ktlc; }
    di_ketsep: si_ket si_sep  { $$ = c3__ktsp; }
    di_ketdeg: si_ket si_deg  { $$ = c3__ktdg; }
    di_ketdel: si_ket si_del  { $$ = c3__ktdl; }
    di_ketled: si_ket si_led  { $$ = c3__ktld; }
    di_ketsig: si_ket si_sig  { $$ = c3__ktsg; }

    di_mitlyc: si_mit si_lyc  { $$ = c3__mtlc; }
    di_mittar: si_mit si_tar  { $$ = c3__mttr; }
    di_mitbar: si_mit si_bar  { $$ = c3__mtbr; }
    di_mitsig: si_mit si_sig  { $$ = c3__mtsg; }
    di_mitsep: si_mit si_sep  { $$ = c3__mtsp; }
    di_mitdeg: si_mit si_deg  { $$ = c3__mtdg; }
    di_mitdot: si_mit si_dot  { $$ = c3__mtdt; }
    di_mitpes: si_mit si_pes  { $$ = c3__mtps; }
    di_mitket: si_mit si_ket  { $$ = c3__mtkt; }

    di_pamlyc: si_pam si_lyc  { $$ = c3__pmlc; }
    di_pamsep: si_pam si_sep  { $$ = c3__pmsp; }
    di_pamdeg: si_pam si_deg  { $$ = c3__pmdg; }
    di_pamdot: si_pam si_dot  { $$ = c3__pmdt; }
    di_pammit: si_pam si_mit  { $$ = c3__pmmt; }
    di_pampes: si_pam si_pes  { $$ = c3__pmps; }
    di_pamzap: si_pam si_zap  { $$ = c3__pmzp; }

    di_tamdeg: si_tam si_deg  { $$ = c3__tmdg; }
    di_tamsig: si_tam si_sig  { $$ = c3__tmsg; }

    di_siglyc: si_sig si_lyc  { $$ = c3__sglc; }
    di_sigbar: si_sig si_bar  { $$ = c3__sgbr; }
    di_sigsep: si_sig si_sep  { $$ = c3__sgsp; }
    di_sigdax: si_sig si_dax  { $$ = c3__sgdx; }
    di_sigdeg: si_sig si_deg  { $$ = c3__sgdg; }
    di_sigdel: si_sig si_del  { $$ = c3__sgdl; }
    di_sigdot: si_sig si_dot  { $$ = c3__sgdt; }
    di_sigket: si_sig si_ket  { $$ = c3__sgkt; }
    di_sigled: si_sig si_led  { $$ = c3__sgld; }
    di_sigmit: si_sig si_mit  { $$ = c3__sgmt; }
    di_sigpes: si_sig si_pes  { $$ = c3__sgps; }
    di_sigpam: si_sig si_pam  { $$ = c3__sgpm; }
    di_sigsig: si_sig si_sig  { $$ = c3__sgsg; }

    di_zaplyc: si_zap si_lyc  { $$ = c3__zplc; }
    di_zapcom: si_zap si_com  { $$ = c3__zpcm; }
    di_zapdax: si_zap si_dax  { $$ = c3__zpdx; }
    di_zapmit: si_zap si_mit  { $$ = c3__zpmt; }
    di_zaptam: si_zap si_tam  { $$ = c3__zptm; }
    di_zapzap: si_zap si_zap  { $$ = c3__zpzp; }
    di_zapdeg: si_zap si_deg  { $$ = c3__zpdg; scanner->bug = u2_yes; }
    
  /* Signs.
  */
    si_cas: '?'
    si_bar: '|'
    si_bot: '\''
    si_cab: '_'
    si_com: ','
    si_dax: '#'
    si_deg: ':'
    si_del: '<'
    si_dot: '.'
    si_des: '{'
    si_hes: '$'
    si_ket: '^'
    si_led: '>'
    si_lyc: '='
    si_tam: ';'
    si_lep: '('
    si_mit: '%'
    si_ned: ']'
    si_den: '['
    si_pel: ')'
    si_pes: '+'
    si_pam: '&'
    si_sep: '-'
    si_tar: '*'
    si_nov: '\\'
    si_sed: '}'
    si_sig: '~'
    si_tec: '`'
    /* si_toq: '"' */
    si_von: '/'
    si_wat: '@'
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
      : si_bot loct_mid si_bot
        { $$ = u2_bn_tape(ywir_r, $2); u2_rz(ywir_r, $2); }
      ;
        loct_mid: { $$ = _0; }
                 | cq gap loct_mid { $$ = _ycell($1, $3); }
                 ;


  /** Whitespace.
  **/
    gap
      : si_nov g si_von  { $$ = _0; }
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
      if ( !y239_parse(&scanner) ) {
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
      if ( !y239_parse(&scanner) ) {
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
y239_lex(YYSTYPE *lvalp, YYLTYPE *llocp, struct _u2_scanner *scanner)
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
          return y239_error(llocp, scanner, "tape error");
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
int y239_error(YYLTYPE *llocp, struct _u2_scanner *scanner, char const *msg)
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
