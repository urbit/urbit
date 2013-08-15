/* The boot parser for watt.
**
** This file is in the public domain.
*/

/* Pre-bison prologue.
*/
%{
# include "all.h"

#   define Pt1           k_256__a
#   define Pt2           k_256__a__b
#   define Pt3           k_256__a__b__c
#   define Pt4           k_256__a__b__c__d
#   define Pt5           k_256__a__b__c__d__e
#   define Pt6           k_256__a__b__c__d__e__f

  /* Everything is a noun - no yacc type declarations!
  */
#   define YYSTYPE u2_noun

  /* Our fake scanner.
  */
    struct _u2_scanner {
      u2_ray  wir_r;
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
  %name-prefix="y256_"

  /* We laugh at your petty shift-reduce conflicts.
  */
  %expect 91

  %pure-parser
  %locations
  %parse-param {struct _u2_scanner *scanner}
  %lex-param {struct _u2_scanner *scanner}


/* Support routines.
*/
%{
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
  : wide_a
  | wide_rope si_deg wide
    { $$ = _ytrel(c3__bndl, $1, $3); }
  ;

wide_a
  : wide_c
  | tok_term si_bon wide
    { $$ = _ytrel(c3__ktbn, $1, $3); }
  ;

wide_c
  : wide_hard
  | wide_base
  | wide_rope
  | wide_funk
  | wide_cage
  | wide_pick
  | wide_call
  | wide_mtsg
  | wide_norm
  ;

    wide_hard
      : '0' 'x' tok_chex
        { $$ = _ycell(c3__dtsg, $3); }
      | tok_delm
        { $$ = _ycell(c3__dtsg, $1); }
      | si_pam
        { $$ = _ycell(c3__dtsg, _0); }
      | si_bar
        { $$ = _ycell(c3__dtsg, _1); }
      | tok_loct
        { $$ = _ycell(c3__dtsg, $1); }
      | si_mit tok_term
        { $$ = _ycell(c3__dtsg, $2); }
      | si_mit si_mit
        { $$ = _ycell(c3__dtsg, _0); }
      ;
   
    wide_base
      : si_cas
        { $$ = _ycell(c3__tmbn, c3__flag); }
      | si_tar
        { $$ = _ycell(c3__tmbn, c3__noun); }
      | si_ket
        { $$ = _ycell(c3__tmbn, c3__cell); }
      | si_wat
        { $$ = _ycell(c3__tmbn, c3__atom); }
      | si_sig
        { $$ = _ycell(c3__tmbn, c3__null); }
      ;
   
    wide_rope
      : rope            
        { $$ = _ytrel(c3__mtbn, $1, _0); }
      ;
    wide_cage
      : si_den g bank_wide g si_ned 
        { $$ = _ycell(c3__dgtr, $3); }
      ; 

    wide_pick
      : si_del g bank_wide g si_led
        { $$ = _ycell(c3__brcs, $3); } 
      ;

    wide_call
      : si_lep g bank_wide g si_pel
        { $$ = _ycell(c3__mtdp, $3); }
      | si_deg si_lep g bank_wide g si_pel
        { $$ = _ycell(c3__tmdg, $4); }
      ;

    wide_mtsg
      : si_sig si_lep rope w gene w bank_wide si_pel
        { $$ = _yqual(c3__mtsg, $3, $5, _ycell(c3__dgtr, $7)); }
 
  /** Wide: funky stuff.
  **/
    wide_funk
      : si_des g bank_wide g si_sed
        { $$ = _ytrel(c3__cbtr, $3, u2_nul); }
      | si_mit si_des g bank_wide g si_sed
        { $$ = _ytrel(c3__cbmt, $4, u2_nul); }
      | rope si_lep rack_wide si_pel
        { $$ = _ytrel(c3__mtbn, $1, $3); }
      | si_zap wide
        { $$ = _ycell(c3__cszp, $2); }
      | si_com wide 
        { $$ = _ycell(c3__ktsg, $2); }
      | si_cab si_den g bank_wide g si_ned
        { $$ = _ytrel(c3__cbdg, $4, u2_nul); }
      | si_tar wide
        { $$ = _ycell(c3__mttr, $2); }
      | si_tec wide si_tec wide
        { $$ = _ytrel(c3__ktdp, $2, $4); }
      | si_bon si_lep g wide w wide g si_pel
        { $$ = _ytrel(c3__dtbn, $4, $6); }
      | si_pad si_lep g wide g si_pel
        { $$ = _ycell(c3__dtpd, $4); }
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
      : si_bon         { $$ = 1; } 
      | si_bon huny    { $$ = 1 + $2; }     // XX vulnerable
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

/*
    path 
      : thin
      | thin si_von path    { $$ = _ytrel(c3__dgdp, $1, $3); }
      ;

    thin
      : term        { $$ = _ycell(c3__dtsg, $1); }
      | wide_hard
      | wide_cage
      | wide_pick
      | wide_call
      ;
*/

  /** Wide: normals.
  **/
    wide_norm: di_cabbon body_b_wide    { $$ = _ytrel($1, $2, u2_nul); }
    wide_norm: di_cabbar body_i_wide    
              { $$ = _yqual($1, u2_h($2), u2_t($2), u2_nul); }
    wide_norm: di_cabdeg body_d_wide    { $$ = _ytrel($1, $2, u2_nul); }
    wide_norm: di_cabket body_d_wide    { $$ = _ytrel($1, $2, u2_nul); }
    wide_norm: di_cabmit body_d_wide    { $$ = _ytrel($1, $2, u2_nul); }
    wide_norm: di_cabpam body_i_wide
              { $$ = _yqual($1, u2_h($2), u2_t($2), u2_nul); }
    wide_norm: di_cabtar body_d_wide    { $$ = _ytrel($1, $2, u2_nul); }

    wide_norm: di_casdeg body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_casdot body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_casdel body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_casled body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_caspam body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_casbar body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_cassig body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_caszap body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_casbon body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_casdap body_h_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_castar body_i_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_bonpad body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_bondap body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_bonled body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_bondel body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_barbon body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barcas body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_bardeg body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_bardap body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_bardot body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barmit body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barpad body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_bartar body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barzap body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_degtar body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_degsig body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_degdap body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_degpad body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_degket body_f_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_dotbon body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_dotpad body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_dotcas body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_dottar body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_ketdap body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_ketsec body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_ketpad body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_ketdeg body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_ketbon body_g_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_kettar body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_ketsig body_a_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_zapbon body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_zapdax body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_zapzap body_l_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_zapmit body_l_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_zapdeg body_a_wide    { $$ = $2; scanner->bug = u2_no; }
    wide_norm: di_zaptam body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_sigbar body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigbon body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigdap hint_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigdax body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigdel hint_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigdeg hint_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigdot hint_e_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigket body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigled hint_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigmit hint_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigpad hint_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigpam body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_sigsig body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_tamdap body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_tamdeg body_i_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_tampad body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_tamsig body_i_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_tamtar body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_mitbon body_j_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mittar body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitdap body_k_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitbar body_p_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitdot body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitdeg body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitpad body_c_wide    { $$ = _ycell($1, $2); }
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
    tall_norm: di_cabbar w body_ev_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_cabbon w body_ew_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_cabdeg w body_ey_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_cabket w body_ey_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_cabmit w body_ey_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_cabpam w body_ev_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_cabtar w body_ey_tall   { $$ = _ycell($1, $3); }

    tall_norm: di_barbon w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barcas w body_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_bardeg w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_bardap w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_bardot w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barmit w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barpad w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_bartar w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barzap w body_b_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_bondap w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_bondel w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_bonled w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_bonpad w body_b_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_casbar w body_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_casbon w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_casdap w body_h_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_casdeg w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_casdel w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_casdot w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_casled w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_caspam w body_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_cassig w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_castar w body_i_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_caszap w body_a_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_degdap w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_degket w body_f_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_degpad w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_degsig w body_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_degtar w body_d_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_dotbon w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_dotcas w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_dotpad w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_dottar w body_b_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_ketbon w body_g_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_ketdap w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_ketpad w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_ketsig w body_a_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_mitbar w body_p_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitbon w body_j_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitdeg w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitdap w body_k_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitdot w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitket w body_f_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitpad w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitsig w body_q_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mittar w body_a_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_pambon w body_ex_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_pamdeg w body_ex_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_pamdap w body_ex_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_pamdot w body_e_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_pammit w body_e_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_pampad w body_ex_tall   { $$ = _ycell($1, $3); }
    tall_norm: di_pamzap w body_ex_tall   { $$ = _ycell($1, $3); }

    tall_norm: di_sigbar w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigbon w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigdap w hint_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigdax w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigdeg w hint_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigdel w hint_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigdot w hint_e_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigket w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigled w hint_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigmit w hint_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigpad w hint_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigpam w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_sigsig w body_b_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_tamdap w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_tamdeg w body_i_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_tampad w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_tamsig w body_i_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_tamtar w body_b_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_zapdax w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_zapdeg w body_a_tall    { $$ = $3; scanner->bug = u2_no; }
    tall_norm: di_zapbon w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_zaptam w body_b_tall    { $$ = _ycell($1, $3); }

  /** Tall - bodies.
  **/
    body_a_tall: gene                       { $$ = $1; }
    body_b_tall: gene w gene                { $$ = _ycell($1, $3); }
    body_c_tall: gene w gene w gene         { $$ = _ytrel($1, $3, $5); }
    body_d_tall: bank_tall                  { $$ = $1; }
    body_e_tall: wing                       { $$ = $1; }
    body_ex_tall: gene w wing               { $$ = _ycell($1, $3); }
    body_ey_tall: tall_star wing          { $$ = _ycell($1, $2); }
    body_ew_tall: gene w gene w wing        { $$ = _ytrel($1, $3, $5); }
    body_ev_tall: gene w tall_star wing   { $$ = _ytrel($1, $3, $4); }
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
      | secs                    { $$ = $1; }
      | secs si_dot g rope      { $$ = j2_mbc(Pt2, weld)(ywir_r, $1, $4); }
      ;

      secs
        : si_sec                { $$ = _ycell(_0, _0); }
        | si_sec secs           { $$ = _ytrel(_0, _ycell(_0, _2), $2); }
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
          : si_dap              { $$ = _2; }
          | si_pad              { $$ = _3; }
          | si_dap axis_galu    { $$ = j2_mbc(Pt3, peg)(ywir_r, _2, $2); }
          | si_pad axis_galu    { $$ = j2_mbc(Pt3, peg)(ywir_r, _3, $2); }
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
    di_casbon: si_cas si_bon  { $$ = c3__csbn; }
    di_casdap: si_cas si_dap  { $$ = c3__csdp; }
    di_castar: si_cas si_tar  { $$ = c3__cstr; }

    di_cabbon: si_cab si_bon  { $$ = c3__cbbn; }
    di_cabbar: si_cab si_bar  { $$ = c3__cbbr; }
    di_cabdeg: si_cab si_deg  { $$ = c3__cbdg; }
    di_cabket: si_cab si_ket  { $$ = c3__cbkt; }
    di_cabmit: si_cab si_mit  { $$ = c3__cbmt; }
    di_cabpam: si_cab si_pam  { $$ = c3__cbpm; }
    di_cabtar: si_cab si_tar  { $$ = c3__cbtr; }

    di_barbon: si_bar si_bon  { $$ = c3__brbn; }
    di_barcas: si_bar si_cas  { $$ = c3__brcs; }
    di_bardap: si_bar si_dap  { $$ = c3__brdp; }
    di_bardeg: si_bar si_deg  { $$ = c3__brdg; }
    di_bardot: si_bar si_dot  { $$ = c3__brdt; }
    di_barmit: si_bar si_mit  { $$ = c3__brmt; }
    di_barpad: si_bar si_pad  { $$ = c3__brpd; }
    di_bartar: si_bar si_tar  { $$ = c3__brtr; }
    di_barzap: si_bar si_zap  { $$ = c3__brzp; }

    di_bondel: si_bon si_del  { $$ = c3__bndl; }
    di_bonled: si_bon si_led  { $$ = c3__bnld; }
    di_bonpad: si_bon si_pad  { $$ = c3__bnpd; }
    di_bondap: si_bon si_dap  { $$ = c3__bndp; }
 
    di_degtar: si_deg si_tar  { $$ = c3__dgtr; }
    di_degsig: si_deg si_sig  { $$ = c3__dgsg; }
    di_degdap: si_deg si_dap  { $$ = c3__dgdp; }
    di_degpad: si_deg si_pad  { $$ = c3__dgpd; }
    di_degket: si_deg si_ket  { $$ = c3__dgkt; }

    di_dotbon: si_dot si_bon  { $$ = c3__dtbn; }
    di_dotpad: si_dot si_pad  { $$ = c3__dtpd; }
    di_dotcas: si_dot si_cas  { $$ = c3__dtcs; }
    di_dottar: si_dot si_tar  { $$ = c3__dttr; }
    
    di_ketdap: si_ket si_dap  { $$ = c3__ktdp; }
    di_ketsec: si_ket si_sec  { $$ = c3__ktbc; }
    di_ketdeg: si_ket si_deg  { $$ = c3__ktdg; }
    di_ketpad: si_ket si_pad  { $$ = c3__ktpd; }
    di_ketbon: si_ket si_bon  { $$ = c3__ktbn; }
    di_kettar: si_ket si_tar  { $$ = c3__kttr; }
    di_ketsig: si_ket si_sig  { $$ = c3__ktsg; }

    di_mitbon: si_mit si_bon  { $$ = c3__mtbn; }
    di_mittar: si_mit si_tar  { $$ = c3__mttr; }
    di_mitbar: si_mit si_bar  { $$ = c3__mtbr; }
    di_mitsig: si_mit si_sig  { $$ = c3__mtsg; }
    di_mitdap: si_mit si_dap  { $$ = c3__mtdp; }
    di_mitdeg: si_mit si_deg  { $$ = c3__mtdg; }
    di_mitdot: si_mit si_dot  { $$ = c3__mtdt; }
    di_mitpad: si_mit si_pad  { $$ = c3__mtpd; }
    di_mitket: si_mit si_ket  { $$ = c3__mtkt; }

    di_pambon: si_pam si_bon  { $$ = c3__pmbn; }
    di_pamdap: si_pam si_dap  { $$ = c3__pmdp; }
    di_pamdeg: si_pam si_deg  { $$ = c3__pmdg; }
    di_pamdot: si_pam si_dot  { $$ = c3__pmdt; }
    di_pammit: si_pam si_mit  { $$ = c3__pmmt; }
    di_pampad: si_pam si_pad  { $$ = c3__pmpd; }
    di_pamzap: si_pam si_zap  { $$ = c3__pmzp; }

    di_tamdap: si_tam si_dap  { $$ = c3__tmdp; }
    di_tamdeg: si_tam si_deg  { $$ = c3__tmdg; }
    di_tampad: si_tam si_pad  { $$ = c3__tmpd; }
    di_tamsig: si_tam si_sig  { $$ = c3__tmsg; }
    di_tamtar: si_tam si_tar  { $$ = c3__tmtr; }

    di_sigbon: si_sig si_bon  { $$ = c3__sgbn; }
    di_sigbar: si_sig si_bar  { $$ = c3__sgbr; }
    di_sigdap: si_sig si_dap  { $$ = c3__sgdp; }
    di_sigdax: si_sig si_dax  { $$ = c3__sgdx; }
    di_sigdeg: si_sig si_deg  { $$ = c3__sgdg; }
    di_sigdel: si_sig si_del  { $$ = c3__sgdl; }
    di_sigdot: si_sig si_dot  { $$ = c3__sgdt; }
    di_sigket: si_sig si_ket  { $$ = c3__sgkt; }
    di_sigled: si_sig si_led  { $$ = c3__sgld; }
    di_sigmit: si_sig si_mit  { $$ = c3__sgmt; }
    di_sigpad: si_sig si_pad  { $$ = c3__sgpd; }
    di_sigpam: si_sig si_pam  { $$ = c3__sgpm; }
    di_sigsig: si_sig si_sig  { $$ = c3__sgsg; }

    di_zapbon: si_zap si_bon  { $$ = c3__zpbn; }
    di_zapdax: si_zap si_dax  { $$ = c3__zpdx; }
    di_zapmit: si_zap si_mit  { $$ = c3__zpmt; }
    di_zaptam: si_zap si_tam  { $$ = c3__zptm; }
    di_zapzap: si_zap si_zap  { $$ = c3__zpzp; }
    di_zapdeg: si_zap si_deg  { $$ = c3__zpdg; scanner->bug = u2_yes; }
    
  /* Signs.
  */
    si_cas: '?'
    si_dap: '-'
    si_bar: '|'
    si_bon: '='
    si_bot: '\''
    si_cab: '_'
    si_com: ','
    si_dax: '#'
    si_deg: ':'
    si_del: '<'
    si_dot: '.'
    si_des: '{'
    si_ket: '^'
    si_led: '>'
    si_tam: ';'
    si_lep: '('
    si_mit: '%'
    si_ned: ']'
    si_den: '['
    si_pel: ')'
    si_pad: '+'
    si_pam: '&'
    si_sec: '$'
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
      | si_sec    { $$ = _0; }
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

    tok_loct
      : si_bot loct_mid si_bot
        { $$ = u2_bn_tape(ywir_r, $2); u2_rz(ywir_r, $2); }
      ;
        loct_mid: { $$ = _0; }
                 | cq loct_mid { $$ = _ycell($1, $2); }
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
                   u2_noun sack)
{
  scanner->wir_r = wir_r;
  scanner->scan = u2_none;
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
                   u2_noun clip)
{
  scanner->wir_r = wir_r;
  scanner->scan = u2_none;
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
  j2_mby(Pt6, ream)(u2_wire wir_r, 
                    u2_noun txt)                                  //  retain
  {
    struct _u2_scanner scanner;

    _scanner_init_sack(&scanner, wir_r, txt);

    if ( 0 != setjmp(scanner.env) ) {
      fprintf(stderr, "ream error: %d, %d\n", 
                      scanner.s.xw_line, scanner.s.xw_col);
      return u2_bl_bail(wir_r, c3__fail);
    }
    else {
      if ( !y256_parse(&scanner) ) {
        return scanner.scan;
      }
      else {
        return u2_bl_bail(wir_r, c3__fail);
      }
    }
  }
  u2_noun                                                         //  transfer
  j2_mby(Pt6, vest)(u2_wire wir_r, 
                    u2_noun tub)                                  //  retain
  {
    struct  _u2_scanner scanner;
    u2_noun hor;

    _scanner_init_clip(&scanner, wir_r, tub);

    if ( 0 != setjmp(scanner.env) ) {
      hor = u2_bc(wir_r, scanner.s.xw_line, scanner.s.xw_col);

      return u2_bc(wir_r, hor, u2_nul);
    }
    else {
      if ( !y256_parse(&scanner) ) {
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
      return j2_mby(Pt6, ream)(wir_r, txt);
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
      return j2_mby(Pt6, vest)(wir_r, tub);
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
y256_lex(YYSTYPE *lvalp, YYLTYPE *llocp, struct _u2_scanner *scanner)
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
          return y256_error(llocp, scanner, "tape error");
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
int y256_error(YYLTYPE *llocp, struct _u2_scanner *scanner, char const *msg)
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
