/* The boot parser for watt.
**
** This file is in the public domain.
*/

/* Pre-bison prologue.
*/
%{
# include "u4/all.h"

  /* Everything is a noun - no yacc type declarations!
  */
#   define YYSTYPE u4_noun

  /* Our fake scanner.
  */
    struct _u4_scanner {
      u4_lane lane;     /* lane for computation */
      u4_noun scan;     /* result - set by parser */

      /* Per-parse state.
      */
      struct {
        u4_log  site;   /* listmark - path from top to part */
        u4_atom tube;   /* part data (ie, source document) */
        u4_tab  bowl;   /* parts - (mark sack). */
      } p;
      
      /* Scanning state.
      */
      struct {
        uint32_t token;    /* initial type token, if any */
        u4_pb    pb;       /* byte position */
        u4_xw    xw_line;  /* current line */
        u4_xw    xw_col;   /* current column */
      } s;
    };
#   define yylane (scanner->lane)

  /* Forward declarations.
  */
    static u4_noun _watt_parse_part(struct _u4_scanner *, u4_noun);
    static u4_noun _watt_locate(u4_lane, const void *, u4_noun);

    /* Unix FS loading.  Highly dangerous and improper.
    */
      /* u4_unix_path_file():
      **
      **  Load abstract Watt gene path as a file.
      **  XX: should use environment variable.
      */
        u4_noun
        u4_unix_path_file(u4_lane lane,
                          u4_noun fud);

      /* u4_unix_path_watt():
      **
      **  Load abstract Watt path as a Watt gene.
      */
        u4_noun
        u4_unix_path_watt(u4_lane lane,
                          u4_noun fud);

  /* Construction macros.
  */
#   define _ycell(a, b)            u4_k_cell(yylane, a, b)
#   define _ytrel(a, b, c)         u4_k_trel(yylane, a, b, c)
#   define _yqual(a, b, c, d)      u4_k_qual(yylane, a, b, c, d)
#   define _yquil(a, b, c, d, e)   u4_k_quil(yylane, a, b, c, d, e)

%}

/* Bison directives.
*/
  /* Start tokens for vere and hume respectively.
  */
  %token T_vere
  %token T_hume

  /* With the mighty power of GLR... 
  */
  %glr-parser
 
  /* We laugh at your petty shift-reduce conflicts.
  */
  %expect 63

  %pure-parser
  %locations
  %parse-param {struct _u4_scanner *scanner}
  %lex-param {struct _u4_scanner *scanner}


/* Support routines.
*/
%{
%}

%%

file 
  : g gene g        { scanner->scan = $2; }
  ;

gene
  : tall  { $$ = _watt_locate(yylane, &@1, $1); }
  | wide  /* { $$ = _watt_locate(yylane, &@1, $1); } */
  ;

wide
  : wide_a
  | wide_rope si_dig wide
    { $$ = _ytrel(u4_atom_claf, $1, $3); }
  ;

wide_a
  : wide_c
  | tok_term si_ben wide
    { $$ = _ytrel(u4_atom_bran, $1, $3); }
  ;

wide_c
  : wide_hard
  | wide_base
  | wide_rope
  | wide_funk
  | wide_cage
  | wide_pick
  | wide_call
  | wide_hang
  | wide_norm
  ;

    wide_hard
      : '0' 'x' tok_chex
        { $$ = _ycell(u4_atom_bone, $3); }
      | tok_delm
        { $$ = _ycell(u4_atom_bone, $1); }
      | si_amp
        { $$ = _ycell(u4_atom_bone, u4_noun_0); }
      | si_bar
        { $$ = _ycell(u4_atom_bone, u4_noun_1); }
      | tok_loct
        { $$ = _ycell(u4_atom_bone, $1); }
      | si_mit tok_term
        { $$ = _ycell(u4_atom_bone, $2); }
      | si_mit si_mit
        { $$ = _ycell(u4_atom_bone, u4_noun_0); }
      ;
   
    wide_base
      : si_ask
        { $$ = _ycell(u4_atom_bean, u4_atom_flag); }
      | si_ras
        { $$ = _ycell(u4_atom_bean, u4_atom_blur); }
      | si_hat
        { $$ = _ycell(u4_atom_bean, u4_atom_cell); }
      | si_pat
        { $$ = _ycell(u4_atom_bean, u4_atom_atom); }
      | si_sig
        { $$ = _ycell(u4_atom_bean, u4_atom_null); }
      ;
   
    wide_rope
      : rope            
        { $$ = _ytrel(u4_atom_mack, $1, u4_noun_0); }
      ;
    wide_cage
      : si_nom g bank_wide g si_mon 
        { $$ = _ycell(u4_atom_prex, $3); }
      ; 

    wide_pick
      : si_der g bank_wide g si_red
        { $$ = _ycell(u4_atom_rond, $3); } 
      ;

    wide_call
      : si_lep g bank_wide g si_pel
        { $$ = _ycell(u4_atom_fung, $3); }
      ;

    wide_hang
      : si_sig si_lep rope w gene w bank_wide si_pel
        { $$ = _yqual(u4_atom_hang, $3, $5, _ycell(u4_atom_prex, $7)); }
 
  /** Wide: funky stuff.
  **/
    wide_funk
      : si_sud g bank_wide g si_dus
        { $$ = _ycell(u4_atom_slax, $3); }
      | si_sud g si_dus
        { $$ = _ycell(u4_atom_slax, u4_noun_0); }
      | rope si_lep rack_wide si_pel
        { $$ = _ytrel(u4_atom_mack, $1, $3); }
      | si_hop wide
        { $$ = _ycell(u4_atom_vern, $2); }
      | si_amp wide
        { $$ = _ycell(u4_atom_mast, $2); }
      | si_ras wide
        { $$ = _ycell(u4_atom_teck, $2); }
      | si_cab wide
        { $$ = _ycell(u4_atom_fist, $2); }
      | si_tic wide si_tic wide
        { $$ = _ytrel(u4_atom_cast, $2, $4); }
/*
      | si_sol path
        { $$ = $2; }
*/
      ;

/*
    path 
      : thin
      | thin si_sol path    { $$ = _ytrel(u4_atom_twix, $1, $3); }
      ;

    thin
      : term        { $$ = _ycell(u4_atom_bone, $1); }
      | wide_hard
      | wide_cage
      | wide_pick
      | wide_call
      ;
*/

  /** Wide: normals.
  **/
    wide_norm: di_askdig body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_askdot body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_askder body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_askred body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_askamp body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_askbar body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_asksig body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_askhop body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_askben body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_asknub body_h_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_askras body_i_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_benpod body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_bennub body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_benred body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_bender body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_barnub body_o_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barben body_o_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barras body_e_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barmit body_e_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_bardig body_o_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barask body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_barsig body_i_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_digras body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_digsig body_d_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_dignub body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_digpod body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_dighat body_f_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_dotben body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_dothat body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_dotask body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_dotras body_b_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_hatask body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_hatnub body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_hatbuc body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_hatpod body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_hatdig body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_hatben body_g_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_hatmit body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_hatras body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_hatpat body_a_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_hopdax body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_hopven body_l_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_hopmit body_l_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_hopdig body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_hopben body_a_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_lomnub body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_lompod body_a_wide    { $$ = _ycell($1, $2); }

    wide_norm: di_mitben body_j_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitras body_a_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitnub body_k_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitbar body_p_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitdot body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitdig body_b_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mitpod body_c_wide    { $$ = _ycell($1, $2); }
    wide_norm: di_mithat body_f_wide    { $$ = _ycell($1, $2); }
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
    body_e_wide: si_lep g prop menu_wide si_pel
      { $$ = _ycell($3, $4); }
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
      { $$ = _ytrel($3, $5, u4_nul); }
    body_l_wide:
      { $$ = u4_nul; }
    body_o_wide: si_lep g prop wide g si_pel
      { $$ = _ycell($3, $4); }
    body_p_wide: si_lep g rope w wide w rack_wide si_pel
      { $$ = _ytrel($3, $5, $7); }
    body_q_wide: si_lep g rope w wide w wide si_pel
      { $$ = _ytrel($3, $5, $7); }

    bank_wide
      : wide             { $$ = _ycell($1, u4_noun_0); }
      | wide w bank_wide { $$ = _ycell($1, $3); }
      ;

    pair_wide
      : wide w wide      { $$ = _ycell($1, $3); }
      ;

    dish_wide
      : term w wide      { $$ = _ycell($1, $3); }
      ;

    rack_wide
      : g                             { $$ = u4_noun_0; }
      | pair_wide g                   { $$ = _ycell($1, u4_noun_0); }
      | pair_wide ',' g rack_wide     { $$ = _ycell($1, $4); }
      ;

    menu_wide
      : g                             { $$ = u4_noun_0; }
      | dish_wide g                   { $$ = _ycell($1, u4_noun_0); }
      | dish_wide ',' g menu_wide     { $$ = _ycell($1, $4); }
      ;
tall
  : tall_norm

  /** Tall - normals.
  **/
    tall_norm: di_askdig w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_askdot w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_askder w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_askred w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_askamp w body_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_askbar w body_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_asksig w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_askhop w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_askben w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_asknub w body_h_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_askras w body_i_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_benpod w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_bennub w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_benred w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_bender w body_b_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_barnub w body_o_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barben w body_o_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_bardig w body_o_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barras w body_e_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barmit w body_e_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barask w body_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_barsig w body_i_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_digras w body_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_digsig w body_d_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_dignub w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_digpod w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_dighat w body_f_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_dotben w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_dothat w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_dotask w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_dotras w body_b_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_hatask w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_hatnub w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_hatbuc w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_hatpod w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_hatdig w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_hatben w body_g_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_hatmit w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_hatras w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_hatpat w body_a_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_hopdax w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_hopdig w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_hopben w body_a_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_lomnub w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_lompod w body_a_tall    { $$ = _ycell($1, $3); }

    tall_norm: di_mitben w body_j_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitras w body_a_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitnub w body_k_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitbar w body_p_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitdot w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitdig w body_b_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitpod w body_c_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mithat w body_f_tall    { $$ = _ycell($1, $3); }
    tall_norm: di_mitsig w body_q_tall    { $$ = _ycell($1, $3); }

  /** Tall - bodies.
  **/
    body_a_tall: gene                       { $$ = $1; }
    body_b_tall: gene w gene                { $$ = _ycell($1, $3); }
    body_c_tall: gene w gene w gene         { $$ = _ytrel($1, $3, $5); }
    body_d_tall: bank_tall                  { $$ = $1; }
    body_e_tall: prop menu_tall             { $$ = _ycell($1, $2); }
    body_f_tall: gene w gene w gene w gene  { $$ = _yqual($1, $3, $5, $7); }
    body_g_tall: term w gene                { $$ = _ycell($1, $3); }
    body_h_tall: gene w rack_tall           { $$ = _ycell($1, $3); }
    body_i_tall: gene w bank_tall           { $$ = _ycell($1, $3); }
    body_j_tall: rope w rack_tall           { $$ = _ycell($1, $3); }
    body_k_tall: gene w gene                { $$ = _ytrel($1, $3, u4_nul); }
    body_o_tall: prop gene                  { $$ = _ycell($1, $2); }
    body_p_tall: rope w gene w rack_tall    { $$ = _ytrel($1, $3, $5); }
    body_q_tall: rope w gene w gene         { $$ = _ytrel($1, $3, $5); }

  /** Tall - body parts.
  **/
    bank_tall
      : tall_star e                     { $$ = $1; }
      ;
      tall_star
        :                               { $$ = u4_noun_0; }
        | gene w tall_star              { $$ = _ycell($1, $3); }
        ;

    rack_tall
      : tall_tall_star e                { $$ = $1; }
      ;
      tall_tall_star
        :                               { $$ = u4_noun_0; }
        | gene w gene w tall_tall_star  { $$ = _ycell(_ycell($1, $3), $5); }
        ;

    menu_tall
      : term_tall_star e                { $$ = $1; }
      ;
      term_tall_star
        :                               { $$ = u4_noun_0; }
        | term w gene w term_tall_star  { $$ = _ycell(_ycell($1, $3), $5); }
        ;
  

  /** Rope: reference path.
  **/
    rope
      : weir                    { $$ = u4_log_flip(yylane, $1); }
      ;

    weir
      : cord                    { $$ = _ycell($1, u4_noun_0); }
      | cord si_dot g weir      { $$ = _ycell($1, $4); }
      | bucs                    { $$ = $1; }
      | bucs si_dot g weir      { $$ = u4_log_cat(yylane, $1, $4); }
      ;

      bucs
        : si_buc                { $$ = _ycell(u4_noun_0, u4_noun_0); }
        | si_buc bucs           
          { $$ = _ytrel(u4_noun_0, 
                        _ycell(u4_atom_frag, u4_noun_2),
                        $2); }
        ;
      cord
        : axis                  { $$ = _ycell(u4_atom_frag, $1); }
        | tok_term              { $$ = _ycell(u4_atom_pane, $1); }
        ;

      axis
        : si_dot                { $$ = u4_noun_1; }
        | si_dot tok_delm       { $$ = $2; }
        | axis_beto             { $$ = $1; }
        ;
        axis_beto
          : si_nub              { $$ = u4_noun_2; }
          | si_pod              { $$ = u4_noun_3; }
          | si_nub axis_galu    { $$ = u4_op_peg(yylane, u4_noun_2, $2); }
          | si_pod axis_galu    { $$ = u4_op_peg(yylane, u4_noun_3, $2); }
          ;
        axis_galu
          : si_der              { $$ = u4_noun_2; }
          | si_red              { $$ = u4_noun_3; }
          | si_der axis_beto    { $$ = u4_op_peg(yylane, u4_noun_2, $2); }
          | si_red axis_beto    { $$ = u4_op_peg(yylane, u4_noun_3, $2); }
          ;

  /** Prop: jet reference.
  **/
    prop
      : si_dax si_lep rope ' ' '%' tok_term ' ' tok_delm si_pel w
        { $$ = _ytrel($3, $6, $8); }
      | { $$ = u4_nul; }
      ;

  /** Digraphs (with stem)
  **/
    di_askdig: si_ask si_dig  { $$ = u4_atom_trol; }
    di_askdot: si_ask si_dot  { $$ = u4_atom_lort; }
    di_askred: si_ask si_red  { $$ = u4_atom_gram; }
    di_askder: si_ask si_der  { $$ = u4_atom_marg; }
    di_askamp: si_ask si_amp  { $$ = u4_atom_chan; }
    di_askbar: si_ask si_bar  { $$ = u4_atom_dorn; }
    di_asksig: si_ask si_sig  { $$ = u4_atom_fent; }
    di_askhop: si_ask si_hop  { $$ = u4_atom_vern; }
    di_askben: si_ask si_ben  { $$ = u4_atom_plin; }
    di_asknub: si_ask si_nub  { $$ = u4_atom_grel; }
    di_askras: si_ask si_ras  { $$ = u4_atom_moze; }

    di_barnub: si_bar si_nub  { $$ = u4_atom_vamp; }
    di_barben: si_bar si_ben  { $$ = u4_atom_lome; }
    di_barras: si_bar si_ras  { $$ = u4_atom_pank; }
    di_barmit: si_bar si_mit  { $$ = u4_atom_tash; }
    di_bardig: si_bar si_dig  { $$ = u4_atom_sunt; }
    di_barask: si_bar si_ask  { $$ = u4_atom_rond; }
    di_barsig: si_bar si_sig  { $$ = u4_atom_lonk; }

    di_bender: si_ben si_der  { $$ = u4_atom_claf; }
    di_benred: si_ben si_red  { $$ = u4_atom_flac; }
    di_benpod: si_ben si_pod  { $$ = u4_atom_gant; }
    di_bennub: si_ben si_nub  { $$ = u4_atom_tang; }
 
    di_digras: si_dig si_ras  { $$ = u4_atom_prex; }
    di_digsig: si_dig si_sig  { $$ = u4_atom_slax; }
    di_dignub: si_dig si_nub  { $$ = u4_atom_twix; }
    di_digpod: si_dig si_pod  { $$ = u4_atom_trex; }
    di_dighat: si_dig si_hat  { $$ = u4_atom_quax; }

    di_dotben: si_dot si_ben  { $$ = u4_atom_sing; }
    di_dothat: si_dot si_hat  { $$ = u4_atom_vint; }
    di_dotask: si_dot si_ask  { $$ = u4_atom_dust; }
    di_dotras: si_dot si_ras  { $$ = u4_atom_sail; }
    
    di_hatask: si_hat si_ask  { $$ = u4_atom_hint; }
    di_hatnub: si_hat si_nub  { $$ = u4_atom_cast; }
    di_hatbuc: si_hat si_buc  { $$ = u4_atom_germ; }
    di_hatdig: si_hat si_dig  { $$ = u4_atom_stil; }
    di_hatpod: si_hat si_pod  { $$ = u4_atom_pock; }
    di_hatben: si_hat si_ben  { $$ = u4_atom_bran; }
    di_hatmit: si_hat si_mit  { $$ = u4_atom_velt; }
    di_hatras: si_hat si_ras  { $$ = u4_atom_mave; }
    di_hatpat: si_hat si_pat  { $$ = u4_atom_grit; }

    di_hopdax: si_hop si_dax  { $$ = u4_atom_zush; }
    di_hopmit: si_hop si_mit  { $$ = u4_atom_zoot; }
    di_hopven: si_hop si_hop  { $$ = u4_atom_zike; }
    di_hopdig: si_hop si_dig  { $$ = u4_atom_zalt; }
    di_hopben: si_hop si_ben  { $$ = u4_atom_zond; }
    
    di_lomnub: si_lom si_nub  { $$ = u4_atom_mast; }
    di_lompod: si_lom si_pod  { $$ = u4_atom_fist; }
    
    di_mitben: si_mit si_ben  { $$ = u4_atom_mack; }
    di_mitras: si_mit si_ras  { $$ = u4_atom_teck; }
    di_mitbar: si_mit si_bar  { $$ = u4_atom_gath; }
    di_mitsig: si_mit si_sig  { $$ = u4_atom_hang; }
    di_mitnub: si_mit si_nub  { $$ = u4_atom_fung; }
    di_mitdig: si_mit si_dig  { $$ = u4_atom_mung; }
    di_mitdot: si_mit si_dot  { $$ = u4_atom_gnum; }
    di_mitpod: si_mit si_pod  { $$ = u4_atom_bung; }
    di_mithat: si_mit si_hat  { $$ = u4_atom_tung; }

  /* Signs.
  */
    si_amp: '&'
    si_ask: '?'
    si_bar: '|'
    si_ben: '='
    si_buc: '$'
    si_bot: '\''
    si_cab: '_'
    /* si_com: ',' */
    si_dax: '#'
    si_dig: ':'
    si_der: '<'
    si_dot: '.'
    si_dus: '}'
    si_hat: '^'
    si_hop: '!'
    si_lom: ';'
    si_lep: '('
    si_mit: '%'
    si_mon: ']'
    si_nom: '['
    si_nub: '-'
    si_pat: '@'
    si_pel: ')'
    si_pod: '+'
    si_ras: '*'
    si_red: '>'
    si_sac: '\\'
    si_sol: '/'
    si_sud: '{'
    si_sig: '~'
    si_tic: '`'
    /* si_tep: '"' */

  /** Basic tokens.
  **/
    term
      : tok_term
      | si_buc    { $$ = u4_noun_0; }
      ;

    tok_term
      : tok_term_pre
      | tok_term_pre tok_term_load
        { $$ = u4_k_atom_log(yylane, _ycell($1, $2)); }
      ;
        tok_term_pre
          : ca

        tok_term_load
          : ca { $$ = _ycell($1, u4_noun_0); }
                 | cd { $$ = _ycell($1, u4_noun_0); }
                 | '-' { $$ = _ycell($1, u4_noun_0); }
                 | ca tok_term_load  { $$ = _ycell($1, $2); }
                 | cd tok_term_load  { $$ = _ycell($1, $2); }
                 | '-' tok_term_load { $$ = _ycell($1, $2); }
                 ;

    tok_chex 
      : '0' 
        { $$ = u4_noun_0; }
      | tok_chex_pre tok_chex_load
        { $$ = u4_k_atom_heximal(yylane, _ycell($1, $2)); }
      ;
        tok_chex_pre
          : cn | ch
          ;

        tok_chex_load
          : { $$ = u4_noun_0; }
                    | cd gap tok_chex_load { $$ = _ycell($1, $3); }
                    | ch gap tok_chex_load { $$ = _ycell($1, $3); }
                    ;


    tok_delm
      : '0' { $$ = u4_noun_0; }
      | tok_delm_pre tok_delm_load
        { $$ = u4_k_atom_decimal(yylane, _ycell($1, $2)); }
      ;
        tok_delm_pre: cn;
        tok_delm_load: { $$ = u4_noun_0; }
                    | cd tok_delm_load { $$ = _ycell($1, $2); }
                    ;

    tok_loct
      : si_bot loct_mid si_bot
        { $$ = u4_k_atom_log(yylane, $2); }
      ;
        loct_mid: { $$ = u4_noun_0; }
                 | cq loct_mid { $$ = _ycell($1, $2); }
                 ;


  /** Whitespace.
  **/
    gap
      : si_sac g si_sol  { $$ = u4_noun_0; }
      |                  { $$ = u4_noun_0; }
      ;

    g:            { $$ = u4_noun_0; }
     | cw g       { $$ = u4_noun_0; }
     | comment g  { $$ = u4_noun_0; }
     ;

    w: cw         { $$ = u4_noun_0; }
     | comment    { $$ = u4_noun_0; }
     | cw w       { $$ = u4_noun_0; }
     | comment w  { $$ = u4_noun_0; }
     ;

    e: '=' '='
        ;

    comment: ':' ':' comment_body '\n' { $$ = u4_noun_0; }
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
static u4_noun
_watt_locate(u4_lane lane,
             const void *vlocp,
             u4_noun gene)
{
  const YYLTYPE *llocp = vlocp;   /* bufalo estupido */

#if 0
  return gene;
#else
  return u4_k_trel
    (lane, 
     u4_atom_zemp, 
     u4_k_cell
      (lane, 
       u4_k_cell
        (lane, 
         u4_k_atom_xw(lane, llocp->first_line), 
         u4_k_atom_xw(lane, llocp->first_column)),
       u4_k_cell
        (lane,
         u4_k_atom_xw(lane, llocp->last_line), 
         u4_k_atom_xw(lane, llocp->last_column))),
     gene);
#endif
}

/* Initialize (scanner) for (site sack).
*/
static void
_scanner_init(struct _u4_scanner *scanner,
              u4_lane lane,
              u4_noun site,
              u4_noun sack)
{
  scanner->lane = lane;
  scanner->scan = u4_bull;

  scanner->p.site = site;
  if ( u4_n_atom(sack) ) {
    scanner->p.tube = sack;
    scanner->p.bowl = u4_noun_0;
  }
  else {
    scanner->p.tube = u4_ch(sack);
    scanner->p.bowl = u4_ct(sack);
  }

  scanner->s.token = 0;
  scanner->s.pb = 0;
  scanner->s.xw_line = 1;
  scanner->s.xw_col = 1;
}

/* _watt_parse_part():
*/
static u4_noun
_watt_parse_part(struct _u4_scanner *scanner,
                 u4_noun term)
{
  u4_lane lane = scanner->lane;
  u4_log  site = scanner->p.site;
  u4_tab  bowl = scanner->p.bowl;
  u4_noun sack;

  if ( !u4_tab_in(term, bowl) ) {
    u4_err(lane, "part", term);
    return u4_exit;
  }
  sack = u4_tab_get(term, bowl);
  site = u4_k_cell(lane, term, site);

  /* Is bison really reentrant?
  */
  return u4_watt_parse(lane, site, sack);
}

/* u4_watt_parse(): 
**
**   At (pif), convert (zar) to a gene.
*/
u4_noun
u4_watt_parse(u4_lane lane,
              u4_noun pif,
              u4_noun zar)
{
  struct _u4_scanner scanner;

  _scanner_init(&scanner, lane, pif, zar);

  if ( !yyparse(&scanner) ) {
    u4_assert(scanner.scan);

    return scanner.scan;
    // return u4_k_trel(lane, u4_atom_home, pif, scanner.scan);
  }
  else {
    return u4_exit;
  }
}

/* u4_unix_path_len():
**
**  Measure abstract Watt gene path.
*/
uint32_t
u4_unix_path_len(u4_noun fud)
{
  if ( u4_n_atom(fud) ) {
    return u4_a_bin(fud, 3);
  }
  else {
    u4_assert(u4_n_atom(u4_ch(fud)));

    return u4_a_bin(u4_ch(fud), 3) + 1 + u4_unix_path_len(u4_ct(fud));
  }
}

/* u4_unix_path_copy():
**
**  Copy abstract Watt gene path.  Produces u4_unix_path_len().
*/
uint32_t
u4_unix_path_copy(u4_noun fud,
                  char *cl_path)
{
  if ( u4_n_atom(fud) ) {
    uint32_t ben = u4_a_bin(fud, 3);

    u4_a_bytes(fud, (uint8_t *)cl_path, 0, ben);
    return ben;
  }
  else {
    u4_noun hed = u4_ch(fud);
    uint32_t ben = u4_a_bin(hed, 3);

    u4_a_bytes(hed, (uint8_t *)cl_path, 0, ben);
    cl_path[ben] = '/';
    
    return ben + 1 + u4_unix_path_copy(u4_ct(fud), cl_path + ben + 1);
  }
}
 
/* u4_unix_path_get():
**
**  Return Watt gene path as a malloced string.
*/
u4_cl *
u4_unix_path_get(u4_noun fud)
{
  uint32_t len = u4_unix_path_len(fud);
  char *cl_path = malloc(len + 1);

  cl_path[len] = 0;
  u4_unix_path_copy(fud, cl_path);
  cl_path[len] = 0;

  return cl_path;
}

/* u4_unix_path_file():
**
**  Load abstract Watt gene path as a file.
**  XX: should use environment variable.
*/
u4_noun
u4_unix_path_file(u4_lane lane,
                  u4_noun fud)
{
  u4_cl buf[1024];
  u4_cl *cl_path = u4_unix_path_get(fud);
  u4_noun text;

  sprintf(buf, "pro/%s.watt", cl_path);
  text = u4_disk_read_file(lane, buf);

  free(cl_path);
  return text;
}

/* u4_unix_path_watt():
**
**  Load abstract Watt path as a Watt gene.
*/
u4_noun
u4_unix_path_watt(u4_lane lane,
                  u4_noun fud)
{
  u4_noun text = u4_unix_path_file(lane, fud);
  u4_noun gene = u4_watt_parse(lane, fud, text);

  return gene;
}

/* u4_vere_parse(): 
**
**   Convert (mez) to a vere command.
*/
u4_noun
u4_vere_parse(u4_lane lane,
              u4_atom mez)
{
  struct _u4_scanner scanner;

  _scanner_init(&scanner, lane, u4_noun_0, mez);
  scanner.s.token = T_vere;

  if ( !yyparse(&scanner) ) {
    u4_assert(scanner.scan);

    return scanner.scan;
  }
  else {
    return u4_exit;
  }
}

/* u4_hume_parse(): 
**
**   Convert (mez) to a data noun.
*/
u4_noun
u4_hume_parse(u4_lane lane,
              u4_atom mez)
{
  struct _u4_scanner scanner;

  _scanner_init(&scanner, lane, u4_noun_0, mez);
  scanner.s.token = T_hume;

  if ( !yyparse(&scanner) ) {
    u4_assert(scanner.scan);

    return scanner.scan;
  }
  else {
    return u4_exit;
  }
}

/* Trivial scanner.
*/
int 
yylex(YYSTYPE *lvalp, YYLTYPE *llocp, struct _u4_scanner *scanner)
{
  if ( scanner->s.token ) {
    int token = scanner->s.token;

    scanner->s.token = 0;
    return token;
  }
  else {
    u4_xb xb = u4_a_byte(scanner->p.tube, scanner->s.pb);

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

    *lvalp = u4_cod_in(xb);
    return xb;
  }
}  

/* Error printer.
*/
int yyerror(YYLTYPE *locp, struct _u4_scanner *scanner, char const *msg)
{
  if ( !u4_n_zero(scanner->p.site) ) {
    u4_err(scanner->lane, "yyerror", scanner->p.site);
  }
  printf("%s: (%d:%d - %d:%d)\n", 
    msg, locp->first_line, locp->first_column,
         locp->last_line, locp->last_column);
}
