/* j/5/repg.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"
#include "cre2.h"
#include <string.h>

  u2_noun                                                         //  produce
  j2_mbc(Pt5, repg)(u2_wire wir_r, 
                    u2_noun lub,
                    u2_noun rad,
                    u2_noun rep)                                  //  retain
  {
    c3_y* lub_y = u2_cr_tape(lub);
    c3_y* rad_y = u2_cr_tape(rad);
    c3_y* rep_y = u2_cr_tape(rep);

    

    char* rec = (char*)lub_y;
    char* end;
    while(*rec != 0) {
      if(*rec == '\\') {
        rec++; 
        switch (*rec) {
        case 'P':
        case 'p':
          free(lub_y);
          free(rad_y);
          return u2_nul;
        case 'Q':
          end = strstr(rec, "\\E");
          if(end == NULL) rec += strlen(rec) - 1;
          else rec = end;
        }
        rec++;
      }
      else if(*rec == '(') {
        rec++;
        if(*rec == '?') {
          rec++;
          if(*rec != ':') {
            free(lub_y);
            free(rad_y);
            return u2_nul;
          }
          rec++;
        }
      }
      else 
        rec++;
    }
    
    fprintf(stderr, "\r\nrepg: \r\n%s : %s\r\n", lub_y, rad_y);

    cre2_regexp_t * rex;
    cre2_options_t * opt;
    
    opt = cre2_opt_new();
    if (opt) {
      cre2_opt_set_log_errors(opt, 0);
      cre2_opt_set_encoding(opt, CRE2_Latin1);
      cre2_opt_set_perl_classes(opt, 1);
      cre2_opt_set_one_line(opt, 1);
      cre2_opt_set_longest_match(opt, 1);
      rex = cre2_new((const char *)lub_y, strlen((char *)lub_y), opt);
      if (rex) {
        if (!cre2_error_code(rex)) {
          int text_len = strlen((char *)rad_y);
          cre2_string_t matches[1];
          int ic = 0;

          u2_noun ret = u2_nul;
          while (ic <= text_len) {
            int match = cre2_match(rex, (const char*)rad_y, text_len, ic, text_len, CRE2_ANCHOR_START, matches, 1);

            if (!match) {
              if(rad_y[ic])
                ret = u2_cn_cell((u2_atom)rad_y[ic], ret);
              ic++;
            }
            else {
              int mlen = matches[0].length;
              if (mlen == 0) {
                ret = u2_ckb_weld(u2_ckb_flop(u2_ci_tape((char *) rad_y+ic)), u2_ckb_flop(u2_ci_tape((char *)rep_y)));
                ic = text_len + 1;
              }
              else {
                ret = u2_ckb_weld(u2_ckb_flop(u2_ci_tape((char *)rep_y)), ret);
                ic += mlen;
              }
            }
          }
          cre2_opt_delete(opt);
          cre2_delete(rex);
          free(lub_y);
          free(rad_y);
          free(rep_y);
          return u2_cn_cell(u2_nul, u2_ckb_flop(ret));
        }
        else {
          // Compiling the regular expression failed
          cre2_opt_delete(opt);
          cre2_delete(rex);
          free(lub_y);
          free(rad_y);
          return u2_nul;
        }
        cre2_opt_delete(opt);
        cre2_delete(rex);
      }
      else {
        // rex Allocation Error
        cre2_opt_delete(opt);
        free(lub_y);
        free(rad_y);
        u2_bl_bail(wir_r, c3__exit);
      }
      cre2_opt_delete(opt);
    }
    // opt Allocation Error
    free(lub_y);
    free(rad_y);
    u2_bl_bail(wir_r, c3__exit);
    return u2_nul;
  }

  u2_weak                                                         //  produce
  j2_mb(Pt5, repg)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun lub;
    u2_noun rad;
    u2_noun rep;

    if ( (u2_none == (lub = u2_frag(u2_cv_sam_2, cor))) ||
         (u2_none == (rad = u2_frag(u2_cv_sam_6, cor))) ||
         (u2_none == (rep = u2_frag(u2_cv_sam_7, cor))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mbc(Pt5, repg)(wir_r, lub, rad, rep);
    }
  }


/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt5, repg)[] = { 
    { ".2", c3__lite, j2_mb(Pt5, repg), u2_jet_live | u2_jet_test, u2_none, u2_none },
    { }
  };
