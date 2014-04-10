/* j/5/rexp.c
**
** This file is in the public domain.
*/
#include "all.h"
#include "../pit.h"
#include "cre2.h"
#include <string.h>

  u2_noun                                                         //  produce
  j2_mbc(Pt5, rexp)(u2_wire wir_r, 
                    u2_noun lub,
                    u2_noun rad)                                  //  retain
  {
    c3_y* lub_y = u2_cr_tape(lub);
    c3_y* rad_y = u2_cr_tape(rad);

    u2k(lub);
    int lub_l = u2_ckb_lent(lub);
    if (lub_l != strlen((char *)lub_y)) {
      free(lub_y);
      free(rad_y);
      return u2_nul;
    }

    char* rec = (char*)lub_y;
    char* end;
    while(*rec != 0) {
      if(*rec > 127) {
        free(lub_y);
        free(rad_y);
        return u2_nul;
      }
      else if(*rec == '\\') {
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
    
    cre2_regexp_t * rex;
    cre2_options_t * opt;
    
    opt = cre2_opt_new();
    if (opt) {
      cre2_opt_set_log_errors(opt, 0);
      cre2_opt_set_encoding(opt, CRE2_UTF8);
      cre2_opt_set_perl_classes(opt, 1);
      cre2_opt_set_one_line(opt, 1);
      cre2_opt_set_longest_match(opt, 1);
      rex = cre2_new((const char *)lub_y, strlen((char *)lub_y), opt);
      if (rex) {
        if (!cre2_error_code(rex)) {
          int text_len = strlen((char *)rad_y);
          int captures = cre2_num_capturing_groups(rex);
          cre2_string_t matches[captures+1];

          int match = cre2_match(rex, (const char*)rad_y, text_len, 0, text_len, CRE2_UNANCHORED, matches, captures+1);
    
          if (!match) {
            // No matches
            cre2_opt_delete(opt);
            cre2_delete(rex);
            free(lub_y);          
            free(rad_y);
            return u2_cn_cell(u2_nul, u2_nul);
          }
    
          u2_noun map = u2_nul;

          int i;
          for (i = 0; i < captures+1; i++) {
            char * buf = malloc(matches[i].length + 1);
            memcpy(buf, matches[i].data, matches[i].length);
            buf[matches[i].length] = 0;
            map = u2_ckd_by_put(map, i, u2_ci_tape(buf));
            free(buf);
          }

          cre2_opt_delete(opt);
          cre2_delete(rex);
          free(lub_y);          
          free(rad_y);
          return u2_cn_cell(u2_nul, u2_cn_cell(u2_nul, map));
          
        }
        else {
          // Compiling the regular expression failed
          cre2_opt_delete(opt);
          cre2_delete(rex);
          free(lub_y);          
          free(rad_y);
          return u2_nul;
        }
        cre2_delete(rex);
      }
      cre2_opt_delete(opt);
    }
    free(lub_y);
    free(rad_y);
    u2_bl_bail(wir_r, c3__exit);
    return u2_nul;
  }

  u2_weak                                                         //  produce
  j2_mb(Pt5, rexp)(u2_wire wir_r, 
                   u2_noun cor)                                   //  retain
  {
    u2_noun lub;
    u2_noun rad;

    if ( (u2_none == (lub = u2_frag(u2_cv_sam_2, cor))) ||
         (u2_none == (rad = u2_frag(u2_cv_sam_3, cor))) )
    {
      return u2_bl_bail(wir_r, c3__fail);
    } else {
      return j2_mbc(Pt5, rexp)(wir_r, lub, rad);
    }
  }


/* structures
*/
  u2_ho_jet 
  j2_mbj(Pt5, rexp)[] = { 
    { ".2", c3__lite, j2_mb(Pt5, rexp), Tier5, u2_none, u2_none },
    { }
  };
