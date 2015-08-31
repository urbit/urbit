/* j/5/repg.c
**
*/
#include "all.h"

#include "cre2.h"
#include <string.h>

  u3_noun
  u3qe_repg(u3_noun lub,
            u3_noun rad,
            u3_noun rep)
  {
    c3_y* lub_y = u3r_tape(lub);
    c3_y* rad_y = u3r_tape(rad);
    c3_y* rep_y = u3r_tape(rep);


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
          return u3_nul;
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
            return u3_nul;
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
          cre2_string_t matches[1];
          int ic = 0;

          u3_noun ret = u3_nul;
          while (ic <= text_len) {
            int match = cre2_match(rex, (const char*)rad_y, text_len, ic, text_len, CRE2_ANCHOR_START, matches, 1);

            if (!match) {
              if(rad_y[ic])
                ret = u3i_cell((c3_y)rad_y[ic], ret);
              ic++;
            }
            else {
              int mlen = matches[0].length;
              if (mlen == 0) {
                ret = u3kb_weld(u3kb_flop(u3i_tape((char *) rad_y+ic)), u3kb_flop(u3i_tape((char *)rep_y)));
                ic = text_len + 1;
              }
              else {
                ret = u3kb_weld(u3kb_flop(u3i_tape((char *)rep_y)), ret);
                ic += mlen;
              }
            }
          }
          cre2_opt_delete(opt);
          cre2_delete(rex);
          free(lub_y);
          free(rad_y);
          free(rep_y);
          return u3i_cell(u3_nul, u3kb_flop(ret));
        }
        else {
          // Compiling the regular expression failed
          cre2_opt_delete(opt);
          cre2_delete(rex);
          free(lub_y);
          free(rad_y);
          return u3_nul;
        }
        cre2_opt_delete(opt);
        cre2_delete(rex);
      }
      else {
        // rex Allocation Error
        cre2_opt_delete(opt);
        free(lub_y);
        free(rad_y);
        u3m_bail(c3__exit);
      }
      cre2_opt_delete(opt);
    }
    // opt Allocation Error
    free(lub_y);
    free(rad_y);
    u3m_bail(c3__exit);
    return u3_nul;
  }

  u3_noun
  u3we_repg(u3_noun cor)
  {
    u3_noun lub;
    u3_noun rad;
    u3_noun rep;

    if ( (u3_none == (lub = u3r_at(u3x_sam_2, cor))) ||
         (u3_none == (rad = u3r_at(u3x_sam_6, cor))) ||
         (u3_none == (rep = u3r_at(u3x_sam_7, cor))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qe_repg(lub, rad, rep);
    }
  }
