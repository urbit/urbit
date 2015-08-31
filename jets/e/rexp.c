/* j/5/rexp.c
**
*/
#include "all.h"

#include "cre2.h"
#include <string.h>

  u3_noun
  u3qe_rexp(u3_noun lub,
            u3_noun rad)
  {
    c3_y* lub_y = u3r_tape(lub);
    c3_y* rad_y = u3r_tape(rad);

    u3k(lub);
    int lub_l = u3kb_lent(lub);
    if (lub_l != strlen((char *)lub_y)) {
      free(lub_y);
      free(rad_y);
      return u3_nul;
    }

    char* rec = (char*)lub_y;
    char* end;
    while(*rec != 0) {
      if(*rec > 127) {
        free(lub_y);
        free(rad_y);
        return u3_nul;
      }
      else if(*rec == '\\') {
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
          int captures = cre2_num_capturing_groups(rex);
          cre2_string_t matches[captures+1];

          int match = cre2_match(rex, (const char*)rad_y, text_len, 0, text_len, CRE2_UNANCHORED, matches, captures+1);

          if (!match) {
            // No matches
            cre2_opt_delete(opt);
            cre2_delete(rex);
            free(lub_y);
            free(rad_y);
            return u3i_cell(u3_nul, u3_nul);
          }

          u3_noun map = u3_nul;

          int i;
          for (i = 0; i < captures+1; i++) {
            char * buf = malloc(matches[i].length + 1);
            memcpy(buf, matches[i].data, matches[i].length);
            buf[matches[i].length] = 0;
            map = u3kdb_put(map, i, u3i_tape(buf));
            free(buf);
          }

          cre2_opt_delete(opt);
          cre2_delete(rex);
          free(lub_y);
          free(rad_y);
          return u3i_cell(u3_nul, u3i_cell(u3_nul, map));

        }
        else {
          // Compiling the regular expression failed
          cre2_opt_delete(opt);
          cre2_delete(rex);
          free(lub_y);
          free(rad_y);
          return u3_nul;
        }
        cre2_delete(rex);
      }
      cre2_opt_delete(opt);
    }
    free(lub_y);
    free(rad_y);
    u3m_bail(c3__exit);
    return u3_nul;
  }

  u3_noun
  u3we_rexp(u3_noun cor)
  {
    u3_noun lub;
    u3_noun rad;

    if ( (u3_none == (lub = u3r_at(u3x_sam_2, cor))) ||
         (u3_none == (rad = u3r_at(u3x_sam_3, cor))) )
    {
      return u3m_bail(c3__fail);
    } else {
      return u3qe_rexp(lub, rad);
    }
  }
