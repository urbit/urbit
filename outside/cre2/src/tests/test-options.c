/*
  Part of: CRE2
  Contents: test for options
  Date: Mon Jan  2, 2012

  Abstract

	Test file for options objects.

  Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>

  See the COPYING file.
*/

#include <stdio.h>
#include <stdlib.h>
#include <cre2.h>

int
main (int argc, const char *const argv[])
{
  cre2_options_t *	opt;

  opt = cre2_opt_new();
  {
    cre2_opt_set_posix_syntax(opt, 1);
    cre2_opt_set_longest_match(opt, 1);
    cre2_opt_set_log_errors(opt, 1);
    cre2_opt_set_literal(opt, 1);
    cre2_opt_set_never_nl(opt, 1);
    cre2_opt_set_case_sensitive(opt, 1);
    cre2_opt_set_perl_classes(opt, 1);
    cre2_opt_set_word_boundary(opt, 1);
    cre2_opt_set_one_line(opt, 1);
    cre2_opt_set_encoding(opt, CRE2_UTF8);
    cre2_opt_set_encoding(opt, CRE2_Latin1);
    cre2_opt_set_max_mem(opt, 4096);
  }
  cre2_opt_delete(opt);
  exit(EXIT_SUCCESS);
}

/* end of file */
