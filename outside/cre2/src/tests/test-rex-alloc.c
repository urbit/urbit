/*
  Part of: CRE2
  Contents: test for rex allocation
  Date: Mon Jan  2, 2012

  Abstract

	Test file for regular expressions allocation.

  Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>

  See the COPYING file.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <cre2.h>

#if 0
#  define PRINTF		printf
#  define FWRITE		fwrite
#else
#  define PRINTF(MSG, ...)	/* empty string */
#  define FWRITE(BUF, ...)	/* empty string */
#endif

int
main (int argc, const char *const argv[])
{
  cre2_regexp_t *	rex;
  cre2_options_t *	opt;
  opt = cre2_opt_new();
  cre2_opt_set_posix_syntax(opt, 1);
  rex = cre2_new("ciao", 4, opt);
  {
    cre2_string_t	S;
    PRINTF("pattern: %s\n", cre2_pattern(rex));
    PRINTF("error code: %d\n", cre2_error_code(rex));
    PRINTF("error string: \"%s\"\n", cre2_error_string(rex));
    PRINTF("number of capturing groups: %d\n", cre2_num_capturing_groups(rex));
    PRINTF("program size: %d\n", cre2_program_size(rex));
    cre2_error_arg(rex, &S);
    PRINTF("error arg: len=%d, data=\"%s\"\n", S.length, S.data);
    if (cre2_error_code(rex))
      goto error;
    if (cre2_num_capturing_groups(rex))
      goto error;
    if (cre2_error_code(rex))
      goto error;
    if (0 != strlen(cre2_error_string(rex)))
      goto error;
    if (0 != S.length)
      goto error;
  }
  cre2_delete(rex);
  cre2_opt_delete(opt);

/* ------------------------------------------------------------------ */
/* no options object */

  rex = cre2_new("ciao", 4, NULL);
  {
    if (cre2_error_code(rex))
      goto error;
  }
  cre2_delete(rex);

/* ------------------------------------------------------------------ */

  opt = cre2_opt_new();
  cre2_opt_set_posix_syntax(opt, 1);
  rex = cre2_new("ci(ao)", 6, opt);
  {
    PRINTF("error code: %d\n", cre2_error_code(rex));
    PRINTF("number of capturing groups: %d\n", cre2_num_capturing_groups(rex));
    PRINTF("program size: %d\n", cre2_program_size(rex));
    if (cre2_error_code(rex))
      goto error;
    if (1 != cre2_num_capturing_groups(rex))
      goto error;
  }
  cre2_delete(rex);
  cre2_opt_delete(opt);

/* ------------------------------------------------------------------ */

  opt = cre2_opt_new();
  cre2_opt_set_log_errors(opt, 0);
  rex = cre2_new("ci(ao", 5, opt);
  {
    int			code = cre2_error_code(rex);
    const char *	msg  = cre2_error_string(rex);
    cre2_string_t	S;
    cre2_error_arg(rex, &S);
    if (CRE2_ERROR_MISSING_PAREN != code)
      goto error;
    if (! msg)
      goto error;
    PRINTF("pattern: %s\n", cre2_pattern(rex));
    PRINTF("error: code=%d, msg=\"%s\"\n", code, msg);
    PRINTF("error arg: len=%d, data=\"%s\"\n", S.length, S.data);
  }
  cre2_delete(rex);
  cre2_opt_delete(opt);

  exit(EXIT_SUCCESS);

 error:
  exit(EXIT_FAILURE);
}

/* end of file */
