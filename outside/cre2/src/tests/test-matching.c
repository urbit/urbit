/*
  Part of: CRE2
  Contents: test for matching
  Date: Mon Jan  2, 2012

  Abstract

	Test file for regular expressions matching.

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
  const char *		pattern;

/* ------------------------------------------------------------------ */
/* single match */

  pattern = "ciao";
  opt     = cre2_opt_new();
  cre2_opt_set_posix_syntax(opt, 1);
  rex = cre2_new(pattern, strlen(pattern), opt);
  {
    if (cre2_error_code(rex))
      goto error;
    cre2_string_t	match;
    int			nmatch = 1;
    int			e;
    const char *	text = "ciao";
    int			text_len = strlen(text);

    e = cre2_match(rex, text, text_len, 0, text_len, CRE2_UNANCHORED, &match, nmatch);
    if (1 != e)
      goto error;
    PRINTF("match: retval=%d, ", e);
    FWRITE(match.data, match.length, 1, stdout);
    PRINTF("\n");
  }
  cre2_delete(rex);
  cre2_opt_delete(opt);

/* ------------------------------------------------------------------ */
/* two groups */

  pattern = "(ciao) (hello)";
  opt = cre2_opt_new();
  rex = cre2_new(pattern, strlen(pattern), opt);
  {
    if (cre2_error_code(rex))
      goto error;
    int			nmatch = 3;
    cre2_string_t	strings[nmatch];
    cre2_range_t	ranges[nmatch];
    int			e;
    const char *	text = "ciao hello";
    int			text_len = strlen(text);

    e = cre2_match(rex, text, text_len, 0, text_len, CRE2_UNANCHORED, strings, nmatch);
    if (1 != e)
      goto error;
    cre2_strings_to_ranges(text, ranges, strings, nmatch);
    PRINTF("full match: ");
    FWRITE(text+ranges[0].start, ranges[0].past-ranges[0].start, 1, stdout);
    PRINTF("\n");
    PRINTF("first group: ");
    FWRITE(text+ranges[1].start, ranges[1].past-ranges[1].start, 1, stdout);
    PRINTF("\n");
    PRINTF("second group: ");
    FWRITE(text+ranges[2].start, ranges[2].past-ranges[2].start, 1, stdout);
    PRINTF("\n");
  }
  cre2_delete(rex);
  cre2_opt_delete(opt);

/* ------------------------------------------------------------------ */
/* test literal option */

  pattern = "(ciao) (hello)";
  opt = cre2_opt_new();
  cre2_opt_set_literal(opt, 1);
  rex = cre2_new(pattern, strlen(pattern), opt);
  {
    if (cre2_error_code(rex))
      goto error;
    int			nmatch = 0;
    int			e;
    const char *	text = "(ciao) (hello)";
    int			text_len = strlen(text);
    e = cre2_match(rex, text, text_len, 0, text_len, CRE2_UNANCHORED, NULL, nmatch);
    if (0 == e)
      goto error;
  }
  cre2_delete(rex);
  cre2_opt_delete(opt);

/* ------------------------------------------------------------------ */

  exit(EXIT_SUCCESS);
 error:
  exit(EXIT_FAILURE);
}

/* end of file */
