/*
  Part of: CRE2
  Contents: test for miscellaneous functions
  Date: Wed Jan  4, 2012

  Abstract

	Test file for miscellaneous functions.

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
  { /* quote meta characters */
    const char *	pattern	 = "1.5-2.0?";
    cre2_string_t	original = {
      .data   = pattern,
      .length = strlen(pattern)
    };
    cre2_string_t	quoted;
    int			result;
    result = cre2_quote_meta(&quoted, &original);
    if (0 != result)
	goto error;
    if (0 != strncmp("1\\.5\\-2\\.0\\?", quoted.data, quoted.length))
      goto error;
    free((void *)quoted.data);
  }

  /* ------------------------------------------------------------------ */

  { /* minimum and maximum matching strings */
    const char *	pattern = "(?i)ABCdef";
    cre2_regexp_t *	rex;
    cre2_string_t	min, max;
    int			result;
    rex = cre2_new(pattern, strlen(pattern), NULL);
    {
      result = cre2_possible_match_range(rex, &min, &max, 1024);
      if (1 != result)
      	goto error;
      if (0 != strncmp("ABCDEF", min.data, min.length))
	goto error;
      if (0 != strncmp("abcdef", max.data, max.length))
	goto error;
    }
    cre2_delete(rex);
    free((void *)min.data);
    free((void *)max.data);
  }

  /* ------------------------------------------------------------------ */

  { /* successfully check rewrite string */
    const char *	pattern = "a(b)c";
    const char *	subst   = "def";
    cre2_string_t	rewrite = {
      .data	= subst,
      .length	= strlen(subst)
    };
    cre2_regexp_t *	rex;
    cre2_string_t	errmsg;
    int			result;
    rex = cre2_new(pattern, strlen(pattern), NULL);
    {
      result = cre2_check_rewrite_string(rex, &rewrite, &errmsg);
      if (1 != result)
      	goto error;
    }
    cre2_delete(rex);
  }
  { /* failed check rewrite string */
    const char *	pattern = "a(b)c";
    const char *	subst   = "\\1 \\2";
    cre2_string_t	rewrite = {
      .data	= subst,
      .length	= strlen(subst)
    };
    cre2_regexp_t *	rex;
    cre2_string_t	errmsg;
    int			result;
    rex = cre2_new(pattern, strlen(pattern), NULL);
    {
      result = cre2_check_rewrite_string(rex, &rewrite, &errmsg);
      if (0 != result)
      	goto error;
      PRINTF("error message: ");
      FWRITE(errmsg.data, errmsg.length, 1, stdout);
      PRINTF("\n");
    }
    cre2_delete(rex);
    free((void *)errmsg.data);
  }

/* ------------------------------------------------------------------ */

  exit(EXIT_SUCCESS);
 error:
  exit(EXIT_FAILURE);
}

/* end of file */
