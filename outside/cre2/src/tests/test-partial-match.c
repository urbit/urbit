/*
  Part of: CRE2
  Contents: test for partial match function
  Date: Tue Jan  3, 2012

  Abstract

	Test file for partial match function.

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
  { /* success, no parentheses */
    const char *	pattern = "ci.*ut";
    const char *	text	= "pre ciao salut post";
    cre2_string_t	input   = { .data = text, .length = strlen(text) };
    int			result;
    result = cre2_partial_match(pattern, &input, NULL, 0);
    if (! result)
      goto error;
    if (0 != strncmp(text, input.data, input.length))
      goto error;
  }
  { /* success, one parenthetical subexpression, one match entry */
    const char *	pattern = "(ciao) salut";
    const char *	text	= "ciao salut";
    cre2_string_t	input   = { .data = text, .length = strlen(text) };
    int			nmatch  = 1;
    cre2_string_t	match[nmatch];
    int			result;
    result = cre2_partial_match(pattern, &input, match, nmatch);
    if (! result)
      goto error;
    if (0 != strncmp(text, input.data, input.length))
      goto error;
    if (0 != strncmp("ciao", match[0].data, match[0].length))
      goto error;
    PRINTF("match 0: ");
    FWRITE(match[0].data, match[0].length, 1, stdout);
    PRINTF("\n");
  }
  { /* success, two parenthetical subexpressions, two match entries */
    const char *	pattern = "(ciao) (salut)";
    const char *	text	= "ciao salut";
    cre2_string_t	input   = { .data = text, .length = strlen(text) };
    int			nmatch  = 2;
    cre2_string_t	match[nmatch];
    int			result;
    result = cre2_partial_match(pattern, &input, match, nmatch);
    if (! result)
      goto error;
    if (0 != strncmp(text, input.data, input.length))
      goto error;
    if (0 != strncmp("ciao", match[0].data, match[0].length))
      goto error;
    if (0 != strncmp("salut", match[1].data, match[1].length))
      goto error;
    PRINTF("match 0: ");
    FWRITE(match[0].data, match[0].length, 1, stdout);
    PRINTF("\n");
    PRINTF("match 1: ");
    FWRITE(match[1].data, match[1].length, 1, stdout);
    PRINTF("\n");
  }
  { /* failure, no parentheses */
    const char *	pattern = "ci.*ut";
    const char *	text	= "ciao hello";
    cre2_string_t	input   = { .data = text, .length = strlen(text) };
    int			result;
    result = cre2_partial_match(pattern, &input, NULL, 0);
    if (result)
      goto error;
  }
  { /* failure, one parenthetical subexpression */
    const char *	pattern = "(ciao) salut";
    const char *	text	= "ciao hello";
    cre2_string_t	input   = { .data = text, .length = strlen(text) };
    int			nmatch  = 1;
    cre2_string_t	match[nmatch];
    int			result;
    result = cre2_partial_match(pattern, &input, match, nmatch);
    if (result)
      goto error;
    if (0 != strncmp(text, input.data, input.length))
      goto error;
  }
  { /* success, one parenthetical subexpression, no match entries */
    const char *	pattern = "(ciao) salut";
    const char *	text	= "ciao salut";
    cre2_string_t	input   = { .data = text, .length = strlen(text) };
    int			result;
    result = cre2_partial_match(pattern, &input, NULL, 0);
    if (! result)
      goto error;
    if (0 != strncmp(text, input.data, input.length))
      goto error;
  }
  { /* failure, one parenthetical subexpression, two match entries */
    const char *	pattern = "(ciao) salut";
    const char *	text	= "ciao salut";
    cre2_string_t	input   = { .data = text, .length = strlen(text) };
    int			nmatch  = 2;
    cre2_string_t	match[nmatch];
    int			result;
    memset(match, '\0', nmatch * sizeof(cre2_string_t));
    result = cre2_partial_match(pattern, &input, match, nmatch);
    if (0 != result)
      goto error;
  }
  { /* success, two parenthetical subexpressions, one match entry */
    const char *	pattern = "(ciao) (salut)";
    const char *	text	= "ciao salut";
    cre2_string_t	input   = { .data = text, .length = strlen(text) };
    int			nmatch  = 1;
    cre2_string_t	match[nmatch];
    int			result;
    result = cre2_partial_match(pattern, &input, match, nmatch);
    if (! result)
      goto error;
    if (0 != strncmp("ciao", match[0].data, match[0].length))
      goto error;
    if (0 != strncmp(text, input.data, input.length))
      goto error;
    PRINTF("match 0: ");
    FWRITE(match[0].data, match[0].length, 1, stdout);
    PRINTF("\n");
  }
  { /* wrong regexp specification */
    const char *	pattern = "cia(o salut";
    const char *	text	= "ciao hello";
    cre2_string_t	input   = { .data = text, .length = strlen(text) };
    int			nmatch  = 1;
    cre2_string_t	match[nmatch];
    int			result;
    result = cre2_partial_match(pattern, &input, match, nmatch);
    if (0 != result)
      goto error;
    if (0 != strncmp(text, input.data, input.length))
      goto error;
  }


/* ------------------------------------------------------------------ */

  { /* success, no parentheses */
    const char *	pattern = "ci.*ut";
    cre2_regexp_t *	rex;
    const char *	text	= "ciao salut";
    cre2_string_t	input   = { .data = text, .length = strlen(text) };
    int			result;
    rex    = cre2_new(pattern, strlen(pattern), NULL);
    result = cre2_partial_match_re(rex, &input, NULL, 0);
    cre2_delete(rex);
    if (! result)
      goto error;
    if (0 != strncmp(text, input.data, input.length))
      goto error;
  }
  { /* success, one parenthetical subexpression, one match entry */
    const char *	pattern = "(ciao) salut";
    cre2_regexp_t *	rex;
    const char *	text	= "ciao salut";
    cre2_string_t	input   = { .data = text, .length = strlen(text) };
    int			nmatch  = 1;
    cre2_string_t	match[nmatch];
    int			result;
    rex    = cre2_new(pattern, strlen(pattern), NULL);
    result = cre2_partial_match_re(rex, &input, match, nmatch);
    cre2_delete(rex);
    if (! result)
      goto error;
    if (0 != strncmp(text, input.data, input.length))
      goto error;
    if (0 != strncmp("ciao", match[0].data, match[0].length))
      goto error;
    PRINTF("match 0: ");
    FWRITE(match[0].data, match[0].length, 1, stdout);
    PRINTF("\n");
  }
  { /* success, two parenthetical subexpressions, two match entries */
    const char *	pattern = "(ciao) (salut)";
    cre2_regexp_t *	rex;
    const char *	text	= "ciao salut";
    cre2_string_t	input   = { .data = text, .length = strlen(text) };
    int			nmatch  = 2;
    cre2_string_t	match[nmatch];
    int			result;
    rex    = cre2_new(pattern, strlen(pattern), NULL);
    result = cre2_partial_match_re(rex, &input, match, nmatch);
    cre2_delete(rex);
    if (! result)
      goto error;
    if (0 != strncmp(text, input.data, input.length))
      goto error;
    if (0 != strncmp("ciao", match[0].data, match[0].length))
      goto error;
    if (0 != strncmp("salut", match[1].data, match[1].length))
      goto error;
    PRINTF("match 0: ");
    FWRITE(match[0].data, match[0].length, 1, stdout);
    PRINTF("\n");
    PRINTF("match 1: ");
    FWRITE(match[1].data, match[1].length, 1, stdout);
    PRINTF("\n");
  }
  { /* failure, no parentheses */
    const char *	pattern = "ci.*ut";
    cre2_regexp_t *	rex;
    const char *	text	= "ciao hello";
    cre2_string_t	input   = { .data = text, .length = strlen(text) };
    int			result;
    rex    = cre2_new(pattern, strlen(pattern), NULL);
    result = cre2_partial_match_re(rex, &input, NULL, 0);
    cre2_delete(rex);
    if (result)
      goto error;
  }
  { /* failure, one parenthetical subexpression */
    const char *	pattern = "(ciao) salut";
    cre2_regexp_t *	rex;
    const char *	text	= "ciao hello";
    cre2_string_t	input   = { .data = text, .length = strlen(text) };
    int			nmatch  = 1;
    cre2_string_t	match[nmatch];
    int			result;
    rex    = cre2_new(pattern, strlen(pattern), NULL);
    result = cre2_partial_match_re(rex, &input, match, nmatch);
    cre2_delete(rex);
    if (result)
      goto error;
    if (0 != strncmp(text, input.data, input.length))
      goto error;
  }
  { /* success, one parenthetical subexpression, no match entries */
    const char *	pattern = "(ciao) salut";
    cre2_regexp_t *	rex;
    const char *	text	= "ciao salut";
    cre2_string_t	input   = { .data = text, .length = strlen(text) };
    int			result;
    rex    = cre2_new(pattern, strlen(pattern), NULL);
    result = cre2_partial_match_re(rex, &input, NULL, 0);
    cre2_delete(rex);
    if (! result)
      goto error;
    if (0 != strncmp(text, input.data, input.length))
      goto error;
  }
  { /* failure, one parenthetical subexpression, two match entries */
    const char *	pattern = "(ciao) salut";
    cre2_regexp_t *	rex;
    const char *	text	= "ciao salut";
    cre2_string_t	input   = { .data = text, .length = strlen(text) };
    int			nmatch  = 2;
    cre2_string_t	match[nmatch];
    int			result;
    memset(match, '\0', nmatch * sizeof(cre2_string_t));
    rex    = cre2_new(pattern, strlen(pattern), NULL);
    result = cre2_partial_match_re(rex, &input, match, nmatch);
    cre2_delete(rex);
    if (0 != result)
      goto error;
  }
  { /* success, two parenthetical subexpressions, one match entry */
    const char *	pattern = "(ciao) (salut)";
    cre2_regexp_t *	rex;
    const char *	text	= "ciao salut";
    cre2_string_t	input   = { .data = text, .length = strlen(text) };
    int			nmatch  = 1;
    cre2_string_t	match[nmatch];
    int			result;
    rex    = cre2_new(pattern, strlen(pattern), NULL);
    result = cre2_partial_match_re(rex, &input, match, nmatch);
    cre2_delete(rex);
    if (! result)
      goto error;
    if (0 != strncmp("ciao", match[0].data, match[0].length))
      goto error;
    if (0 != strncmp(text, input.data, input.length))
      goto error;
    PRINTF("match 0: ");
    FWRITE(match[0].data, match[0].length, 1, stdout);
    PRINTF("\n");
  }

  exit(EXIT_SUCCESS);
 error:
  exit(EXIT_FAILURE);
}

/* end of file */
