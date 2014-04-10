/*
  Part of: CRE2
  Contents: test for easy matching
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
  const char *	pattern;
  const char *	text;

/* ------------------------------------------------------------------ */
/* single match */

  pattern = "ciao";
  text    = "ciao";
  {
    cre2_string_t	match;
    int			nmatch = 1;
    cre2_easy_match(pattern, strlen(pattern),
		    text,    strlen(text),
		    &match, nmatch);
    PRINTF("match: ");
    FWRITE(match.data, match.length, 1, stdout);
    PRINTF("\n");
    if (0 != strncmp("ciao", match.data, match.length))
      goto error;
  }

/* ------------------------------------------------------------------ */
/* wrong pattern */

  pattern = "ci(ao";
  text    = "ciao";
  {
    cre2_string_t	match;
    int			nmatch = 1;
    int			retval;
    retval = cre2_easy_match(pattern, strlen(pattern),
			     text,    strlen(text),
			     &match, nmatch);
    if (2 != retval)
      goto error;
  }

/* ------------------------------------------------------------------ */
/* two groups */

  pattern = "(ciao) (hello)";
  text    = "ciao hello";
  {
    int			nmatch = 3;
    cre2_string_t	match[nmatch];
    cre2_easy_match(pattern, strlen(pattern),
		    text,    strlen(text),
		    match, nmatch);
    PRINTF("full match: ");
    FWRITE(match[0].data, match[0].length, 1, stdout);
    PRINTF("\n");
    PRINTF("first group: ");
    FWRITE(match[1].data, match[1].length, 1, stdout);
    PRINTF("\n");
    PRINTF("second group: ");
    FWRITE(match[2].data, match[2].length, 1, stdout);
    PRINTF("\n");
    if (0 != strncmp("ciao hello", match[0].data, match[0].length))
      goto error;
    if (0 != strncmp("ciao", match[1].data, match[1].length))
      goto error;
    if (0 != strncmp("hello", match[2].data, match[2].length))
      goto error;
  }

/* ------------------------------------------------------------------ */

  exit(EXIT_SUCCESS);
 error:
  exit(EXIT_FAILURE);
}

/* end of file */
