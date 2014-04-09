/*
  Part of: CRE2
  Contents: test for version functions
  Date: Mon Jan  2, 2012

  Abstract

	Test file for version functions.

  Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>

  See the COPYING file.
*/

#include <stdio.h>
#include <stdlib.h>
#include <cre2.h>

int
main (int argc, const char *const argv[])
{
  printf("version number string: %s\n", cre2_version_string());
  printf("libtool version number: %d:%d:%d\n",
	 cre2_version_interface_current(),
	 cre2_version_interface_revision(),
	 cre2_version_interface_age());
  exit(EXIT_SUCCESS);
}

/* end of file */
