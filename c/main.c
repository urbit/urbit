/* c/main.c
**
** This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <setjmp.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <gmp.h>
#include <stdint.h>

#include "all.h"

extern void *hill_boot(void);
extern void  hill_line(void *hill, const char *line);
extern void  hill_lose(void *hill);

int
main(int  argc,
     char **argv)
{
  char *history_name = c3_comd_init();
  void *hill;

  if ( !(hill = hill_boot()) ) {
    return 1;
  }
  while ( 1 ) {
    char *line = c3_comd_line(history_name);

    if ( !line ) {
      break;
    }
    else {
      hill_line(hill, line);
    }
  }
  hill_lose(hill);
  return 0;
}
