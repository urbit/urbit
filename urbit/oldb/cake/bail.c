/* cake/bail.c
**
** This file is in the public domain.
*/
#include <stdio.h>
#include "u4/all.h"

/* u4_bail_out():
**
**   Bail out with (code).
*/
u4_int
u4_bail_out(enum u4_bail_code code)
{
  if ( code == u4_bail_trip ) {
    // longjmp(U4_Bail.jmpbuf, code);
    printf("trip!\n");
    abort();
  }
  else if ( code == u4_bail_stub ) {
    // longjmp(U4_Bail.jmpbuf, code);
    printf("stub!\n");
    abort();
  }
  else if ( code == u4_bail_tank ) {
    longjmp(U4_Bail.jmpbuf, code);
  }
  else if ( code == u4_bail_exit ) {
    longjmp(U4_Bail.jmpbuf, code);
  }
  else {
    printf("huh %x?\n", code);
    abort();
    longjmp(U4_Bail.jmpbuf, code);
  }
  return 0;
}
