/* fake/pump/err.c
**
** This file is in the public domain.
*/
#include "u4/all.h"
#include <stdio.h>

/* print_dump(): print (dump).
*/
static void
_print_dump(u4_dump dump)
{
  while ( !u4_n_zero(dump) ) {
    u4_atom line = u4_ch(dump);
    u4_sb   sb   = u4_a_bin(line, 3);
    u4_cl   *cl  = alloca(sb + 1);

    u4_a_bytes(line, (u4_xb *)cl, 0, sb);
    cl[sb] = 0;
    printf(" %s\n", cl);

    dump = u4_ct(dump);
  }
}

/* u4_err():
**
**   Print (nopt) with (caption), using (lane).
*/
void
u4_err(u4_lane     lane,
       const u4_cl *cl_caption,
       u4_nopt     noun)
{
  if ( u4_bull == noun ) {
    printf("%s: <bull>", cl_caption);
  }
  else {
    u4_prep prep = u4_pump_prep(lane, noun);
    u4_dump dump = u4_pump_dump(lane, u4_cod_in(75), prep);

    if ( cl_caption ) {
      printf("%s:\n", cl_caption);
    }
    _print_dump(dump);
  }
}

/* u4_burp():
**
**   Print (prep) with (caption), using (lane).
*/
void
u4_burp(u4_lane     lane,
        const u4_cl *cl_caption,
        u4_prep     prep)
{
  u4_dump dump = u4_pump_dump(lane, u4_cod_in(75), prep);

  if ( cl_caption ) {
    printf("%s:\n", cl_caption);
  }
  _print_dump(dump);
}
