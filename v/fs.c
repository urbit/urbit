/* v/fs.c
**
** This file is in the public domain.
*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <setjmp.h>
#include <gmp.h>
#include <stdint.h>
#include <uv.h>
#include <curses.h>
#include <termios.h>
#include <term.h>

#include "all.h"
#include "f/coal.h"
#include "v/vere.h"

/* u2_ve_file(): load internal file as atom from local or system.
*/
u2_weak
u2_ve_file(c3_c* ext_c, u2_noun tah)
{
  u2_noun pas;
  u2_weak dat;

  c3_assert(u2_System);
  pas = u2_cf_path(u2_System, ext_c, u2k(tah));
  dat = u2_cf_flat_load(c3__atom, pas);

  u2z(tah);
  return dat;
}

/* u2_ve_date(): date internal file.
*/
c3_d
u2_ve_date(c3_c* ext_c, u2_noun tah)
{
  u2_noun pas;
  u2_weak dat;

  pas = u2_cf_path(u2_System, ext_c, u2k(tah));
  dat = u2_cf_flat_date(pas);

  u2z(tah);
  return dat;
}

/* u2_ve_frep(): load [.~ %rep myp {now} hat].
**
**   File is either ~ or [nbytes mdate atom].
*/
u2_noun
u2_ve_frep(u2_noun myp, u2_noun hat)
{
  //  Total hack - switch to this form exclusively,
  //  then revise callers.
  {
    c3_c* dex_c = u2_cr_string(myp);
    u2_noun tah = u2_ckb_flop(hat);
    u2_weak fat = u2_ve_file(dex_c, u2_ct(tah));

    u2_cz(myp);
    if ( u2_none == fat ) {
      u2_cz(tah);
      free(dex_c);
      return u2_nul;
    } else {
      c3_d wen_d = u2_ve_date(dex_c, tah);
      c3_w wen_w[2];

      wen_w[0] = (wen_d & 0xffffffffULL);
      wen_w[1] = (wen_d >> 32ULL);

      free(dex_c);
      return u2nt(u2_cr_met(3, fat),
                  u2_ci_words(2, wen_w),
                  fat);
    }
  }
}

/* u2_ve_save(): save internal file as atom.
*/
u2_bean
u2_ve_save(c3_c* ext_c, u2_noun tah, u2_noun dat)
{
  return u2_cf_flat_save(c3__atom, u2_cf_path(u2_System, ext_c, tah), dat);
}

/* u2_ve_fold(): load a list of all files with extension `ext` in `tah`.
*/
u2_noun
u2_ve_fold(c3_c* ext_c, u2_noun tah)
{
  u2_noun pas = u2_cf_path(u2_System, 0, tah);
  u2_noun raw = u2_cf_list(pas);

  /* Filter for extension.
  */
  if ( !ext_c ) {
    return raw;
  } else {
    u2_noun lis = u2_nul;

    while ( u2_nul != raw ) {
      u2_noun i_raw = u2h(raw);
      u2_noun t_raw = u2t(raw);

      {
        c3_c* str_c = u2_cr_string(i_raw);
        c3_c* chr_c = strrchr(str_c, '.');

        if ( !strcmp(ext_c, chr_c+1) ) {
          *chr_c = 0;
          lis = u2nc(u2_ci_string(str_c), lis);
        }
      }
      raw = t_raw;
    }
    u2z(raw);
    return lis;
  }
}
